{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_MapSvcScanStorage;

interface

uses
  SysUtils,
  Classes,
  ALString,
  SQLite3Handler,
  i_Listener,
  i_MapSvcScanConfig,
  i_MapSvcScanStorage,
  u_BaseInterfacedObject;

type
  TMapSvcScanStorage = class(TBaseInterfacedObject, IMapSvcScanStorage)
  private
    FMapSvcScanConfig: IMapSvcScanConfig;
    FConfigChangeListener: IListener;
    FSync: IReadWriteSync;
    FDbHandler: TSQLite3DbHandler;
    FInitialized: Boolean;
    FServices: TStringList;
    FFormatSettings: TAlFormatSettings;
  private
    procedure CallbackReadSingleInt(
      const AHandler: PSQLite3DbHandler;
      const ACallbackPtr: Pointer;
      const AStmtData: PSQLite3StmtData
    );
    function GetServiceId(const AServiceName: String): Integer;
    // dont use unix seconds because of very big delta and Int64
    function DateTimeToDBSeconds(const AValue: TDateTime): Integer;
    function DBSecondsToDateTime(const AValue: Integer): TDateTime;
  private
    procedure OnConfigChanged;
  private
    { IMapSvcScanStorage }
    function Available: Boolean;
    function ItemExists(
      const AServiceName: String;
      const AIdentifier: WideString;
      const AFetchedDate: PDateTime
    ): Boolean;
    function AddItem(
      const AServiceName: String;
      const AIdentifier: WideString;
      const AFetchedDate: TDateTime
    ): Boolean;
    function GetScanDate(
      const AVersionId: WideString
    ): string;
    function AddImageDate(
      const AVersionId: WideString;
      const ADate: string;
      const AX: Double;
      const AY: Double;
      const AZoom: Byte
    ): Boolean;

  public
    constructor Create(
      const AMapSvcScanConfig: IMapSvcScanConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  ALSqlite3Wrapper,
  u_ListenerByEvent,
  u_Synchronizer;

const
  c_DBSecondsDateDelta = 40000;

{ TMapSvcScanStorage }

function TMapSvcScanStorage.AddItem(
  const AServiceName: String;
  const AIdentifier: WideString;
  const AFetchedDate: TDateTime
): Boolean;
var
  VId: Integer;
begin
  Result := Available;
  if (not Result) then
    Exit;

  // get service id
  VId := GetServiceId(AServiceName);
  Result := (VId<>0);
  if (not Result) then
    Exit;

  // insert one row
  try
    FDbHandler.ExecSQLWithTEXTW(
      'INSERT OR IGNORE INTO svcitem (id,itemname,itemdate) VALUES ('+ALIntToStr(VId) + ',?,' + ALIntToStr(DateTimeToDBSeconds(AFetchedDate)) + ')',
      TRUE,
      PWideChar(AIdentifier),
      Length(AIdentifier)
    );
    Result := TRUE;
  except
    Result := FALSE;
  end;
end;

function TMapSvcScanStorage.Available: Boolean;
begin
  Result := FInitialized and FDbHandler.Opened;
end;

procedure TMapSvcScanStorage.CallbackReadSingleInt(
  const AHandler: PSQLite3DbHandler; const ACallbackPtr: Pointer;
  const AStmtData: PSQLite3StmtData);
begin
  // читаем одно поле типа INT
  PInteger(ACallbackPtr)^ := AStmtData^.ColumnInt(0);
  AStmtData^.Cancelled := TRUE;
end;

constructor TMapSvcScanStorage.Create(
  const AMapSvcScanConfig: IMapSvcScanConfig
);
begin
  inherited Create;
  FMapSvcScanConfig := AMapSvcScanConfig;
  FSync := GSync.SyncStd.Make(Self.ClassName);

  FServices := TStringList.Create;
  FServices.Sorted := TRUE;
  FServices.Duplicates := dupIgnore;

  FFormatSettings.DecimalSeparator := '.';

  FInitialized := FALSE;

  FConfigChangeListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChanged);
  FMapSvcScanConfig.ChangeNotifier.Add(FConfigChangeListener);

  OnConfigChanged;
end;

function TMapSvcScanStorage.DateTimeToDBSeconds(const AValue: TDateTime): Integer;
begin
  Result := Round((AValue - c_DBSecondsDateDelta) * SecsPerDay);
end;

function TMapSvcScanStorage.DBSecondsToDateTime(const AValue: Integer): TDateTime;
begin
  Result := AValue / SecsPerDay + c_DBSecondsDateDelta;
end;

destructor TMapSvcScanStorage.Destroy;
begin
  if Assigned(FMapSvcScanConfig) and Assigned(FConfigChangeListener) then begin
    FMapSvcScanConfig.ChangeNotifier.Remove(FConfigChangeListener);
    FConfigChangeListener := nil;
  end;

  FInitialized := FALSE;
  if Assigned(FSync) then begin
    FSync.BeginWrite;
    try
      FDbHandler.Close;
      FreeAndNil(FServices);
    finally
      FSync.EndWrite;
    end;
  end;
  inherited;
end;

function TMapSvcScanStorage.GetServiceId(const AServiceName: String): Integer;
var
  VServiceName: WideString;

  function _SelectId: Integer;
  begin
    Result := 0;
    FDbHandler.OpenSQLWithTEXTW(
      'SELECT id FROM svcinfo WHERE svcname=?',
      CallbackReadSingleInt,
      @Result,
      FALSE,
      TRUE,
      PWideChar(VServiceName),
      Length(VServiceName)
    );
  end;

begin
  FSync.BeginRead;
  try
    // get from list
    Result := FServices.IndexOf(AServiceName);
    if (Result>=0) then begin
      // found
      Result := Integer(Pointer(FServices.Objects[Result]));
      Exit;
    end;
  finally
    FSync.EndRead;
  end;

  // not found
  FSync.BeginWrite;
  try
    VServiceName := AServiceName;

    // select from database
    Result := _SelectId;

    // check row not found
    if (0=Result) then begin
      // insert row
      FDbHandler.ExecSQLWithTEXTW(
        'INSERT OR IGNORE INTO svcinfo(svcname) VALUES(?)',
        TRUE,
        PWideChar(VServiceName),
        Length(VServiceName)
      );
      // again
      Result := _SelectId;
    end;

    if (0<>Result) then begin
      FServices.AddObject(AServiceName, Pointer(Result));
    end;
  finally
    FSync.EndWrite;
  end;
end;

function TMapSvcScanStorage.GetScanDate(
  const AVersionId: WideString
): string;
var
  VDBSeconds: Integer;
begin
  Result := '';
  if  (AVersionId <> '') or (not Available) then begin
    try
      VDBSeconds := 0;
      FDbHandler.OpenSQLWithTEXTW(
        'SELECT date FROM scandate WHERE imageid=?',
        CallbackReadSingleInt,
        @VDBSeconds,
        True,
        TRUE,
        PWideChar(AVersionId),
        Length(AVersionId)
      );
      if (VDBSeconds <> 0) then begin
        Result := DateTimeToStr(DBSecondsToDateTime(VDBSeconds));
      end;
    except
    end;
  end;
end;

function TMapSvcScanStorage.AddImageDate(
      const AVersionId: WideString;
      const ADate: string;
      const AX: Double;
      const AY: Double;
      const AZoom: Byte
    ): Boolean;
var
  VSQLText: AnsiString;
  VDBSeconds: Integer;
  VdateTime : TDateTime;
begin
  Result := FALSE;
  if GetScanDate(AVersionId) <> '' then Exit;

  VdateTime := EncodeDate(StrToInt(Copy(ADate,1,4)), StrToInt(Copy(ADate,6,2)), StrToInt((Copy(ADate,9,2))));
  VDBSeconds := DateTimeToDBSeconds(VdateTime);

  // insert one row
  try
    VSQLText :=
      'INSERT OR IGNORE INTO scandate (imageid, date, x, y, z) VALUES (?,' +
      ALIntToStr(VDBSeconds) + ',' +
      AlFloatToStr(AX, FFormatSettings) + ',' +
      AlFloatToStr(AY, FFormatSettings) + ',' +
      ALIntToStr(AZoom) + ')';

    FDbHandler.ExecSQLWithTEXTW(
      VSQLText,
      TRUE,
      PWideChar(AVersionId),
      Length(AVersionId)
    );
    Result := TRUE;
  except
    Result := FALSE;
  end;
end;

function TMapSvcScanStorage.ItemExists(
  const AServiceName: String;
  const AIdentifier: WideString;
  const AFetchedDate: PDateTime
): Boolean;
var
  VId: Integer;
  VDBSeconds: Integer;
begin
  Result := Available;
  if (not Result) then
    Exit;

  // get service id
  VId := GetServiceId(AServiceName);
  Result := (VId<>0);
  if (not Result) then
    Exit;

  try
    VDBSeconds := 0;
    FDbHandler.OpenSQLWithTEXTW(
      'SELECT itemdate FROM svcitem WHERE id='+ALIntToStr(VId) + ' AND itemname=?',
      CallbackReadSingleInt,
      @VDBSeconds,
      TRUE,
      TRUE,
      PWideChar(AIdentifier),
      Length(AIdentifier)
    );
    Result := (VDBSeconds<>0);
    if Result then begin
      if AFetchedDate<>nil then begin
        AFetchedDate^ := DBSecondsToDateTime(VDBSeconds);
      end;
    end;
  except
    Result := FALSE;
  end;
end;

procedure TMapSvcScanStorage.OnConfigChanged;
var
  VUseStorage: Boolean;
  VPath: String;
begin
  VUseStorage := FMapSvcScanConfig.UseStorage;
  if (Available=VUseStorage) then
    Exit;

  FSync.BeginWrite;
  try
    if (not VUseStorage) then begin
      FDbHandler.Close;
      Exit;
    end;

    if (not FInitialized) then begin
      FInitialized := FDbHandler.Init;
      if not FInitialized then
        Exit;
    end;

    try
      VPath := FMapSvcScanConfig.Path.FullPath;
      ForceDirectories(VPath);
      VPath := IncludeTrailingPathDelimiter(VPath) + 'FoundItems.sqlitedb';
      if FileExists(VPath) then begin
        // open existing
        FDbHandler.Open(VPath, SQLITE_OPEN_READWRITE);
      end else begin
        // create new database
        FDbHandler.Open(VPath, (SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE));
        // generate structure
        FDbHandler.ExecSQL('create table IF NOT EXISTS svcinfo (id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, svcname NVARCHAR NOT NULL)');
        FDbHandler.ExecSQL('create unique index IF NOT EXISTS svcinfo_uniq on svcinfo (svcname)');
        FDbHandler.ExecSQL('create table IF NOT EXISTS svcitem (id INTEGER NOT NULL CONSTRAINT svcinfo_fk REFERENCES svcinfo (id) ON DELETE CASCADE, itemname NVARCHAR NOT NULL, itemdate INT NOT NULL, constraint PK_SVCITEM primary key (id, itemname))');
        FDbHandler.ExecSQL('create table IF NOT EXISTS scandate (imageid NVARCHAR NOT NULL, date int NOT NULL, x INT NOT NULL, y INT NOT NULL, z INT NOT NULL)');
      end;
      // apply config
      FDbHandler.ExecSQL('PRAGMA main.journal_mode=PERSIST'); // DELETE by default // WAL // PERSIST
      FDbHandler.ExecSQL('PRAGMA synchronous=NORMAL'); // FULL by default
      FDbHandler.ExecSQL('PRAGMA foreign_keys=ON'); // OFF by default

      if GetScanDate('*') <> '' then begin
        FDbHandler.Open(VPath, (SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE));
        FDbHandler.ExecSQL('create table IF NOT EXISTS scandate (imageid NVARCHAR NOT NULL, date int NOT NULL, x INT NOT NULL, y INT NOT NULL, z INT NOT NULL)');
      end;


    except
      FDbHandler.Close;
    end;
  finally
    FSync.EndWrite;
  end;
end;

end.
