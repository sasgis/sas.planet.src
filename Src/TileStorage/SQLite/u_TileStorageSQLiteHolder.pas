unit u_TileStorageSQLiteHolder;

interface

uses
  Windows,
  Types,
  SysUtils,
  Classes,
  SQLite3Handler,
  i_StringListStatic,
  i_ContentTypeInfo,
  i_ContentTypeManager,
  i_MapVersionInfo,
  i_MapVersionFactory,
  i_TileStorageSQLiteHolder,
  i_TileStorageSQLiteErrorLogger,
  u_BaseInterfacedObject;

type
  TTileStorageSQLiteHolder = class(TBaseInterfacedObject, ITileStorageSQLiteHolder)
  private
    FStoragePath: string;
    FContentTypeManager: IContentTypeManager;
    FMainContentType: IContentTypeInfoBasic;
    FMapVersionFactory: IMapVersionFactory;
    FSQLMakeSession: IStringListStatic;
    FSQLForNewTable: IStringListStatic;
    FSQLEstablished: IStringListStatic;
    FLogging: Boolean;
    FLogger: IErrorLoggerToFile;
  private
    procedure InternalLoggerGetFileNameEvent(
      var AFullLogFilename: String;
      var AForbidden: Boolean
    );
    procedure InternalLoadParams;
    procedure InternalExecSQL(
      const ASQL: IStringListStatic;
      const AExecProc: TSetSQLiteExecProc
    );
  protected
    { ITileStorageSQLiteHolder }
    procedure LogError(
      const ACmd: AnsiChar;
      const AMsg: String;
      const ARaiseError: Boolean
    );
    // sql
    procedure ExecMakeSession(const AExecProc: TSetSQLiteExecProc);
    procedure ExecForNewTable(const AExecProc: TSetSQLiteExecProc); virtual;
    procedure ExecEstablished(const AExecProc: TSetSQLiteExecProc);
    // tiles
    function GetContentTypeToDB(const AContentType: IContentTypeInfoBasic): AnsiString;
    function GetContentTypeInfo(const AContentTypeFromDB: AnsiString): IContentTypeInfoBasic;
    function GetVersionInfo(const AVersionStr: String): IMapVersionInfo;
  public
    constructor Create(
      const AStoragePath: string;
      const AContentTypeManager: IContentTypeManager;
      const AMainContentType: IContentTypeInfoBasic;
      const AMapVersionFactory: IMapVersionFactory;
      const ALogging: Boolean
    ); virtual;
    destructor Destroy; override;
  end;

  ETileStorageSQLiteError = class(ESQLite3Exception);

implementation

uses
  ALString,
  c_TileStorageSQLite,
  u_StringListStatic,
  u_TileStorageSQLiteHandler,
  u_TileStorageSQLiteErrorLogger;

{ TTileStorageSQLiteHolder }

constructor TTileStorageSQLiteHolder.Create(
  const AStoragePath: string;
  const AContentTypeManager: IContentTypeManager;
  const AMainContentType: IContentTypeInfoBasic;
  const AMapVersionFactory: IMapVersionFactory;
  const ALogging: Boolean
);
begin
  inherited Create;

  FStoragePath := AStoragePath;
  FContentTypeManager := AContentTypeManager;
  FMainContentType := AMainContentType;
  FMapVersionFactory := AMapVersionFactory;
  FLogging := ALogging;

  if FLogging then begin
    FLogger := TErrorLoggerToFile.Create('', InternalLoggerGetFileNameEvent);
  end;

  FSQLMakeSession := nil;
  FSQLForNewTable := nil;
  FSQLEstablished := nil;

  InternalLoadParams;
end;

destructor TTileStorageSQLiteHolder.Destroy;
begin
  FMapVersionFactory := nil;
  FMainContentType := nil;
  FContentTypeManager := nil;

  FSQLMakeSession := nil;
  FSQLForNewTable := nil;
  FSQLEstablished := nil;

  FLogging := False;
  FLogger := nil;

  inherited;
end;

procedure TTileStorageSQLiteHolder.ExecEstablished(const AExecProc: TSetSQLiteExecProc);
begin
  InternalExecSQL(FSQLEstablished, AExecProc);
end;

procedure TTileStorageSQLiteHolder.ExecForNewTable(const AExecProc: TSetSQLiteExecProc);
begin
  InternalExecSQL(FSQLForNewTable, AExecProc);
end;

procedure TTileStorageSQLiteHolder.ExecMakeSession(const AExecProc: TSetSQLiteExecProc);
begin
  InternalExecSQL(FSQLMakeSession, AExecProc);
end;

function TTileStorageSQLiteHolder.GetContentTypeInfo(
  const AContentTypeFromDB: AnsiString
): IContentTypeInfoBasic;
begin
  if Length(AContentTypeFromDB) = 0 then begin
    // use main
    Result := FMainContentType;
  end else begin
    // get by string (returns NIL for unknown types)
    Result := FContentTypeManager.GetInfo(AContentTypeFromDB);
  end;
end;

function TTileStorageSQLiteHolder.GetContentTypeToDB(
  const AContentType: IContentTypeInfoBasic
): AnsiString;
begin
  if (AContentType = nil) or ALSameText(AContentType.GetContentType, FMainContentType.GetContentType) then begin
    // no contenttype or main contenttype
    Result := 'NULL';
  end else begin
    Result := AContentType.GetContentType;
  end;
end;

function TTileStorageSQLiteHolder.GetVersionInfo(const AVersionStr: String): IMapVersionInfo;
begin
  Result := FMapVersionFactory.CreateByStoreString(AVersionStr);
end;

procedure TTileStorageSQLiteHolder.InternalExecSQL(
  const ASQL: IStringListStatic;
  const AExecProc: TSetSQLiteExecProc
);
var
  I: Integer;
begin
  if ASQL <> nil then begin
    for I := 0 to ASQL.Count - 1 do begin
      AExecProc(AnsiString(ASQL.Items[I]));
    end;
  end;
end;

procedure TTileStorageSQLiteHolder.InternalLoadParams;
var
  VFileName: string;
begin
  // MakeSession
  VFileName := FStoragePath + 's.sql';
  if FileExists(VFileName) then begin
    // load from file
    FSQLMakeSession := TStringListStatic.CreateFromFile(VFileName);
  end else begin
    // no file - use default values
    FSQLMakeSession := TStringListStatic.CreateByPlainText(
      'PRAGMA cache_size=60000' + #13#10 +
      'PRAGMA main.journal_mode=WAL' + #13#10 +
      'PRAGMA synchronous=NORMAL'
    );
  end;

  // ForNewTable
  VFileName := FStoragePath + 't.sql';
  if FileExists(VFileName) then begin
    // load from file
    FSQLForNewTable := TStringListStatic.CreateFromFile(VFileName);
  end else begin
    // no file - use default values
    VFileName :=
      'CREATE TABLE IF NOT EXISTS t ('+
      'x INTEGER NOT NULL,'+
      'y INTEGER NOT NULL,'+
      'v INTEGER DEFAULT 0 NOT NULL,'+ // version
      'c TEXT,'+                       // content_type
      's INTEGER DEFAULT 0 NOT NULL,'+ // size
      'd INTEGER NOT NULL,'+           // date as unix seconds DEFAULT (strftime(''%s'',''now'')))
      'b BLOB,'+                       // body
      'constraint PK_TB primary key (x,y,v))' + #13#10 +
      'CREATE INDEX IF NOT EXISTS t_v_idx on t (v)';
    FSQLForNewTable := TStringListStatic.CreateByPlainText(VFileName);
  end;

  // Established
  VFileName := FStoragePath + 'e.sql';
  if FileExists(VFileName) then begin
    // load from file
    FSQLEstablished := TStringListStatic.CreateFromFile(VFileName);
  end else begin
    // no file - no default values
    FSQLEstablished := nil;
  end;
end;

procedure TTileStorageSQLiteHolder.InternalLoggerGetFileNameEvent(
  var AFullLogFilename: String;
  var AForbidden: Boolean
);
begin
  AFullLogFilename := FStoragePath + 'sqlite.log';
end;

procedure TTileStorageSQLiteHolder.LogError(
  const ACmd: AnsiChar;
  const AMsg: String;
  const ARaiseError: Boolean
);
var
  VLogText: String;
begin
  if FLogging then begin
    // make text to save to log
    case ACmd of
      c_Log_Init:
        VLogText := 'init';
      c_Log_Delete:
        VLogText := 'delete';
      c_Log_Select:
        VLogText := 'select';
      c_Log_Replace:
        VLogText := 'replace';
      c_Log_GetVer:
        VLogText := 'getver';
      c_Log_GetMap:
        VLogText := 'fillmap';
    else
      VLogText := 'custom';
    end;

    VLogText :=
      FormatDateTime('yyyy-mm-dd hh:nn:ss.zzzz', Now) + #9 +
      '(' + VLogText + ')' + #9 +
      AMsg + #13#10;

    // save
    FLogger.LogString(VLogText);
  end;

  if ARaiseError then begin
    raise ETileStorageSQLiteError.Create(AMsg);
  end;
end;

end.
