{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_BerkeleyDB;

interface

uses
  SysUtils,
  SyncObjs,
  db_h;

const
  BDB_MIN_PAGE_SIZE = $200; // 512 b
  BDB_MAX_PAGE_SIZE = $10000; // 64 Kb

  BDB_MEM_CACHE_SIZE = $100000; // 1 Mb

type
  TBerkeleyDB = class(TObject)
  private
    FDB: PDB;
    FFileName: string;
    FDBEnabled: Boolean;
    FLastErrorStr: string;
    FCS: TCriticalSection;
  public
    constructor Create;

    destructor Destroy; override;

    function Open(
      const AFileName: string;
      ADBType: DBTYPE = DB_BTREE;
      AFlags: Cardinal = DB_CREATE_;
      APageSize: Cardinal = BDB_MAX_PAGE_SIZE;
      AMemCacheSize: Cardinal = BDB_MEM_CACHE_SIZE
    ): Boolean;

    procedure Close;

    function Write(
      AKey: Pointer;
      AKeySize: Cardinal;
      AData: Pointer;
      ADataSize: Cardinal;
      AFlags: Cardinal = 0
    ): Boolean;

    function Read(
      AKey: Pointer;
      AKeySize: Cardinal;
      out AData: Pointer;
      out ADataSize: Cardinal;
      AFlags: Cardinal = 0
    ): Boolean;

    function Exists(
      AKey: Pointer;
      AKeySize: Cardinal;
      AFlags: Cardinal = 0
    ): Boolean;

    function Del(
      AKey: Pointer;
      AKeySize: Cardinal;
      AFlags: Cardinal = 0
    ): Boolean;

    property FileName: string read FFileName write FFileName;
  end;

implementation

var
  BDBLibInitError: Boolean = False;
  BDBLibInitErrorStr: string = '';
  LibInitCS: TCriticalSection = nil;

function BDBInitLib(out AErrorStr: string): Boolean;
begin
  LibInitCS.Acquire;
  try
    Result := False;
    if not BDBLibInitError then begin
      try
        InitBerkeleyDB;
        Result := True;
      except
        on E: Exception do begin
          BDBLibInitError := True;
          BDBLibInitErrorStr := E.ClassName + ':' + E.Message;
        end;
      end;
    end;
    AErrorStr := BDBLibInitErrorStr;
  finally
    LibInitCS.Release;
  end;
end;

{ TBerkeleyDB }

constructor TBerkeleyDB.Create;
begin
  inherited Create;
  FCS := TCriticalSection.Create;
  FFileName := '';
  FDB := nil;
  if not BDBInitLib(FLastErrorStr) then begin
    raise Exception.Create(FLastErrorStr);
  end;
end;

destructor TBerkeleyDB.Destroy;
begin
  Close;
  FCS.Free;
  inherited Destroy;
end;

function TBerkeleyDB.Open(
  const AFileName: string;
  ADBType: DBTYPE = DB_BTREE;
  AFlags: Cardinal = DB_CREATE_;
  APageSize: Cardinal = BDB_MAX_PAGE_SIZE;
  AMemCacheSize: Cardinal = BDB_MEM_CACHE_SIZE
): Boolean;
begin
  FCS.Acquire;
  try
    if (FDB <> nil) then begin
      if (FFileName <> '') and (AFileName = FFileName) then begin
        Result := FDBEnabled;
      end else begin
        Result := False;
      end;
    end else begin
      try
        CheckBDB(db_create(FDB, nil, 0));
        CheckBDB(FDB.set_alloc(FDB, @GetMemory, @ReallocMemory, @FreeMemory));
        if not FileExists(FFileName) then begin
          CheckBDB(FDB.set_pagesize(FDB, APageSize));
        end;
        CheckBDB(FDB.set_cachesize(FDB, 0, AMemCacheSize, 0));
        CheckBDB(FDB.open(FDB, nil, PAnsiChar(AFileName), '', ADBType, AFlags, 0));
        FDBEnabled := True;
      except
        on E: Exception do begin
          FDBEnabled := False;
          FLastErrorStr := E.Message;
        end;
      end;
      Result := FDBEnabled;
    end;
  finally
    FCS.Release;
  end;
end;

procedure TBerkeleyDB.Close;
begin
  FCS.Acquire;
  try
    if Assigned(FDB) then begin
      CheckBDBandNil(FDB.close(FDB, 0), FDB);
    end;
  finally
    FCS.Release;
  end;
end;

function TBerkeleyDB.Read(
  AKey: Pointer;
  AKeySize: Cardinal;
  out AData: Pointer;
  out ADataSize: Cardinal;
  AFlags: Cardinal = 0
): Boolean;
var
  dbtKey, dbtData: DBT;
begin
  FCS.Acquire;
  try
    Result := False;
    if FDBEnabled then begin
      FillChar(dbtKey, Sizeof(DBT), 0);
      FillChar(dbtData, Sizeof(DBT), 0);
      dbtKey.data := AKey;
      dbtKey.size := AKeySize;
      Result := CheckAndFoundBDB(FDB.get(FDB, nil, @dbtKey, @dbtData, AFlags));
      if Result then begin
        AData := dbtData.data;
        ADataSize := dbtData.size;
      end;
    end;
  finally
    FCS.Release;
  end;
end;

function TBerkeleyDB.Write(
  AKey: Pointer;
  AKeySize: Cardinal;
  AData: Pointer;
  ADataSize: Cardinal;
  AFlags: Cardinal = 0
): Boolean;
var
  dbtKey, dbtData: DBT;
begin
  FCS.Acquire;
  try
    Result := False;
    if FDBEnabled then begin
      FillChar(dbtKey, Sizeof(DBT), 0);
      FillChar(dbtData, Sizeof(DBT), 0);
      dbtKey.data := AKey;
      dbtKey.size := AKeySize;
      dbtData.data := AData;
      dbtData.size := ADataSize;
      Result := CheckAndNotExistsBDB(FDB.put(FDB, nil, @dbtKey, @dbtData, AFlags));
    end;
  finally
    FCS.Release;
  end;
end;

function TBerkeleyDB.Exists(
  AKey: Pointer;
  AKeySize: Cardinal;
  AFlags: Cardinal = 0
): Boolean;
var
  dbtKey: DBT;
begin
  FCS.Acquire;
  try
    Result := False;
    if FDBEnabled then begin
      FillChar(dbtKey, Sizeof(DBT), 0);

      dbtKey.data := AKey;
      dbtKey.size := AKeySize;

      Result := CheckAndFoundBDB(FDB.exists(FDB, nil, @dbtKey, AFlags));
    end;
  finally
    FCS.Release;
  end;
end;

function TBerkeleyDB.Del(
  AKey: Pointer;
  AKeySize: Cardinal;
  AFlags: Cardinal = 0
): Boolean;
var
  dbtKey: DBT;
begin
  FCS.Acquire;
  try
    Result := False;
    if FDBEnabled then begin
      FillChar(dbtKey, Sizeof(DBT), 0);

      dbtKey.data := AKey;
      dbtKey.size := AKeySize;

      Result := CheckAndFoundBDB(FDB.del(FDB, nil, @dbtKey, AFlags));
    end;
  finally
    FCS.Release;
  end;
end;

initialization
  LibInitCS := TCriticalSection.Create;

finalization
  LibInitCS.Free;

end.

