{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
  Classes,
  SysUtils,
  SyncObjs,
  libdb51;

const
  BDB_MIN_PAGE_SIZE : Cardinal  = $200;  //512 b
  BDB_MAX_PAGE_SIZE : Cardinal  = $10000;  //64 Kb

  BDB_MIN_CACHE_SIZE : Cardinal = $5000;  //20k
  BDB_MAX_CACHE_SIZE : Cardinal = $FFFFFFFF;  //4G for 32-bit OS

  BDB_DEF_CACHE_SIZE = $40000; //256k
  BDB_DEF_PAGE_SIZE  = 0;      //auto-selected based on the underlying
                               //filesystem I/O block size (512b - 16k)

type
  TBDBOnEvent = procedure(Sender: TObject) of object;

  TBerkeleyDB = class(TObject)
  private
    FDB: PDB;
    FENV: PDB_ENV;
    FTXN: PDB_TXN;
    FTXNCommitCount: Integer;
    FAppData: Pointer;
    FFileName: string;
    FDBEnabled: Boolean;
    FSyncAllow: Boolean;
    FCS: TCriticalSection;
    FOnCreate: TBDBOnEvent;
    FOnOpen: TBDBOnEvent;
    FOnClose: TBDBOnEvent;
    FOnCheckPoint: TBDBOnEvent;

    function GetTransaction: PDB_TXN;
    procedure CommitTransaction;
  public
    constructor Create;

    destructor Destroy; override;

    function Open(
      AENV: PDB_ENV;
      const AFileName: string;
      APageSize: Cardinal = BDB_DEF_PAGE_SIZE;
      AMemCacheSize: Cardinal = BDB_DEF_CACHE_SIZE;
      ADBType: DBTYPE = DB_BTREE
    ): Boolean;

    procedure Close;

    function Write(
      AKey: Pointer;
      AKeySize: Cardinal;
      AData: Pointer;
      ADataSize: Cardinal
    ): Boolean;

    function Read(
      AKey: Pointer;
      AKeySize: Cardinal;
      out AData: Pointer;
      out ADataSize: Cardinal
    ): Boolean;

    function Exists(
      AKey: Pointer;
      AKeySize: Cardinal
    ): Boolean;

    function Del(
      AKey: Pointer;
      AKeySize: Cardinal
    ): Boolean;

    function Sync(): Boolean;

    function GetKeyExistsList(
      const AKeySize: Cardinal;
      out AKeyList: TList
    ): Boolean;

    property FileName: string read FFileName write FFileName;
    property AppData: Pointer read FAppData write FAppData;

    property OnCreate: TBDBOnEvent read FOnCreate write FOnCreate;
    property OnOpen: TBDBOnEvent read FOnOpen write FOnOpen;
    property OnClose: TBDBOnEvent read FOnClose write FOnClose;
    property OnCheckPoint: TBDBOnEvent read FOnCheckPoint write FOnCheckPoint;
  end;

implementation

uses
  u_BerkeleyDBEnv;

const
  CBerkeleyDBErrPfx = 'BerkeleyDB';
  cMaxTxnCommitCount = 32;

{ TBerkeleyDB }

constructor TBerkeleyDB.Create;
begin
  inherited Create;
  FCS := TCriticalSection.Create;
  FFileName := '';
  FDB := nil;
  FENV := nil;
  FTXN := nil;
  FTXNCommitCount := 0;
  FAppData := nil;
  FOnCreate := nil;
  FOnOpen := nil;
  FOnClose := nil;
  FOnCheckPoint := nil;
  FDBEnabled := False;
  FSyncAllow := False;
end;

destructor TBerkeleyDB.Destroy;
begin
  Close;
  FCS.Free;
  inherited Destroy;
end;

function TBerkeleyDB.Open(
  AENV: PDB_ENV;
  const AFileName: string;
  APageSize: Cardinal = BDB_DEF_PAGE_SIZE;
  AMemCacheSize: Cardinal = BDB_DEF_CACHE_SIZE;
  ADBType: DBTYPE = DB_BTREE
): Boolean;

  function TryOpen(AFileExists: Boolean): Boolean;
  var
    VEnvHome: string;
    VEnvHomePtr: PAnsiChar;
    VRelativeFileName: string;
  begin
    if FENV = nil then begin
      BDBRaiseException(CBerkeleyDBErrPfx + ': Environment not assigned.');
    end;

    VEnvHomePtr := '';
    CheckBDB((FENV.get_home(FENV, @VEnvHomePtr)));
    VEnvHome := StringReplace(
      VEnvHomePtr,
      IncludeTrailingPathDelimiter(CEnvSubDir),
      '',
      [rfIgnoreCase]
    );
    VRelativeFileName := StringReplace(FFileName, VEnvHome, '', [rfIgnoreCase]);

    CheckBDB(db_create(FDB, FENV, 0));
    try
      FDB.set_errpfx(FDB, CBerkeleyDBErrPfx);

      if not AFileExists then begin
        CheckBDB(FDB.set_pagesize(FDB, APageSize));
      end;

      CheckBDB(
        FDB.open(
          FDB,
          nil,
          Pointer(AnsiToUtf8(VRelativeFileName)),
          '',
          ADBType,
          DB_CREATE_ or
          DB_AUTO_COMMIT or
          DB_THREAD,
          0
        )
      );

      Result := True;
    except
      FDB.close(FDB, 0);
      FDB := nil;
      raise;
    end;             
  end;

var
  VOnCreateAllow: Boolean;
  VOnOpenAllow: Boolean;
  VFileExists: Boolean;
begin
  Result := False;
  VOnCreateAllow := False;
  VOnOpenAllow := False;
  FCS.Acquire;
  try
    if (FDB <> nil) then begin
      if (FFileName <> '') and (AFileName = FFileName) then begin
        Result := FDBEnabled;
      end;
    end else begin
      FDB := nil;
      FENV := AENV;
      FFileName := AFileName;
      FDBEnabled := False;
      VFileExists := FileExists(FFileName);
      Result := TryOpen(VFileExists);
      FDBEnabled := Result;
      VOnCreateAllow := not VFileExists;
      VOnOpenAllow := Result;
    end;
  finally
    FCS.Release;
  end;
  if VOnCreateAllow and (Addr(FOnCreate) <> nil) then begin
    FOnCreate(Self);
  end;
  if VOnOpenAllow and (Addr(FOnOpen) <> nil) then begin
    FOnOpen(Self);
  end;
end;

procedure TBerkeleyDB.Close;
var
  VOnCloseAllow: Boolean;
begin
  VOnCloseAllow := False;
  FCS.Acquire;
  try
    if Assigned(FDB) then begin
      VOnCloseAllow := True;
      CommitTransaction;
      CheckBDBandNil(FDB.close(FDB, 0), FDB);
    end;
  finally
    FCS.Release;
  end;
  if VOnCloseAllow and (Addr(FOnClose) <> nil) then begin
    FOnClose(Self);
  end;
end;

function TBerkeleyDB.GetTransaction: PDB_TXN;
begin
  if FTXNCommitCount > cMaxTxnCommitCount then begin
    CommitTransaction;
  end;
  if FTXN = nil then begin
    (*
       Отключено до победы над эксепшеном при построении карты заполнения или
       конвертировании кэша

       Exception class EBerkeleyDBExeption with message
       'BerkeleyDB: Lock table is out of available object entries'.

       CheckBDB(FENV.txn_begin(FENV, nil, @FTXN, 0));
    *)
  end;
  Inc(FTXNCommitCount);
  Result := FTXN;
end;

procedure TBerkeleyDB.CommitTransaction;
begin
  if FTXN <> nil then begin
    CheckBDB(FTXN.commit(FTXN, 0));
    FTXNCommitCount := 0;
    FTXN := nil;
  end;
end;

function TBerkeleyDB.Read(
  AKey: Pointer;
  AKeySize: Cardinal;
  out AData: Pointer;
  out ADataSize: Cardinal
): Boolean;
var
  dbtKey, dbtData: DBT;
  txn: PDB_TXN;
begin
  FCS.Acquire;
  try
    Result := False;
    if FDBEnabled then begin
      FillChar(dbtKey, Sizeof(DBT), 0);
      FillChar(dbtData, Sizeof(DBT), 0);
      dbtKey.data := AKey;
      dbtKey.size := AKeySize;
      dbtData.flags := DB_DBT_MALLOC;
      txn := GetTransaction;
      Result := CheckAndFoundBDB(FDB.get(FDB, txn, @dbtKey, @dbtData, 0));
      if Result and (dbtData.data <> nil) and (dbtData.size > 0) then begin
        AData := dbtData.data;
        ADataSize := dbtData.size;
        dbtData.data := nil;
        dbtData.size := 0;
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
  ADataSize: Cardinal
): Boolean;
var
  dbtKey, dbtData: DBT;
  txn: PDB_TXN;
  VOnCheckPointAllow: Boolean;
begin
  VOnCheckPointAllow := False;
  FCS.Acquire;
  try
    Result := False;
    if FDBEnabled then begin
      FSyncAllow := True;
      FillChar(dbtKey, Sizeof(DBT), 0);
      FillChar(dbtData, Sizeof(DBT), 0);
      dbtKey.data := AKey;
      dbtKey.size := AKeySize;
      dbtData.data := AData;
      dbtData.size := ADataSize;
      txn := GetTransaction;
      Result := CheckAndNotExistsBDB(FDB.put(FDB, txn, @dbtKey, @dbtData, 0));
      VOnCheckPointAllow := Result;
    end;
  finally
    FCS.Release;
  end;
  if VOnCheckPointAllow and (Addr(FOnCheckPoint) <> nil) then begin
    FOnCheckPoint(Self);
  end;
end;

function TBerkeleyDB.Exists(
  AKey: Pointer;
  AKeySize: Cardinal
): Boolean;
var
  dbtKey: DBT;
  txn: PDB_TXN;
begin
  FCS.Acquire;
  try
    Result := False;
    if FDBEnabled then begin
      FillChar(dbtKey, Sizeof(DBT), 0);
      dbtKey.data := AKey;
      dbtKey.size := AKeySize;
      txn := GetTransaction;
      Result := CheckAndFoundBDB(FDB.exists(FDB, txn, @dbtKey, 0));
    end;
  finally
    FCS.Release;
  end;
end;

function TBerkeleyDB.Del(
  AKey: Pointer;
  AKeySize: Cardinal
): Boolean;
var
  dbtKey: DBT;
  txn: PDB_TXN;
  VOnCheckPointAllow: Boolean;
begin
  VOnCheckPointAllow := False;
  FCS.Acquire;
  try
    Result := False;
    if FDBEnabled then begin
      FillChar(dbtKey, Sizeof(DBT), 0);
      dbtKey.data := AKey;
      dbtKey.size := AKeySize;
      txn := GetTransaction;
      Result := CheckAndFoundBDB(FDB.del(FDB, txn, @dbtKey, 0));
      VOnCheckPointAllow := Result;
    end;
  finally
    FCS.Release;
  end;
  if VOnCheckPointAllow and (Addr(FOnCheckPoint) <> nil) then begin
    FOnCheckPoint(Self);
  end;
end;

function TBerkeleyDB.Sync(): Boolean;
begin
  FCS.Acquire;
  try
    Result := False;
    if FDBEnabled and FSyncAllow then begin
      FSyncAllow := False;
      CommitTransaction;
      CheckBDB(FDB.sync(FDB, 0));
      Result := True; 
    end;
  finally
    FCS.Release;
  end;
end;

function TBerkeleyDB.GetKeyExistsList(
  const AKeySize: Cardinal;
  out AKeyList: TList
): Boolean;
var
  dbtKey, dbtData: DBT;
  txn: PDB_TXN;
  dbc: PDBC;
begin
  FCS.Acquire;
  try
    Result := False;
    AKeyList.Clear;
    if FDBEnabled then begin
      txn := GetTransaction;
      CheckBDB(FDB.cursor(FDB, txn, @dbc, 0));
      try
        repeat
          FillChar(dbtKey, Sizeof(DBT), 0);
          FillChar(dbtData, Sizeof(DBT), 0);

          dbtKey.flags := DB_DBT_MALLOC;

          dbtData.dlen := 0;
          dbtData.doff := 0;
          dbtData.flags := DB_DBT_PARTIAL;

          if CheckAndFoundBDB(dbc.get(dbc, @dbtKey, @dbtData, DB_NEXT)) then begin
            if (dbtKey.data <> nil) and (dbtKey.size = AKeySize) then begin
              AKeyList.Add(dbtKey.data);
            end else begin
              Break;
            end;
          end else begin
            Break;
          end;
        until False;
      finally
        CheckBDBandNil(dbc.close(dbc), dbc);
      end;
      Result := AKeyList.Count > 0;
    end;
  finally
    FCS.Release;
  end;
end;

end.

