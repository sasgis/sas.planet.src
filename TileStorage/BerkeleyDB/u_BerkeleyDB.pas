{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2013, SAS.Planet development team.                      *}
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
  libdb51,
  t_BerkeleyDB,
  i_BinaryData,
  i_BerkeleyDB,
  i_BerkeleyDBEnv,
  u_BaseInterfacedObject;

type
  TBerkeleyDB = class(TBaseInterfacedObject, IBerkeleyDB)
  private
    db: PDB;
    dbenv: PDB_ENV;
  private
    FEnvRootPath: string;
    FPageSize: Cardinal;
    FFileName: string;
    FIsReadOnly: Boolean;
    FLock: IReadWriteSync;
    FOnDeadLockRetryCount: Integer;
  private
    { IBerkeleyDB }
    procedure Open(const ADatabaseFileName: string);
    function Read(const AKey: IBinaryData): IBinaryData; overload;
    function Write(const AKey, AValue: IBinaryData): Boolean; overload;
    function Exists(const AKey: IBinaryData): Boolean; overload;
    function Del(const AKey: IBinaryData): Boolean; overload;
    function Read(const AKey: IBinaryData; const ATxn: PBerkeleyTxn; out AIsDeadLock: Boolean; const AFlag: Cardinal = 0): IBinaryData; overload;
    function Write(const AKey, AValue: IBinaryData; const ATxn: PBerkeleyTxn; out AIsDeadLock: Boolean): Boolean; overload;
    function Exists(const AKey: IBinaryData; const ATxn: PBerkeleyTxn; out AIsDeadLock: Boolean; const AFlag: Cardinal = 0): Boolean; overload;
    function Del(const AKey: IBinaryData; const ATxn: PBerkeleyTxn; out AIsDeadLock: Boolean): Boolean; overload;
    function CreateExistsKeyArray(out AKeyArray: TExistsKeyArray): Boolean;
    procedure ReleaseExistsKeyArray(var AKeyArray: TExistsKeyArray);
    procedure Sync;
    function GetFileName: string;
    procedure LockRead;
    procedure UnlockRead;
    function LockWrite: Boolean;
    procedure UnlockWrite;
  public
    constructor Create(
      const AEnvironment: IBerkeleyDBEnvironment;
      const AIsReadOnly: Boolean;
      const AOnDeadLockRetryCount: Integer;
      const APageSize: Cardinal
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_BinaryData,
  u_Synchronizer;

type
  EBerkeleyDB = class(Exception);

const
  cBerkeleyDBErrPfx: AnsiString = 'BerkeleyDB';

{ TBerkeleyDB }

constructor TBerkeleyDB.Create(
  const AEnvironment: IBerkeleyDBEnvironment;
  const AIsReadOnly: Boolean;
  const AOnDeadLockRetryCount: Integer;
  const APageSize: Cardinal
);
begin
  Assert(AEnvironment <> nil);
  inherited Create;
  FPageSize := APageSize;
  FIsReadOnly := AIsReadOnly;
  db := nil;
  dbenv := AEnvironment.dbenv;
  FEnvRootPath := AEnvironment.RootPath;
  FFileName := '';
  FLock := MakeSyncRW_Big(Self, False);
  FOnDeadLockRetryCount := AOnDeadLockRetryCount;
end;

destructor TBerkeleyDB.Destroy;
begin
  if db <> nil then begin
    CheckBDBandNil(db.close(db, 0), db);
  end;
  FLock := nil;
  inherited;
end;

procedure TBerkeleyDB.LockRead;
begin
  FLock.BeginRead;
end;

procedure TBerkeleyDB.UnlockRead;
begin
  FLock.EndRead;
end;
function TBerkeleyDB.LockWrite: Boolean;
begin
  Result := FLock.BeginWrite;
end;

procedure TBerkeleyDB.UnlockWrite;
begin
  FLock.EndWrite;
end;

procedure TBerkeleyDB.Open(const ADatabaseFileName: string);
var
  I: Integer;
  ret: Integer;
  VRelativeFileName: UTF8String;
  VOpenFlags: Cardinal;
begin
  if db = nil then
  try
    FFileName := ADatabaseFileName;
    VRelativeFileName := AnsiToUtf8(StringReplace(FFileName, FEnvRootPath, '', [rfIgnoreCase]));
    CheckBDB(db_create(db, dbenv, 0));
    Assert(db <> nil);
    db.set_errpfx(db, PAnsiChar(cBerkeleyDBErrPfx));
    if not FileExists(FFileName) then begin
      CheckBDB(db.set_pagesize(db, FPageSize));
    end;
    if FIsReadOnly then begin
      VOpenFlags := DB_RDONLY or DB_THREAD;
    end else begin
      VOpenFlags := DB_CREATE_ or DB_AUTO_COMMIT or DB_THREAD;
    end;

    I := 0;
    repeat
      ret := db.open(db, nil, PAnsiChar(VRelativeFileName), '', DB_BTREE, VOpenFlags, 0);
      if ret = DB_LOCK_DEADLOCK then begin
        Sleep(50);
        Inc(I);
      end else begin
        Break;
      end;
    until I > FOnDeadLockRetryCount;

    CheckBDB(ret);
  except
    if db <> nil then begin
      CheckBDBandNil(db.close(db, 0), db);
    end;
    raise;
  end;
end;

function TBerkeleyDB.Read(const AKey: IBinaryData): IBinaryData;
var
  b: Boolean;
begin
  Result := Read(AKey, nil, b);
end;
function TBerkeleyDB.Write(const AKey, AValue: IBinaryData): Boolean;
var
  b: Boolean;
begin
  Result := Write(AKey, AValue, nil, b);
end;

function TBerkeleyDB.Exists(const AKey: IBinaryData): Boolean;
var
  b: Boolean;
begin
  Result := Exists(AKey, nil, b);
end;

function TBerkeleyDB.Del(const AKey: IBinaryData): Boolean;
var
  b: Boolean;
begin
  Result := Del(AKey, nil, b);
end;

function TBerkeleyDB.Read(const AKey: IBinaryData; const ATxn: PBerkeleyTxn; out AIsDeadLock: Boolean; const AFlag: Cardinal = 0): IBinaryData;
var
  I: Integer;
  ret: Integer;
  dbtKey, dbtData: DBT;
  VFound: Boolean;
begin
  Assert(AKey <> nil);
  Assert(db <> nil);

  I := 0;
  Result := nil;
  VFound := False;

  FillChar(dbtKey, Sizeof(DBT), 0);
  FillChar(dbtData, Sizeof(DBT), 0);

  dbtKey.data := AKey.Buffer;
  dbtKey.size := AKey.Size;

  dbtData.flags := DB_DBT_MALLOC;

  repeat
    ret := db.get(db, PDB_TXN(ATxn), @dbtKey, @dbtData, AFlag);

    case ret of
      DB_LOCK_DEADLOCK: begin
        AIsDeadLock := True;
        if ATxn <> nil then begin
          Break;
        end else begin
          Sleep(50);
        end;
      end;
    else
      AIsDeadLock := False;
      VFound := CheckAndFoundBDB(ret);
      Break;
    end;
    Inc(I);
  until I > FOnDeadLockRetryCount;

  if AIsDeadLock and (ATxn = nil) then begin
    CheckBDB(DB_LOCK_DEADLOCK);
  end;

  if VFound then begin
    if (dbtData.data <> nil) and (dbtData.size > 0) then begin
      Result := TBinaryData.CreateWithOwn(dbtData.size, dbtData.data);
    end else begin
      raise EBerkeleyDB.Create('Value not assigned!');
    end;
  end else begin
    Result := nil;
  end;
end;

function TBerkeleyDB.Write(const AKey, AValue: IBinaryData; const ATxn: PBerkeleyTxn; out AIsDeadLock: Boolean): Boolean;
var
  I: Integer;
  ret: Integer;
  dbtKey, dbtData: DBT;
begin
  Assert(AKey <> nil);
  Assert(AValue <> nil);
  Assert(db <> nil);
  Assert(not FIsReadOnly);

  I := 0;
  Result := False;
  AIsDeadLock := False;

  FillChar(dbtKey, Sizeof(DBT), 0);
  FillChar(dbtData, Sizeof(DBT), 0);

  dbtKey.data := AKey.Buffer;
  dbtKey.size := AKey.Size;

  dbtData.data := AValue.Buffer;
  dbtData.size := AValue.Size;

  repeat
    ret := db.put(db, PDB_TXN(ATxn), @dbtKey, @dbtData, 0);

    case ret of
      DB_LOCK_DEADLOCK: begin
        AIsDeadLock := True;
        if ATxn <> nil then begin
          Break;
        end else begin
          Sleep(50);
        end;
      end;
    else
      AIsDeadLock := False;
      Result := CheckAndNotExistsBDB(ret);
      Break;
    end;
    Inc(I);
  until I > FOnDeadLockRetryCount;

  if AIsDeadLock and (ATxn = nil) then begin
    CheckBDB(DB_LOCK_DEADLOCK);
  end;
end;

function TBerkeleyDB.Exists(const AKey: IBinaryData; const ATxn: PBerkeleyTxn; out AIsDeadLock: Boolean; const AFlag: Cardinal = 0): Boolean;
var
  I: Integer;
  ret: Integer;
  dbtKey: DBT;
begin
  Assert(AKey <> nil);
  Assert(db <> nil);

  I := 0;
  Result := False;

  FillChar(dbtKey, Sizeof(DBT), 0);

  dbtKey.data := AKey.Buffer;
  dbtKey.size := AKey.Size;

  repeat
    ret := db.exists(db, PDB_TXN(ATxn), @dbtKey, AFlag);

    case ret of
      DB_LOCK_DEADLOCK: begin
        AIsDeadLock := True;
        if ATxn <> nil then begin
          Break;
        end else begin
          Sleep(50);
        end;
      end;
    else
      AIsDeadLock := False;
      Result := CheckAndFoundBDB(ret);
      Break;
    end;
    Inc(I);
  until I > FOnDeadLockRetryCount;

  if AIsDeadLock and (ATxn = nil) then begin
    CheckBDB(DB_LOCK_DEADLOCK);
  end;
end;

function TBerkeleyDB.Del(const AKey: IBinaryData; const ATxn: PBerkeleyTxn; out AIsDeadLock: Boolean): Boolean;
var
  I: Integer;
  ret: Integer;
  dbtKey: DBT;
begin
  Assert(AKey <> nil);
  Assert(db <> nil);
  Assert(not FIsReadOnly);

  I := 0;
  Result := False;

  FillChar(dbtKey, Sizeof(DBT), 0);

  dbtKey.data := AKey.Buffer;
  dbtKey.size := AKey.Size;

  repeat
    ret := db.del(db, PDB_TXN(ATxn), @dbtKey, 0);

    case ret of
      DB_LOCK_DEADLOCK: begin
        AIsDeadLock := True;
        if ATxn <> nil then begin
          Break;
        end else begin
          Sleep(50);
        end;
      end;
    else
      AIsDeadLock := False;
      Result := CheckAndFoundBDB(ret);
      Break;
    end;
    Inc(I);
  until I > FOnDeadLockRetryCount;

  if AIsDeadLock and (ATxn = nil) then begin
    CheckBDB(DB_LOCK_DEADLOCK);
  end;
end;

procedure TBerkeleyDB.Sync;
begin
  if (db <> nil) then begin
    CheckBDB(db.sync(db, 0));
  end;
end;

function TBerkeleyDB.CreateExistsKeyArray(out AKeyArray: TExistsKeyArray): Boolean;
var
  I: Integer;
  VIsFound: Boolean;
  dbtKey, dbtData: DBT;
  dbc: PDBC;
begin
  Assert(db <> nil);

  SetLength(AKeyArray, 0);

  CheckBDB(db.cursor(db, nil, @dbc, 0));
  try
    repeat
      FillChar(dbtKey, Sizeof(DBT), 0);
      FillChar(dbtData, Sizeof(DBT), 0);

      dbtKey.flags := DB_DBT_MALLOC;

      dbtData.dlen := 0;
      dbtData.doff := 0;
      dbtData.flags := DB_DBT_PARTIAL;

      if CheckAndFoundBDB(dbc.get(dbc, @dbtKey, @dbtData, DB_NEXT)) then begin
        if (dbtKey.data <> nil) and (dbtKey.size > 0) then begin
          VIsFound := False;
          for I := 0 to Length(AKeyArray) - 1 do begin
            if AKeyArray[I].KeySize = Integer(dbtKey.size) then begin
              VIsFound := True;
              AKeyArray[I].KeyData.Add(dbtKey.data);
              Break;
            end;
          end;
          if not VIsFound then begin
            I := Length(AKeyArray);
            SetLength(AKeyArray, I + 1);
            AKeyArray[I].KeySize := dbtKey.size;
            AKeyArray[I].KeyData := TList.Create;
            AKeyArray[I].KeyData.Add(dbtKey.data);
          end;
        end;
      end else begin
        Break;
      end;
    until False;

    Result := Length(AKeyArray) > 0;
  finally
    CheckBDBandNil(dbc.close(dbc), dbc);
  end; 
end;

procedure TBerkeleyDB.ReleaseExistsKeyArray(var AKeyArray: TExistsKeyArray);
var
  I, J: Integer;
begin
  for I := 0 to Length(AKeyArray) - 1 do begin
    for J := 0 to AKeyArray[I].KeyData.Count - 1 do begin
      if AKeyArray[I].KeyData.Items[J] <> nil then begin
        FreeMemory(AKeyArray[I].KeyData.Items[J]);
      end;
    end;
    AKeyArray[I].KeyData.Free;
  end;
  SetLength(AKeyArray, 0);
end;

function TBerkeleyDB.GetFileName: string;
begin
  Result := FFileName;
end;

end.

