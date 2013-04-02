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
  i_Notifier,
  i_Listener,
  i_BinaryData,
  i_SimpleFlag,
  i_BerkeleyDB,
  i_BerkeleyDBEnv,
  i_GlobalBerkeleyDBHelper,
  u_BaseInterfacedObject;

type
  TBerkeleyDB = class(TBaseInterfacedObject, IBerkeleyDB)
  private
    db: PDB;
    dbenv: PDB_ENV;
  private
    FHelper: IGlobalBerkeleyDBHelper;
    FEnvRootPath: string;
    FPageSize: Cardinal;
    FFileName: string;
    FSyncAllow: ISimpleFlag;
    FOperationsCount: ICounter;
    FLock: IReadWriteSync;
    FSyncCallListener: IListener;
    FSyncCallNotifier: INotifierInternal;
    FOnDeadLockRetryCount: Integer;
    function IsNeedDoSync: Boolean;
  private
    { IBerkeleyDB }
    procedure Open(const ADatabaseFileName: string);
    procedure Close;
    function Read(const AKey: IBinaryData): IBinaryData; overload;
    function Write(const AKey, AValue: IBinaryData): Boolean; overload;
    function Exists(const AKey: IBinaryData): Boolean; overload;
    function Del(const AKey: IBinaryData): Boolean; overload;
    function Read(const AKey: IBinaryData; const ATxn: PBerkeleyTxn; out AIsDeadLock: Boolean; const AFlag: Cardinal = 0): IBinaryData; overload;
    function Write(const AKey, AValue: IBinaryData; const ATxn: PBerkeleyTxn; out AIsDeadLock: Boolean): Boolean; overload;
    function Exists(const AKey: IBinaryData; const ATxn: PBerkeleyTxn; out AIsDeadLock: Boolean): Boolean; overload;
    function Del(const AKey: IBinaryData; const ATxn: PBerkeleyTxn; out AIsDeadLock: Boolean): Boolean; overload;
    function ExistsList: IInterfaceList;
    procedure Sync(const ASyncWithNotifier: Boolean);
    function GetFileName: string;
  public
    constructor Create(
      const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
      const AEnvironment: IBerkeleyDBEnvironment;
      const ASyncCallListener: IListener;
      const APageSize: Cardinal
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_Notifier,
  u_BinaryData,
  u_Synchronizer,
  u_SimpleFlagWithInterlock;

type
  EBerkeleyDB = class(Exception);

const
  cBerkeleyDBErrPfx = 'BerkeleyDB';
  cMaxOperationsCountToSync = 1024;

{ TBerkeleyDB }

constructor TBerkeleyDB.Create(
  const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
  const AEnvironment: IBerkeleyDBEnvironment;
  const ASyncCallListener: IListener;
  const APageSize: Cardinal
);
begin
  inherited Create;
  FHelper := AGlobalBerkeleyDBHelper;
  FPageSize := APageSize;
  db := nil;
  dbenv := AEnvironment.dbenv;
  FEnvRootPath := AEnvironment.RootPath;
  FFileName := '';                      
  FLock := MakeSyncRW_Std(Self, False);
  FSyncAllow := TSimpleFlagWithInterlock.Create;
  FOperationsCount := TCounterInterlock.Create;
  FSyncCallListener := ASyncCallListener;
  if Assigned(FSyncCallListener) then begin
    FSyncCallNotifier := TNotifierBase.Create;
    FSyncCallNotifier.Add(FSyncCallListener);
  end;
  FOnDeadLockRetryCount := 3;
end;

destructor TBerkeleyDB.Destroy;
begin
  try
    Sync(False);
    Close;
  finally
    if Assigned(FSyncCallNotifier) then begin
      FSyncCallNotifier.Remove(FSyncCallListener);
      FSyncCallListener := nil;
      FSyncCallNotifier := nil;
    end;
    FLock := nil;
    FHelper := nil;
    inherited Destroy;
  end;
end;

procedure TBerkeleyDB.Open(const ADatabaseFileName: string);
var
  VErrorMsg: string;
  VRelativeFileName: AnsiString;
begin
  FLock.BeginWrite;
  try
    if db = nil then
    try
      FFileName := ADatabaseFileName;
      VRelativeFileName :=
        StringReplace(FFileName, FEnvRootPath, '', [rfIgnoreCase]);
      CheckBDB(db_create(db, dbenv, 0));
      db.set_errpfx(db, cBerkeleyDBErrPfx);
      if not FileExists(FFileName) then begin
        CheckBDB(db.set_pagesize(db, FPageSize));
      end;
      CheckBDB(db.open(db, nil, Pointer(AnsiToUtf8(VRelativeFileName)), '',
        DB_BTREE, (DB_CREATE_ or DB_AUTO_COMMIT or DB_THREAD), 0));
    except
      on E: Exception do begin
        VErrorMsg := '';
        if db <> nil then
        try
          CheckBDBandNil(db.close(db, 0), db);
        except
          on EClose: Exception do
            VErrorMsg := EClose.ClassName + ': ' + EClose.Message + #13#10;
        end;
        VErrorMsg := VErrorMsg + E.ClassName + ': ' + E.Message;
        FHelper.RaiseException(VErrorMsg);
      end;
    end;
  finally
    FLock.EndWrite;
  end;
end;

procedure TBerkeleyDB.Close;
begin
  try
    FLock.BeginWrite;
    try
      CheckBDBandNil(db.close(db, 0), db);
    finally
      FLock.EndWrite;
    end;
  except
    on E: Exception do
      FHelper.RaiseException(E.ClassName + ': ' + E.Message);
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
  Result := nil;
  VFound := False;
  try
    FillChar(dbtKey, Sizeof(DBT), 0);
    FillChar(dbtData, Sizeof(DBT), 0);

    dbtKey.data := AKey.Buffer;
    dbtKey.size := AKey.Size;

    dbtData.flags := DB_DBT_MALLOC; // -> память должен освобождать юзер

    I := 0;
    repeat
      Inc(I);

      FLock.BeginWrite;
      try
        ret := db.get(db, PDB_TXN(ATxn), @dbtKey, @dbtData, AFlag);
      finally
        FLock.EndWrite;
      end;

      case ret of
        DB_LOCK_DEADLOCK: begin
          AIsDeadLock := True;
          if ATxn <> nil then begin
            Break;
          end;
        end;
      else
        AIsDeadLock := False;
        VFound := CheckAndFoundBDB(ret);
        Break;
      end;
    until I > FOnDeadLockRetryCount;

    if AIsDeadLock and (ATxn = nil) then begin
      CheckBDB(DB_LOCK_DEADLOCK); // <- raise exception about deadlock
    end;

    if VFound then begin
      if (dbtData.data <> nil) and (dbtData.size > 0) then begin
        Result := TBinaryData.Create(dbtData.size, dbtData.data, True);
      end else begin
        raise EBerkeleyDB.Create('Value not assigned!');
      end;
    end else begin
      Result := nil;
    end;
  except
    on E: Exception do
      FHelper.RaiseException(E.ClassName + ': ' + E.Message);
  end;
end;

function TBerkeleyDB.Write(const AKey, AValue: IBinaryData; const ATxn: PBerkeleyTxn; out AIsDeadLock: Boolean): Boolean;
var
  I: Integer;
  ret: Integer;
  dbtKey, dbtData: DBT;
begin
  Result := False;
  AIsDeadLock := False;
  try
    FillChar(dbtKey, Sizeof(DBT), 0);
    FillChar(dbtData, Sizeof(DBT), 0);

    dbtKey.data := AKey.Buffer;
    dbtKey.size := AKey.Size;

    dbtData.data := AValue.Buffer;
    dbtData.size := AValue.Size;

    I := 0;
    repeat
      Inc(I);

      FLock.BeginWrite;
      try
        ret := db.put(db, PDB_TXN(ATxn), @dbtKey, @dbtData, 0);
      finally
        FLock.EndWrite;
      end;

      case ret of
        DB_LOCK_DEADLOCK: begin
          AIsDeadLock := True;
          if ATxn <> nil then begin
            Break;
          end;
        end;
      else
        AIsDeadLock := False;
        Result := CheckAndNotExistsBDB(ret);
        Break;
      end;
    until I > FOnDeadLockRetryCount;

    if AIsDeadLock and (ATxn = nil) then begin
      CheckBDB(DB_LOCK_DEADLOCK); // <- raise exception about deadlock
    end;

    if Result then begin
      if IsNeedDoSync then begin
        Sync(True);
      end;
    end;
  except
    on E: Exception do
      FHelper.RaiseException(E.ClassName + ': ' + E.Message);
  end;
end;

function TBerkeleyDB.Exists(const AKey: IBinaryData; const ATxn: PBerkeleyTxn; out AIsDeadLock: Boolean): Boolean;
var
  I: Integer;
  ret: Integer;
  dbtKey: DBT;
begin
  Result := False;
  try
    FillChar(dbtKey, Sizeof(DBT), 0);

    dbtKey.data := AKey.Buffer;
    dbtKey.size := AKey.Size;

    I := 0;
    repeat
      Inc(I);

      FLock.BeginWrite;
      try
        ret := db.exists(db, PDB_TXN(ATxn), @dbtKey, 0);
      finally
        FLock.EndWrite;
      end;

      case ret of
        DB_LOCK_DEADLOCK: begin
          AIsDeadLock := True;
          if ATxn <> nil then begin
            Break;
          end;
        end;
      else
        AIsDeadLock := False;
        Result := CheckAndFoundBDB(ret);
        Break;
      end;
    until I > FOnDeadLockRetryCount;

    if AIsDeadLock and (ATxn = nil) then begin
      CheckBDB(DB_LOCK_DEADLOCK); // <- raise exception about deadlock
    end;
  except
    on E: Exception do
      FHelper.RaiseException(E.ClassName + ': ' + E.Message);
  end;
end;

function TBerkeleyDB.Del(const AKey: IBinaryData; const ATxn: PBerkeleyTxn; out AIsDeadLock: Boolean): Boolean;
var
  I: Integer;
  ret: Integer;
  dbtKey: DBT;
begin
  Result := False;
  try
    FillChar(dbtKey, Sizeof(DBT), 0);

    dbtKey.data := AKey.Buffer;
    dbtKey.size := AKey.Size;

    I := 0;
    repeat
      Inc(I);

      FLock.BeginWrite;
      try
        ret := db.del(db, PDB_TXN(ATxn), @dbtKey, 0);
      finally
        FLock.EndWrite;
      end;

      case ret of
        DB_LOCK_DEADLOCK: begin
          AIsDeadLock := True;
          if ATxn <> nil then begin
            Break;
          end;
        end;
      else
        AIsDeadLock := False;
        Result := CheckAndFoundBDB(ret);
        Break;
      end;
    until I > FOnDeadLockRetryCount;

    if AIsDeadLock and (ATxn = nil) then begin
      CheckBDB(DB_LOCK_DEADLOCK); // <- raise exception about deadlock
    end;

    if Result then begin
      if IsNeedDoSync then begin
        Sync(True);
      end;
    end else begin
      // key not found
    end;
  except
    on E: Exception do
      FHelper.RaiseException(E.ClassName + ': ' + E.Message);
  end;
end;

procedure TBerkeleyDB.Sync(const ASyncWithNotifier: Boolean);
begin
  try
    if FSyncAllow.CheckFlagAndReset then begin
      FLock.BeginWrite;
      try
        CheckBDB(db.sync(db, 0));
      finally
        FLock.EndWrite;
      end;
      if ASyncWithNotifier and Assigned(FSyncCallNotifier) then begin
        FSyncCallNotifier.Notify(nil);
      end;
    end
  except
    on E: Exception do
      FHelper.RaiseException(E.ClassName + ': ' + E.Message);
  end;
end;

function TBerkeleyDB.ExistsList: IInterfaceList;
var
  dbtKey, dbtData: DBT;
  dbc: PDBC;
  VKey: IBinaryData;
begin
  try
    Result := TInterfaceList.Create;
    FLock.BeginWrite;
    try
      CheckBDB(db.cursor(db, nil, @dbc, 0));
      try
        repeat
          FillChar(dbtKey, Sizeof(DBT), 0);
          FillChar(dbtData, Sizeof(DBT), 0);

          dbtKey.flags := DB_DBT_MALLOC;   // -> память должен освобождать юзер

          dbtData.dlen := 0;
          dbtData.doff := 0;
          dbtData.flags := DB_DBT_PARTIAL; // -> не считывать value

          if CheckAndFoundBDB(dbc.get(dbc, @dbtKey, @dbtData, DB_NEXT)) then begin
            if (dbtKey.data <> nil) and (dbtKey.size > 0) then begin
              VKey := TBinaryData.Create(dbtKey.size, dbtKey.data, True);
              Result.Add(VKey);
            end;
          end else begin
            Break;
          end;
        until False;
      finally
        CheckBDBandNil(dbc.close(dbc), dbc);
      end;
    finally
      FLock.EndWrite;
    end;
  except
    on E: Exception do
      FHelper.RaiseException(E.ClassName + ': ' + E.Message);
  end;
end;

function TBerkeleyDB.GetFileName: string;
begin
  Result := FFileName;
end;

function TBerkeleyDB.IsNeedDoSync: Boolean;
var
  VCount: Integer;
begin
  FSyncAllow.SetFlag;
  VCount := FOperationsCount.Inc;
  Result := (VCount >= cMaxOperationsCountToSync);
  if Result then begin
    FOperationsCount.Reset;
  end;
end;

end.

