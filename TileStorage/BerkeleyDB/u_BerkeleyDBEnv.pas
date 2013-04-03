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

unit u_BerkeleyDBEnv;

interface

uses
  Classes,
  SyncObjs,
  libdb51,
  i_Listener,
  i_BerkeleyDBEnv,
  i_GlobalBerkeleyDBHelper,
  u_BaseInterfacedObject;

type
  TBerkeleyDBEnvAppPrivate = record
    FEnvRootPath: string;
    FHelper: IGlobalBerkeleyDBHelper;
  end;
  PBerkeleyDBEnvAppPrivate = ^TBerkeleyDBEnvAppPrivate;

  TBerkeleyDBEnv = class(TBaseInterfacedObject, IBerkeleyDBEnvironment)
  private
    dbenv: PDB_ENV;
  private
    FAppPrivate: PBerkeleyDBEnvAppPrivate;
    FActive: Boolean;
    FLibInitOk: Boolean;
    FLastRemoveLogTime: Cardinal;
    FCS: TCriticalSection;
    FClientsCount: Integer;
    FListener: IListener;
    FTxnList: TList;
    function Open: Boolean;
    procedure Sync;
    procedure MakeDefConfigFile(const AEnvHomePath: string);
  private
    { IBerkeleyDBEnvironment }
    function GetEnvironmentPointerForApi: Pointer;
    function GetRootPath: string;
    function GetClientsCount: Integer;
    procedure SetClientsCount(const AValue: Integer);
    procedure RemoveUnUsedLogs;
    procedure TransactionBegin(out ATxn: PBerkeleyTxn);
    procedure TransactionCommit(var ATxn: PBerkeleyTxn);
    procedure TransactionAbort(var ATxn: PBerkeleyTxn);
    procedure TransactionCheckPoint;
    function GetSyncCallListener: IListener;
  public
    constructor Create(
      const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
      const AEnvRootPath: string
    );
    destructor Destroy; override;
  end;

implementation

uses
  Windows,
  SysUtils,
  u_ListenerByEvent;

const
  cBerkeleyDBEnvSubDir = 'env';
  cBerkeleyDBEnvErrPfx = 'BerkeleyDB Env';
  cBerkeleyDBEnvConfig = 'DB_CONFIG';

procedure BerkeleyDBErrCall(dbenv: PDB_ENV; errpfx, msg: PAnsiChar); cdecl;
var
  VMsg: AnsiString;
  VEnvPrivate: PBerkeleyDBEnvAppPrivate;
begin
  VMsg := errpfx + AnsiString(': ') + msg;
  VEnvPrivate := dbenv.app_private;
  if Assigned(VEnvPrivate) then begin
    VMsg := VMsg + ' (' + VEnvPrivate.FEnvRootPath + ')';
  end;
  if Assigned(VEnvPrivate) and Assigned(VEnvPrivate.FHelper) then begin
    VEnvPrivate.FHelper.RaiseException(VMsg);
  end else begin
    raise EBerkeleyDBExeption.Create(string(VMsg));
  end;
end;

{ TBerkeleyDBEnv }

constructor TBerkeleyDBEnv.Create(
  const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
  const AEnvRootPath: string
);
begin
  inherited Create;
  dbenv := nil;
  FTxnList := TList.Create;
  New(FAppPrivate);
  FAppPrivate.FEnvRootPath := AEnvRootPath;
  FAppPrivate.FHelper := AGlobalBerkeleyDBHelper;
  FCS := TCriticalSection.Create;
  FActive := False;
  FLastRemoveLogTime := 0;
  FClientsCount := 1;
  FLibInitOk := InitBerkeleyDB;
  FListener := TNotifyNoMmgEventListener.Create(Self.Sync);
end;

destructor TBerkeleyDBEnv.Destroy;
var
  I: Integer;
  txn: PDB_TXN;
begin
  try
    FListener := nil;
    if dbenv <> nil then begin
      for I := 0 to FTxnList.Count - 1 do begin
        txn := FTxnList.Items[I];
        if txn <> nil then begin
          txn.abort(txn);
        end;
      end;
      TransactionCheckPoint;
      RemoveUnUsedLogs;
      dbenv.close(dbenv, 0);
    end;
  finally
    FAppPrivate.FHelper := nil;
    Dispose(FAppPrivate);
    FTxnList.Free;
    FCS.Free;
    inherited Destroy;
  end;
end;

function TBerkeleyDBEnv.GetRootPath: string;
begin
  if Assigned(FAppPrivate) then begin
    Result := FAppPrivate.FEnvRootPath;
  end else begin
    Result := '';
  end;
end;

procedure TBerkeleyDBEnv.MakeDefConfigFile(const AEnvHomePath: string);
var
  VFile: string;
  VList: TStringList;
begin
  VFile := AEnvHomePath + cBerkeleyDBEnvConfig;
  if not FileExists(VFile) then begin
    VList := TStringList.Create;
    try
      VList.Add('set_flags DB_TXN_NOSYNC on');
      VList.Add('set_flags DB_TXN_WRITE_NOSYNC on');
      VList.Add('set_verbose DB_VERB_RECOVERY on');
      VList.Add('set_data_dir ..');
      VList.Add('');
      VList.Add('#Logs dir');
      VList.Add('set_lg_dir .');
      VList.Add('');
      VList.Add('#Single log-file size in bytes (def = 10M)');
      VList.Add('set_lg_max 10485760');
      VList.Add('');
      VList.Add('#Log in-memory buffer size in bytes (def = 256k)');
      VList.Add('set_lg_bsize 10485760');
      VList.Add('');
      VList.Add('log_set_config DB_LOG_AUTO_REMOVE on');
      VList.SaveToFile(VFile);
    finally
      VList.Free;
    end;
  end;
end;

function TBerkeleyDBEnv.Open: Boolean;
var
  VPath: AnsiString;
begin
  if not FActive and FLibInitOk then begin

    VPath := FAppPrivate.FEnvRootPath + cBerkeleyDBEnvSubDir + PathDelim;
    if not DirectoryExists(VPath) then begin
      ForceDirectories(VPath);
    end;

    MakeDefConfigFile(VPath);
    
    CheckBDB(db_env_create(dbenv, 0));
    dbenv.app_private := FAppPrivate;
    dbenv.set_errpfx(dbenv, CBerkeleyDBEnvErrPfx);
    dbenv.set_errcall(dbenv, BerkeleyDBErrCall);
    CheckBDB(dbenv.set_alloc(dbenv, @GetMemory, @ReallocMemory, @FreeMemory));

    CheckBDB(
      dbenv.open(
        dbenv,
        Pointer(AnsiToUtf8(VPath)),
        DB_CREATE_ or
        DB_RECOVER or
        DB_INIT_LOCK or
        DB_INIT_LOG or
        DB_INIT_MPOOL or
        DB_INIT_TXN or
        DB_REGISTER or
        DB_THREAD,
        0
      )
    );
    FActive := True;
  end;
  Result := FActive and FLibInitOk;
end;

procedure TBerkeleyDBEnv.RemoveUnUsedLogs;
begin
  FCS.Acquire;
  try
    if FActive and FLibInitOk and (GetTickCount - FLastRemoveLogTime > 30000) then begin
      FLastRemoveLogTime := GetTickCount;
      CheckBDB(dbenv.log_archive(dbenv, nil, DB_ARCH_REMOVE));
    end;
  finally
    FCS.Release;
  end;
end;

procedure TBerkeleyDBEnv.TransactionBegin(out ATxn: PBerkeleyTxn);
var
  txn: PDB_TXN;
begin
  FCS.Acquire;
  try
    if FActive and FLibInitOk then begin
      txn := nil;
      CheckBDB(dbenv.txn_begin(dbenv, nil, @txn, DB_TXN_NOWAIT));
      ATxn := txn;
      FTxnList.Add(txn);
    end else begin
      ATxn := nil;
    end;
  finally
    FCS.Release;
  end;
end;

procedure TBerkeleyDBEnv.TransactionCommit(var ATxn: PBerkeleyTxn);
var
  txn: PDB_TXN;
begin
  if ATxn <> nil then begin
    txn := ATxn;
    ATxn := nil;
    FCS.Acquire;
    try
      if FActive and FLibInitOk then begin
        FTxnList.Remove(txn);
        CheckBDBandNil(txn.commit(txn, 0), txn);
      end;
    finally
      FCS.Release;
    end;
  end;
end;

procedure TBerkeleyDBEnv.TransactionAbort(var ATxn: PBerkeleyTxn);
var
  txn: PDB_TXN;
begin
  if ATxn <> nil then begin
    txn := ATxn;
    ATxn := nil;
    FCS.Acquire;
    try
      if FActive and FLibInitOk then begin
        FTxnList.Remove(txn);
        CheckBDBandNil(txn.abort(txn), txn);
      end;
    finally
      FCS.Release;
    end;
  end;
end;

procedure TBerkeleyDBEnv.TransactionCheckPoint;
begin
  FCS.Acquire;
  try
    if FActive and FLibInitOk then begin
      CheckBDB(dbenv.txn_checkpoint(dbenv, 0, 0, DB_FORCE));
    end;
  finally
    FCS.Release;
  end;
end;

procedure TBerkeleyDBEnv.Sync;
begin
  TransactionCheckPoint;
  RemoveUnUsedLogs;
end;

function TBerkeleyDBEnv.GetEnvironmentPointerForApi: Pointer;
begin
  FCS.Acquire;
  try
    if Open then begin
      Result := dbenv;
    end else begin
      Result := nil;
    end;
  finally
    FCS.Release;
  end;
end;

function TBerkeleyDBEnv.GetClientsCount: Integer;
begin
  FCS.Acquire;
  try
    Result := FClientsCount;
  finally
    FCS.Release;
  end;
end;

procedure TBerkeleyDBEnv.SetClientsCount(const AValue: Integer);
begin
  FCS.Acquire;
  try
    FClientsCount := AValue;
  finally
    FCS.Release;
  end;
end;

function TBerkeleyDBEnv.GetSyncCallListener: IListener;
begin
  Result := FListener;
end;

end.

