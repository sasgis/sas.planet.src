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
  t_BerkeleyDB,
  i_BerkeleyDB,
  i_BerkeleyDBEnv,
  i_BerkeleyDBPool,
  i_GlobalBerkeleyDBHelper,
  i_TileStorageBerkeleyDBConfigStatic,
  u_BerkeleyDBMsgLogger,
  u_BaseInterfacedObject;

type
  TBerkeleyDBEnvAppPrivate = record
    FEnvRootPath: string;
    FHelper: IGlobalBerkeleyDBHelper;
    FMsgLogger: TBerkeleyDBMsgLogger;
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
    FTxnList: TList;
    FDatabasePool: IBerkeleyDBPool;
    FIsFullVerboseMode: Boolean;
    FDatabasePageSize: Cardinal;
    function Open: Boolean;
    procedure MakeDefConfigFile(const AEnvHomePath: string);
    procedure RemoveUnUsedLogs;
    procedure TransactionCheckPoint;
  private
    { IBerkeleyDBEnvironment }
    function GetEnvironmentPointerForApi: Pointer;
    function GetRootPath: string;
    function GetClientsCount: Integer;
    function Acquire(const ADatabaseFileName: string): IBerkeleyDB;
    procedure Release(const ADatabase: IBerkeleyDB);
    procedure SetClientsCount(const AValue: Integer);
    procedure TransactionBegin(out ATxn: PBerkeleyTxn);
    procedure TransactionCommit(var ATxn: PBerkeleyTxn);
    procedure TransactionAbort(var ATxn: PBerkeleyTxn);
    procedure Sync(out AHotDatabaseCount: Integer);
  public
    constructor Create(
      const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
      const AStorageConfig: ITileStorageBerkeleyDBConfigStatic;
      const AStorageEPSG: Integer;
      const AEnvRootPath: string
    );
    destructor Destroy; override;
  end;

implementation

uses
  Windows,
  SysUtils,
  i_BinaryData,
  i_BerkeleyDBFactory,
  u_BerkeleyDBKey,
  u_BerkeleyDBValue,
  u_BerkeleyDBPool,
  u_BerkeleyDBFactory,
  u_GlobalBerkeleyDBHelper;

const
  cBerkeleyDBEnvSubDir = 'env';
  cBerkeleyDBEnvErrPfx: AnsiString = 'BerkeleyDB Env';
  cBerkeleyDBEnvConfig = 'DB_CONFIG';
  cVerboseMsgFileName  = 'msg.log';

procedure BerkeleyDBErrCall(dbenv: PDB_ENV; errpfx, msg: PAnsiChar); cdecl;
var
  VMsg: string;
  VEnvPrivate: PBerkeleyDBEnvAppPrivate;
begin
  VMsg := string(errpfx) + ': ' + string(msg);
  VEnvPrivate := dbenv.app_private;
  if Assigned(VEnvPrivate) then begin
    VMsg := VMsg + ' [root path: ' + VEnvPrivate.FEnvRootPath + ']';
    if Assigned(VEnvPrivate.FHelper) then begin
      VEnvPrivate.FHelper.LogException('[BDB ErrCall] ' + VMsg);
    end;
  end;                                       
  raise EBerkeleyDBExeption.Create(VMsg);
end;

procedure BerkeleyDBMsgCall(dbenv: PDB_ENV; msg: PAnsiChar); cdecl;
var
  VEnvPrivate: PBerkeleyDBEnvAppPrivate;
begin
  VEnvPrivate := dbenv.app_private;
  if Assigned(VEnvPrivate) and Assigned(VEnvPrivate.FMsgLogger) then begin
    VEnvPrivate.FMsgLogger.SaveVerbMsg(string(msg));
  end;
end;

{ TBerkeleyDBEnv }

constructor TBerkeleyDBEnv.Create(
  const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
  const AStorageConfig: ITileStorageBerkeleyDBConfigStatic;
  const AStorageEPSG: Integer;
  const AEnvRootPath: string
);
var
  VDatabaseFactory: IBerkeleyDBFactory;
begin
  Assert(AGlobalBerkeleyDBHelper <> nil);
  Assert(AStorageConfig <> nil);

  inherited Create;
  dbenv := nil;
  FTxnList := TList.Create;

  New(FAppPrivate);
  FAppPrivate.FEnvRootPath := AEnvRootPath;
  FAppPrivate.FHelper := AGlobalBerkeleyDBHelper;
  FAppPrivate.FMsgLogger := TBerkeleyDBMsgLogger.Create(IncludeTrailingPathDelimiter(AEnvRootPath) + cVerboseMsgFileName);

  VDatabaseFactory := TBerkeleyDBFactory.Create(
    AStorageConfig.DatabasePageSize,
    AStorageConfig.OnDeadLockRetryCount,
    AStorageConfig.IsReadOnly,
    TBerkeleyDBMetaKey.Create as IBinaryData,
    TBerkeleyDBMetaValue.Create(AStorageEPSG) as IBinaryData
  );

  FDatabasePool := TBerkeleyDBPool.Create(
    VDatabaseFactory,
    AStorageConfig.PoolSize,
    AStorageConfig.PoolObjectTTL
  );

  FIsFullVerboseMode := AStorageConfig.IsFullVerboseMode;
  FDatabasePageSize := AStorageConfig.DatabasePageSize;
  FCS := TCriticalSection.Create;
  FActive := False;
  FLastRemoveLogTime := 0;
  FClientsCount := 1;
  FLibInitOk := InitBerkeleyDB;
end;

destructor TBerkeleyDBEnv.Destroy;
var
  I: Integer;
  txn: PDB_TXN;
begin
  try
    FDatabasePool := nil;
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
    if Assigned(FAppPrivate) then begin
      FAppPrivate.FHelper := nil;
      FAppPrivate.FMsgLogger.Free;
      Dispose(FAppPrivate);
      FAppPrivate := nil;
    end;
    FreeAndNil(FTxnList);
    FreeAndNil(FCS);
    inherited;
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
      VList.Add('set_flags DB_TXN_NOWAIT on');
      VList.Add('set_flags DB_TXN_WRITE_NOSYNC off');
      VList.Add('set_lg_dir .');
      VList.Add('set_data_dir ..');
      VList.Add('set_cachesize 0 2097152 1');
      VList.Add('set_lg_max 10485760');
      VList.Add('set_lg_bsize 2097152');
      VList.Add('log_set_config DB_LOG_AUTO_REMOVE on');
      VList.SaveToFile(VFile);
    finally
      VList.Free;
    end;
  end;
end;

function TBerkeleyDBEnv.Open: Boolean;
var
  VPath: string;
  VPathUtf8: UTF8String;
begin
  if not FActive and FLibInitOk then begin
    Assert(FAppPrivate <> nil);
    VPath := FAppPrivate.FEnvRootPath + cBerkeleyDBEnvSubDir + PathDelim;
    VPathUtf8 := AnsiToUtf8(VPath);

    if not DirectoryExists(VPath) then begin
      ForceDirectories(VPath);
    end;

    MakeDefConfigFile(VPath);

    CheckBDB(db_env_create(dbenv, 0));
    dbenv.app_private := FAppPrivate;
    dbenv.set_errpfx(dbenv, PAnsiChar(cBerkeleyDBEnvErrPfx));
    dbenv.set_errcall(dbenv, BerkeleyDBErrCall);
    dbenv.set_msgcall(dbenv, BerkeleyDBMsgCall);
    CheckBDB(dbenv.set_alloc(dbenv, @GetMemory, @ReallocMemory, @FreeMemory));

    CheckBDB(dbenv.set_flags(dbenv, DB_TXN_NOWAIT, 1));
    CheckBDB(dbenv.set_flags(dbenv, DB_TXN_WRITE_NOSYNC, 0));
    CheckBDB(dbenv.set_lg_dir(dbenv, '.'));
    CheckBDB(dbenv.set_data_dir(dbenv, '..'));
    CheckBDB(dbenv.set_cachesize(dbenv, 0, 2*1024*1024, 1));

    if FDatabasePageSize <= 4096 then begin
      CheckBDB(dbenv.set_mp_pagesize(dbenv, FDatabasePageSize));
    end else begin
      CheckBDB(dbenv.set_mp_pagesize(dbenv, 4096));
    end;

    CheckBDB(dbenv.set_lg_max(dbenv, 10*1024*1024));
    CheckBDB(dbenv.set_lg_bsize(dbenv, 2*1024*1024));
    CheckBDB(dbenv.log_set_config(dbenv, DB_LOG_AUTO_REMOVE, 1));

    if FIsFullVerboseMode then begin
      CheckBDB(dbenv.set_verbose(dbenv, DB_VERB_DEADLOCK, 1));
      CheckBDB(dbenv.set_verbose(dbenv, DB_VERB_FILEOPS, 1));
      CheckBDB(dbenv.set_verbose(dbenv, DB_VERB_FILEOPS_ALL, 1));
      CheckBDB(dbenv.set_verbose(dbenv, DB_VERB_RECOVERY, 1));
      CheckBDB(dbenv.set_verbose(dbenv, DB_VERB_REGISTER, 1));
      CheckBDB(dbenv.set_verbose(dbenv, DB_VERB_REPLICATION, 1));
      CheckBDB(dbenv.set_verbose(dbenv, DB_VERB_REP_ELECT, 1));
      CheckBDB(dbenv.set_verbose(dbenv, DB_VERB_REP_LEASE, 1));
      CheckBDB(dbenv.set_verbose(dbenv, DB_VERB_REP_MISC, 1));
      CheckBDB(dbenv.set_verbose(dbenv, DB_VERB_REP_MSGS, 1));
      CheckBDB(dbenv.set_verbose(dbenv, DB_VERB_REP_SYNC, 1));
      CheckBDB(dbenv.set_verbose(dbenv, DB_VERB_REP_SYSTEM, 1));
      CheckBDB(dbenv.set_verbose(dbenv, DB_VERB_REPMGR_CONNFAIL, 1));
      CheckBDB(dbenv.set_verbose(dbenv, DB_VERB_REPMGR_MISC, 1));
      CheckBDB(dbenv.set_verbose(dbenv, DB_VERB_WAITSFOR, 1));
    end;

    CheckBDB(
      dbenv.open(
        dbenv,
        PAnsiChar(VPathUtf8),
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

procedure TBerkeleyDBEnv.Sync(out AHotDatabaseCount: Integer);
begin
  TransactionCheckPoint;
  RemoveUnUsedLogs;
  if Assigned(FDatabasePool) then begin
    FDatabasePool.Sync(AHotDatabaseCount);
  end;
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

function TBerkeleyDBEnv.Acquire(const ADatabaseFileName: string): IBerkeleyDB;
begin
  Assert(FDatabasePool <> nil);
  Result := FDatabasePool.Acquire(ADatabaseFileName, Self);
  Assert(Result <> nil);
end;

procedure TBerkeleyDBEnv.Release(const ADatabase: IBerkeleyDB);
begin
  Assert(FDatabasePool <> nil);
  FDatabasePool.Release(ADatabase);
end;

end.

