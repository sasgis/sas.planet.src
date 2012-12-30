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
  SyncObjs,
  libdb51,
  i_GlobalBerkeleyDBHelper,
  u_BerkeleyDBPool;

type
  TBerkeleyDBEnvAppPrivate = record
    EnvRootPath: string;
    Helper: IGlobalBerkeleyDBHelper;
  end;
  PBerkeleyDBEnvAppPrivate = ^TBerkeleyDBEnvAppPrivate;

  TBerkeleyDBEnv = class(TObject)
  private
    FEnv: PDB_ENV;
    FPool: TBerkeleyDBPool;
    FAppPrivate: PBerkeleyDBEnvAppPrivate;
    FActive: Boolean;
    FLibInitOk: Boolean;
    FLastRemoveLogTime: Cardinal;
    FCS: TCriticalSection;
    FClientsCount: Integer;

    function Open: Boolean;
    function GetEnv: PDB_ENV;
    function GetEnvRootPath: string;
  public
    constructor Create(
      const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
      const AEnvRootPath: string
    );
    destructor Destroy; override;
    procedure RemoveUnUsedLogs;
    procedure CheckPoint(Sender: TObject);
    class function LsnReset(const AFileName: string): Boolean;

    property EnvPtr: PDB_ENV read GetEnv;
    property EnvRootPath: string read GetEnvRootPath;
    property Pool: TBerkeleyDBPool read FPool;
    property ClientsCount: Integer read FClientsCount write FClientsCount;
  end;

const
  CEnvSubDir = 'env';
  CBerkeleyDBEnvErrPfx = 'BerkeleyDB (Env)';

implementation

uses
  Windows,  
  SysUtils,
  u_GlobalBerkeleyDBHelper;

{ TBerkeleyDBEnv }

constructor TBerkeleyDBEnv.Create(
  const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
  const AEnvRootPath: string
);
begin
  inherited Create;
  New(FAppPrivate);
  FAppPrivate.EnvRootPath := AEnvRootPath;
  FAppPrivate.Helper := AGlobalBerkeleyDBHelper;
  FCS := TCriticalSection.Create;
  FActive := False;
  FLastRemoveLogTime := 0;
  FClientsCount := 0;
  FPool := TBerkeleyDBPool.Create(FAppPrivate.Helper, 12);
  FLibInitOk := InitBerkeleyDB;
end;

destructor TBerkeleyDBEnv.Destroy;
begin
  try
    FPool.Free;
    if FEnv <> nil then begin
      CheckPoint(Self);
      RemoveUnUsedLogs;
      CheckBDBandNil(FEnv.close(FEnv, 0), FEnv);
    end;
    FCS.Free;
  finally
    FAppPrivate.Helper := nil;
    Dispose(FAppPrivate);
    inherited Destroy;
  end;
end;

function TBerkeleyDBEnv.GetEnvRootPath: string;
begin
  if Assigned(FAppPrivate) then begin
    Result := FAppPrivate.EnvRootPath;
  end else begin
    Result := '';
  end;
end;

function TBerkeleyDBEnv.Open: Boolean;
var
  I: Integer;
  VPath: AnsiString;
begin
  if not FActive and FLibInitOk then begin
    CheckBDB(db_env_create(FEnv, 0));

    FEnv.app_private := FAppPrivate;
    FEnv.set_errpfx(FEnv, CBerkeleyDBEnvErrPfx);
    FEnv.set_errcall(FEnv, BerkeleyDBErrCall);
    CheckBDB(FEnv.set_alloc(FEnv, @GetMemory, @ReallocMemory, @FreeMemory));
    CheckBDB(FEnv.set_flags(FEnv, DB_TXN_NOSYNC, 1));
    CheckBDB(FEnv.set_flags(FEnv, DB_TXN_WRITE_NOSYNC, 1));
    CheckBDB(FEnv.set_verbose(FEnv, DB_VERB_RECOVERY, 1));

    if CEnvSubDir <> '' then begin
      VPath := FAppPrivate.EnvRootPath + CEnvSubDir + PathDelim;
      I := LastDelimiter(PathDelim, VPath);
      VPath := copy(VPath, 1, I);
      CheckBDB(FEnv.set_data_dir(FEnv, '..'));
    end else begin
      VPath := FAppPrivate.EnvRootPath;
    end;
    if not DirectoryExists(VPath) then begin
      ForceDirectories(VPath);
    end;
    CheckBDB(FEnv.log_set_config(FEnv, DB_LOG_AUTO_REMOVE, 1));

    CheckBDB(
      FEnv.open(
        FEnv,
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
      CheckBDB(FEnv.log_archive(FEnv, nil, DB_ARCH_REMOVE));
    end;
  finally
    FCS.Release;
  end;
end;

procedure TBerkeleyDBEnv.CheckPoint(Sender: TObject);
begin
  FCS.Acquire;
  try
    if FActive and FLibInitOk then begin
      CheckBDB(FEnv.txn_checkpoint(FEnv, 0, 0, DB_FORCE));
    end;
  finally
    FCS.Release;
  end;
end;

function TBerkeleyDBEnv.GetEnv: PDB_ENV;
begin
  FCS.Acquire;
  try
    if Open then begin
      Result := FEnv;
    end else begin
      Result := nil;
    end;
  finally
    FCS.Release;
  end;
end;

class function TBerkeleyDBEnv.LsnReset(const AFileName: string): Boolean;
var
  env: PDB_ENV;
begin
  Result := False;
  if (AFileName <> '') and FileExists(AFileName) then begin
    CheckBDB(db_env_create(env, 0));
    try
      CheckBDB(env.open(env, '', DB_CREATE_ or DB_PRIVATE or DB_INIT_MPOOL, 0));
      CheckBDB(env.lsn_reset(env, PAnsiChar(AFileName), 0));
    finally
      if env <> nil then begin
        CheckBDB(env.close(env, 0));
      end;
    end;
    Result := True;
  end else begin
    Assert(False, 'Warning [BerkeleyDB Env]: LsnReset - invalid arguments!');
  end;
end;

end.

