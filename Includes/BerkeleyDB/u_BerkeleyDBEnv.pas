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

unit u_BerkeleyDBEnv;

interface

uses
  SyncObjs,
  db_h;

type
  TBerkeleyDBEnv = class(TObject)
  private
    FEnv: PDB_ENV;
    FActive: Boolean;
    FLastRemoveLogTime: Cardinal;
    FLastCheckPointTime: Cardinal;
    FEnvRootPath: string;
    FCS: TCriticalSection;
    function Open: Boolean;
    function GetEnv: PDB_ENV;
  public
    constructor Create(const AEnvRootPath: string);
    destructor Destroy; override;
    procedure RemoveUnUsedLogs;
    procedure CheckPoint;
    property EnvPtr: PDB_ENV read GetEnv;
    property EnvRootPath: string read FEnvRootPath;
  end;

function GlobalAllocateEnvironment(const AEnvRootPath: string): TBerkeleyDBEnv;

implementation

uses
  Windows,
  Contnrs,
  SysUtils;

const
  CEnvSubDir = 'env';

var
  GEnvList: TObjectList = nil;
  GCS: TCriticalSection = nil;

function GlobalAllocateEnvironment(const AEnvRootPath: string): TBerkeleyDBEnv;
var
  I: Integer;
  VEnv: TBerkeleyDBEnv;
begin
  Result := nil;
  GCS.Acquire;
  try
    for I := 0 to GEnvList.Count - 1 do begin
      VEnv := TBerkeleyDBEnv(GEnvList.Items[I]);
      if Assigned(VEnv) then begin
        if VEnv.EnvRootPath = AEnvRootPath then begin
          Result := VEnv;
          Break;
        end;
      end;
    end;
    if not Assigned(Result) then begin
      VEnv := TBerkeleyDBEnv.Create(AEnvRootPath);
      GEnvList.Add(VEnv);
      Result := VEnv;
    end;
  finally
    GCS.Release;
  end;
end;

procedure ErrCall(dbenv: PDB_ENV; errpfx, msg: PAnsiChar); cdecl;
begin
  raise Exception.Create(errpfx + ': ' + msg);
end;

{ TBerkeleyDBEnv }

constructor TBerkeleyDBEnv.Create(const AEnvRootPath: string);
begin
  inherited Create;
  FCS := TCriticalSection.Create;
  FActive := False;
  FLastRemoveLogTime := 0;
  FLastCheckPointTime := 0;
  FEnvRootPath := AEnvRootPath;
  InitBerkeleyDB;
end;

destructor TBerkeleyDBEnv.Destroy;
begin
  if FEnv <> nil then begin
    CheckPoint;
    RemoveUnUsedLogs;
    CheckBDBandNil(FEnv.close(FEnv, 0), FEnv);
  end;
  FCS.Free;
  inherited Destroy;
end;

function TBerkeleyDBEnv.Open: Boolean;
var
  I: Integer;
  VPath: string;
begin
  if not FActive then begin
    CheckBDB(db_env_create(FEnv, 0));
    CheckBDB(FEnv.set_alloc(FEnv, @GetMemory, @ReallocMemory, @FreeMemory));
    CheckBDB(FEnv.set_cache_max(FEnv, 0, 8*1024*1024));
    CheckBDB(FEnv.set_flags(FEnv, DB_TXN_NOSYNC, 1));
    CheckBDB(FEnv.set_flags(FEnv, DB_TXN_WRITE_NOSYNC, 1));
    if CEnvSubDir <> '' then begin
      VPath := FEnvRootPath + CEnvSubDir + PathDelim;
      I := LastDelimiter(PathDelim, VPath);
      VPath := copy(VPath, 1, I);
      CheckBDB(FEnv.set_data_dir(FEnv, '..'));
    end else begin
      VPath := FEnvRootPath;
    end;
    if not DirectoryExists(VPath) then begin
      ForceDirectories(VPath);
    end;
    CheckBDB(FEnv.log_set_config(FEnv, DB_LOG_AUTO_REMOVE, 1));
    CheckBDB(FEnv.set_verbose(FEnv, DB_VERB_RECOVERY, 1));
    FEnv.set_errpfx(FEnv, 'BerkeleyDB (Env)');
    FEnv.set_errcall(FEnv,ErrCall);

    CheckBDB(
      FEnv.open(
        FEnv,
        PAnsiChar(VPath),
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
  Result := FActive;
end;

procedure TBerkeleyDBEnv.RemoveUnUsedLogs;
begin
  FCS.Acquire;
  try
    if FActive and (GetTickCount - FLastRemoveLogTime > 30000) then begin
      FLastRemoveLogTime := GetTickCount;
      CheckBDB(FEnv.log_archive(FEnv, nil, DB_ARCH_REMOVE));
    end;
  finally
    FCS.Release;
  end;
end;

procedure TBerkeleyDBEnv.CheckPoint;
begin
  FCS.Acquire;
  try
    if FActive and (GetTickCount - FLastCheckPointTime > 30000) then begin
      FLastCheckPointTime := GetTickCount;
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

initialization
  GCS := TCriticalSection.Create;
  GEnvList := TObjectList.Create(True);

finalization
  FreeAndNil(GEnvList);
  FreeAndNil(GCS);

end.

