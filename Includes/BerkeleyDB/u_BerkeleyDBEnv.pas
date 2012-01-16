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
    FEnvRootPath: string;
    FCS: TCriticalSection;
    function Open: Boolean;
    function GetEnv: PDB_ENV;
  public
    constructor Create(const AEnvRootPath: string);
    destructor Destroy; override;
    procedure RemoveUnUsedLogs;
    property EnvPtr: PDB_ENV read GetEnv;
    property EnvRootPath: string read FEnvRootPath;
  end;

implementation

uses
  SysUtils;

const
  CEnvSubDir = 'env';

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
  FEnvRootPath := AEnvRootPath;
  InitBerkeleyDB;
end;

destructor TBerkeleyDBEnv.Destroy;
begin
  if FEnv <> nil then begin
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
    {$IFDEF DEBUG}
    CheckBDB(FEnv.set_verbose(FEnv, DB_VERB_RECOVERY, 1));
    FEnv.set_errpfx(FEnv, 'BerkeleyDB (Env)');
    FEnv.set_errcall(FEnv,ErrCall);
    {$ENDIF}
    CheckBDB(
      FEnv.open(
        FEnv,
        PAnsiChar(VPath),
        DB_CREATE_ or
        DB_RECOVER or
        DB_INIT_LOCK or
        DB_INIT_LOG or
        DB_INIT_MPOOL or
        DB_INIT_TXN,
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
    if FActive then begin
      // Удаляя таким образом логи, мы автоматически лишаемся 
      // возможности catastrophic restore.
      // По хорошему, логи нужно складывать в бэкап архив.
      CheckBDB(FEnv.log_archive(FEnv, nil, DB_ARCH_REMOVE));
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

end.

