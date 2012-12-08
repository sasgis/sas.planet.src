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
  u_BerkeleyDBPool;

type
  TBerkeleyDBEnv = class(TObject)
  private
    FEnv: PDB_ENV;
    FPool: TBerkeleyDBPool;
    FActive: Boolean;
    FLibInitOk: Boolean;
    FLastRemoveLogTime: Cardinal;
    FEnvRootPath: string;
    FCS: TCriticalSection;
    FClientsCount: Integer;
    FSingleMode: Boolean;

    function Open: Boolean;
    function GetEnv: PDB_ENV;
  public
    constructor Create(const AEnvRootPath: string; ASingleMode: Boolean);
    destructor Destroy; override;
    procedure RemoveUnUsedLogs;
    procedure CheckPoint(Sender: TObject);
    class function LsnReset(const AFileName: string): Boolean;

    property EnvPtr: PDB_ENV read GetEnv;
    property EnvRootPath: string read FEnvRootPath;
    property Pool: TBerkeleyDBPool read FPool;
    property ClientsCount: Integer read FClientsCount write FClientsCount;
  end;

function GlobalAllocateEnvironment(const AEnvRootPath: string; ASingleMode: Boolean): TBerkeleyDBEnv;
function GlobalFreeAndNilEnvironment(var AEnv: TBerkeleyDBEnv): Boolean;

const
  CEnvSubDir = 'env';
  CBerkeleyDBEnvErrPfx = 'BerkeleyDB (Env)';

implementation

uses
  Windows,
  Contnrs,
  SysUtils,
  ShLwApi,
  u_BerkeleyDBErrorHandler;

var
  GEnvList: TObjectList = nil;
  GCS: TCriticalSection = nil;

function GetFullPathName(const ARelativePathName: string): string;
begin
  SetLength(Result, MAX_PATH);
  PathCombine(@Result[1], PChar(ExtractFilePath(ParamStr(0))), PChar(ARelativePathName));
  SetLength(Result, StrLen(PChar(Result)));
  Result := LowerCase(IncludeTrailingPathDelimiter(Result));
end;

function GlobalAllocateEnvironment(
  const AEnvRootPath: string;
  ASingleMode: Boolean
): TBerkeleyDBEnv;
var
  I: Integer;
  VPath: string;
  VEnv: TBerkeleyDBEnv;
begin
  Result := nil;
  GCS.Acquire;
  try
    VPath := GetFullPathName(AEnvRootPath);
    for I := 0 to GEnvList.Count - 1 do begin
      VEnv := TBerkeleyDBEnv(GEnvList.Items[I]);
      if Assigned(VEnv) then begin
        if VEnv.EnvRootPath = VPath then begin
          Result := VEnv;
          Break;
        end;
      end;
    end;
    if not Assigned(Result) then begin
      VEnv := TBerkeleyDBEnv.Create(VPath, ASingleMode);
      GEnvList.Add(VEnv);
      Result := VEnv;
    end;
    if Assigned(Result) then begin
      Result.ClientsCount := Result.ClientsCount + 1;
    end;
  finally
    GCS.Release;
  end;
end;

function GlobalFreeAndNilEnvironment(var AEnv: TBerkeleyDBEnv): Boolean;
var
  I: Integer;
  VEnv: TBerkeleyDBEnv;
begin
  Result := False;
  GCS.Acquire;
  try
    if Assigned(AEnv) then begin
      for I := 0 to GEnvList.Count - 1 do begin
        VEnv := TBerkeleyDBEnv(GEnvList.Items[I]);
        if Assigned(VEnv) then begin
          if VEnv = AEnv then begin
            VEnv.ClientsCount := VEnv.ClientsCount - 1;
            if VEnv.ClientsCount <= 0 then begin
              GEnvList.Remove(VEnv);
              GEnvList.Pack;
              AEnv := nil;
              Result := True;
            end;
            Break;
          end;
        end;
      end;
    end;
  finally
    GCS.Release;
  end;
end;

{ TBerkeleyDBEnv }

constructor TBerkeleyDBEnv.Create(
  const AEnvRootPath: string;
  ASingleMode: Boolean
);
begin
  inherited Create;
  FCS := TCriticalSection.Create;
  FActive := False;
  FLastRemoveLogTime := 0;
  FEnvRootPath := AEnvRootPath;
  FSingleMode := ASingleMode;
  FClientsCount := 1;
  FPool := TBerkeleyDBPool.Create;
  FLibInitOk := InitBerkeleyDB;
end;

destructor TBerkeleyDBEnv.Destroy;
begin
  FPool.Free;
  if FEnv <> nil then begin
    CheckPoint(Self);
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
  VSingleModFlag: Cardinal;
begin
  if not FActive and FLibInitOk then begin
    CheckBDB(db_env_create(FEnv, 0));
    FEnv.set_errpfx(FEnv, CBerkeleyDBEnvErrPfx);
    FEnv.set_errcall(FEnv, BDBErrCall);
    CheckBDB(FEnv.set_alloc(FEnv, @GetMemory, @ReallocMemory, @FreeMemory));
    CheckBDB(FEnv.set_flags(FEnv, DB_TXN_NOSYNC, 1));
    CheckBDB(FEnv.set_flags(FEnv, DB_TXN_WRITE_NOSYNC, 1));
    CheckBDB(FEnv.set_verbose(FEnv, DB_VERB_RECOVERY, 1));
    if FSingleMode then begin
      VPath := '';
      VSingleModFlag := DB_PRIVATE or DB_SYSTEM_MEM;
      CheckBDB(FEnv.log_set_config(FEnv, DB_LOG_IN_MEMORY, 1));
      CheckBDB(FEnv.set_lg_bsize(FEnv, 10*1024*1024));
    end else begin
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
      VSingleModFlag := DB_REGISTER;
      CheckBDB(FEnv.log_set_config(FEnv, DB_LOG_AUTO_REMOVE, 1));
    end;

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
        VSingleModFlag or
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
    Assert(False, 'Warning [BerkeleyDB]: LsnReset - invalid arguments!');
  end;
end;

initialization
  GCS := TCriticalSection.Create;
  GEnvList := TObjectList.Create(True);

finalization
  FreeAndNil(GEnvList);
  FreeAndNil(GCS);

end.

