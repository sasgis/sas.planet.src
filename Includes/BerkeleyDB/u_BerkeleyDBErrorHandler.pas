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

unit u_BerkeleyDBErrorHandler;

interface

uses
  libdb51;

procedure BDBRaiseException(const EMsg: string);
procedure BDBErrCall(dbenv: PDB_ENV; errpfx, msg: PAnsiChar); cdecl;

implementation

uses
  Classes,
  SysUtils,
  SyncObjs,
  u_GlobalState;

const
  cLogEnabled = {$IFDEF DEBUG} True {$ELSE} False {$ENDIF};

var
  GCS: TCriticalSection = nil;
  GDebugLogFileStream: TFileStream = nil;

procedure InitHandler;
begin
  if cLogEnabled then begin
    GCS := TCriticalSection.Create;
  end;
end;

procedure FinHandler;
begin
  if cLogEnabled then begin
    GDebugLogFileStream.Free;
    GCS.Free;
  end;
end;

procedure WriteErrorToLog(const AMsg: string);
var
  VLogMsg: string;
  VLogFileName: string;
begin
  GCS.Acquire;
  try
    if not Assigned(GDebugLogFileStream) then begin
      VLogFileName := GState.CacheConfig.BDBCachepath + '\sdb.log';
      if not FileExists(VLogFileName) then begin
        GDebugLogFileStream := TFileStream.Create(VLogFileName, fmCreate);
        GDebugLogFileStream.Free;
      end;
      GDebugLogFileStream := TFileStream.Create(VLogFileName, fmOpenReadWrite or fmShareDenyNone);
    end;

    DateTimeToString(VLogMsg, 'dd-mm-yyyy  hh:nn:ss.zzzz', Now);
    VLogMsg := VLogMsg + '  ' + AMsg + #13#10;

    GDebugLogFileStream.Position := GDebugLogFileStream.Size;
    GDebugLogFileStream.Write(VLogMsg[1], Length(VLogMsg));
  finally
    GCS.Release;
  end;
end;

procedure BDBErrCall(dbenv: PDB_ENV; errpfx, msg: PAnsiChar); cdecl;
var
  VMsg: string;
  VEnvHome: PAnsiChar;
begin
  VMsg := errpfx + ': ' + msg;
  if cLogEnabled then
  try
    dbenv.get_home(dbenv, @VEnvHome);
    WriteErrorToLog(VMsg + ' (' + VEnvHome + ')');
  except
  end;
  raise EBerkeleyDBExeption.Create(VMsg);
end;

procedure BDBRaiseException(const EMsg: string);
begin
  if cLogEnabled then
  try
    WriteErrorToLog(EMsg);
  except
  end;
  raise EBerkeleyDBExeption.Create(EMsg);
end;

initialization
  InitHandler;

finalization
  FinHandler;

end.

