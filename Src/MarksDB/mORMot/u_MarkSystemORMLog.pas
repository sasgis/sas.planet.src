{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_MarkSystemORMLog;

{$INCLUDE MarkSystemORM.inc}

interface

procedure InitOrmLog; {$IFNDEF ORM_LOG_ENABLE} inline; {$ENDIF}

{$IFDEF ORM_LOG_ENABLE}
function OrmLogEnter(const AInstance: TObject; const AMethod: UTF8String = ''): IInterface;
procedure OrmLogDebug(const AMsgFmt: UTF8String; const AMsgArgs: array of const; const AInstance: TObject = nil);
procedure OrmLogCache(const AMsgFmt: UTF8String; const AMsgArgs: array of const; const AInstance: TObject = nil);
procedure OrmLogInfo(const AMsgFmt: UTF8String; const AMsgArgs: array of const; const AInstance: TObject = nil);
{$ENDIF}

implementation

{$IFDEF ORM_LOG_ENABLE}
uses
  SysUtils,
  mormot.core.base,
  mormot.core.unicode,
  mormot.core.log,
  mormot.core.rtti,
  u_DebugLogger;

procedure InitOrmLog;
var
  VLogPath: string;
begin
  VLogPath := TLog.BasePath + 'marks\';

  if not ForceDirectories(VLogPath) then begin
    RaiseLastOSError;
  end;

  with TSynLog.Family do begin
    {$IFDEF ORM_LOG_VERBOSE}
    Level := LOG_VERBOSE;
    {$ELSE}
    Level := [sllSQL, sllInfo] + LOG_STACKTRACE;
    {$ENDIF}
    HighResolutionTimeStamp := True;
    PerThreadLog := ptIdentifiedInOnFile;
    DestinationPath := VLogPath;
    CustomFileName := 'mormot';
    DefaultExtension := '.txt';
  end;
end;

function OrmLogEnter(const AInstance: TObject; const AMethod: UTF8String): IInterface;
begin
  if AMethod <> '' then begin
    Result := TSynLog.Enter(AInstance, PUtf8Char(AMethod));
  end else begin
    Result := TSynLog.Enter(AInstance);
  end;
end;

procedure OrmLogDebug(const AMsgFmt: UTF8String; const AMsgArgs: array of const; const AInstance: TObject);
begin
  TSynLog.Add.Log(sllDebug, PUtf8Char(AMsgFmt), AMsgArgs, AInstance);
end;

procedure OrmLogCache(const AMsgFmt: UTF8String; const AMsgArgs: array of const; const AInstance: TObject);
begin
  TSynLog.Add.Log(sllCache, PUtf8Char(AMsgFmt), AMsgArgs, AInstance);
end;

procedure OrmLogInfo(const AMsgFmt: UTF8String; const AMsgArgs: array of const; const AInstance: TObject);
begin
  TSynLog.Add.Log(sllInfo, PUtf8Char(AMsgFmt), AMsgArgs, AInstance);
end;
{$ELSE}
procedure InitOrmLog;
begin
  //
end;
{$ENDIF}

end.
