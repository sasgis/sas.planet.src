{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_MarkSystemORMLog;

{$INCLUDE MarkSystemORM.inc}

interface

procedure InitSQLLog;

{$IFDEF SQL_LOG_ENABLE}
function SQLLogEnter(const AInstance: TObject; const AMethod: string = ''): IInterface;

procedure SQLLogDebug(const AMsgFmt: string; const AMsgArgs: array of const;
  const AInstance: TObject = nil);

procedure SQLLogCache(const AMsgFmt: string; const AMsgArgs: array of const;
  const AInstance: TObject = nil);

procedure SQLLogInfo(const AMsgFmt: string; const AMsgArgs: array of const;
  const AInstance: TObject = nil);
{$ENDIF}

implementation

{$IFDEF SQL_LOG_ENABLE}
uses
  mORMot,
  SynLog,
  SynCommons;

procedure InitSQLLog;
begin
  with TSQLLog.Family do begin
    {$IFDEF SQL_LOG_VERBOSE}
    Level := LOG_VERBOSE;
    {$ELSE}
    Level := [sllSQL, sllInfo] + LOG_STACKTRACE;
    {$ENDIF}
    HighResolutionTimeStamp := True;
    PerThreadLog := ptIdentifiedInOnFile;
  end;
end;

function SQLLogEnter(const AInstance: TObject; const AMethod: string): IInterface;
begin
  if AMethod <> '' then begin
    Result := TSQLLog.Enter(AInstance, PUTF8Char(StringToUtf8(AMethod)));
  end else begin
    Result := TSQLLog.Enter(AInstance);
  end;
end;

procedure SQLLogDebug(const AMsgFmt: string; const AMsgArgs: array of const;
  const AInstance: TObject);
begin
  TSQLLog.Add.Log(sllDebug, StringToUtf8(AMsgFmt), AMsgArgs, AInstance);
end;

procedure SQLLogCache(const AMsgFmt: string; const AMsgArgs: array of const;
  const AInstance: TObject);
begin
  TSQLLog.Add.Log(sllCache, StringToUtf8(AMsgFmt), AMsgArgs, AInstance);
end;

procedure SQLLogInfo(const AMsgFmt: string; const AMsgArgs: array of const;
  const AInstance: TObject);
begin
  TSQLLog.Add.Log(sllInfo, StringToUtf8(AMsgFmt), AMsgArgs, AInstance);
end;

{$ELSE}

procedure InitSQLLog;
begin
end;

{$ENDIF}

end.
