{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2021, SAS.Planet development team.                      *}
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

unit u_PascalScriptLogger;

interface

uses
  Classes,
  SyncObjs,
  i_PascalScriptLogger,
  u_BaseInterfacedObject;

type
  TPascalScriptLogger = class(TBaseInterfacedObject, IPascalScriptLogger)
  private
    FFileName: string;
    FStream: TFileStream;
    FLock: TCriticalSection;
    procedure LazyInit;
    function GetUniqId(const AAppId: Integer): string;
    function GetFormattedStr(const AStr: AnsiString): AnsiString;
  private
    { IPascalScriptLogger }
    procedure Write(const AStr: AnsiString);
    procedure WriteFmt(const AFormat: string; const AArgs: array of const);
  public
    constructor Create(
      const AAppId: Integer;
      const ALogsPath: string;
      const AZmpFileName: string
    );
    destructor Destroy; override;
  end;

implementation

uses
  Windows,
  SysUtils;

{ TPascalScriptLogger }

constructor TPascalScriptLogger.Create(
  const AAppId: Integer;
  const ALogsPath: string;
  const AZmpFileName: string
);
begin
  Assert(ALogsPath <> '');
  Assert(AZmpFileName <> '');

  inherited Create;

  FLock := TCriticalSection.Create;
  FStream := nil;

  FFileName :=
    IncludeTrailingPathDelimiter(ALogsPath) + 'zmp\' +
    ChangeFileExt(ExtractFileName(AZmpFileName), GetUniqId(AAppId) + '.log');
end;

destructor TPascalScriptLogger.Destroy;
begin
  FreeAndNil(FStream);
  FreeAndNil(FLock);

  inherited Destroy;
end;

function TPascalScriptLogger.GetUniqId(const AAppId: Integer): string;
begin
  if AAppId = 1 then begin
    Result := '';
  end else begin
    Result := '.' + IntToStr(AAppId);
  end;
end;

procedure TPascalScriptLogger.LazyInit;
var
  VDir: string;
begin
  if FStream = nil then begin
    VDir := ExtractFileDir(FFileName);
    if not DirectoryExists(VDir) then begin
      if not ForceDirectories(VDir) then begin
        RaiseLastOSError;
      end;
    end;
    FStream := TFileStream.Create(FFileName, fmCreate or fmShareDenyWrite);
  end;
end;

function TPascalScriptLogger.GetFormattedStr(const AStr: AnsiString): AnsiString;
begin
  Result := AnsiString(Format(
    '[%.8d] %s: %s' + #13#10,
    [GetCurrentThreadId, FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now), AStr]
  ));
end;

procedure TPascalScriptLogger.Write(const AStr: AnsiString);
var
  VStr: AnsiString;
begin
  if AStr = '' then begin
    Exit;
  end;

  FLock.Acquire;
  try
    LazyInit;
    VStr := GetFormattedStr(AStr);
    FStream.WriteBuffer(VStr[1], Length(VStr));
  finally
    FLock.Release;
  end;
end;

procedure TPascalScriptLogger.WriteFmt(const AFormat: string; const AArgs: array of const);
begin
  Self.Write( AnsiString(SysUtils.Format(AFormat, AArgs)) );
end;

end.
