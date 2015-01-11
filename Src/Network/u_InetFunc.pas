{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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

unit u_InetFunc;

interface

procedure OpenUrlInBrowser(const URL: string);
procedure SelectFileInExplorer(const AFullFileName: String);

function IsGZipped(const AHeader: AnsiString): Boolean;

implementation

uses
  Windows,
  ShellAPI,
  SysUtils,
  ALString;

procedure OpenUrlInBrowser(const URL: string);
begin
  ShellExecute(0, nil, PChar(URL), nil, nil, SW_RESTORE);
end;

procedure SelectFileInExplorer(const AFullFileName: String);
const
  ACmdShow: UINT = SW_SHOWNORMAL;
var
  SI: TStartupInfo;
  PI: TProcessInformation;
  CmdLine: String;
begin
  Assert(AFullFileName <> '');

  CmdLine := 'explorer /select,' + AFullFileName;
  UniqueString(CmdLine);

  FillChar(SI, SizeOf(SI), 0);
  FillChar(PI, SizeOf(PI), 0);
  SI.cb := SizeOf(SI);
  SI.dwFlags := STARTF_USESHOWWINDOW;
  SI.wShowWindow := ACmdShow;

  SetLastError(ERROR_INVALID_PARAMETER);
  {$WARN SYMBOL_PLATFORM OFF}
  Win32Check(CreateProcess(nil, PChar(CmdLine), nil, nil, False, CREATE_DEFAULT_ERROR_MODE {$IFDEF UNICODE}or CREATE_UNICODE_ENVIRONMENT{$ENDIF}, nil, nil, SI, PI));
  {$WARN SYMBOL_PLATFORM ON}
  CloseHandle(PI.hThread);
  CloseHandle(PI.hProcess);
end;

function IsGZipped(const AHeader: AnsiString): Boolean;
const
  c_Content = 'Content-Encoding';
  c_GZIPped = 'gzip';
var
  VPos: Integer;
  VTxt: AnsiString;
begin
  Result := False;
  VPos := ALPos(c_Content, AHeader);
  if (VPos > 0) then begin
    // skip before
    VPos := VPos + Length(c_Content) + 1;
    while (VPos <= Length(AHeader)) and (AHeader[VPos] in [#32, #10, #13, #160, ':']) do begin
      Inc(VPos);
    end;
    VTxt := '';
    while (VPos <= Length(AHeader)) and (not (AHeader[VPos] in [#32, #10, #13, #160, ':'])) do begin
      VTxt := VTxt + AHeader[VPos];
      Inc(VPos);
    end;
    Result := (VTxt = c_GZIPped);
  end;
end;

end.
