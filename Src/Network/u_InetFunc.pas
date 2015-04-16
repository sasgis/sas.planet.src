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
procedure OpenFileInDefaultProgram(const AFullFileName: string);
procedure SelectFileInExplorer(const AFullFileName: String);
procedure SelectPathInExplorer(const APath: string);

function IsGZipped(const AHeader: AnsiString): Boolean;

implementation

uses
  Windows,
  ActiveX,
  ShellAPI,
  SysUtils,
  ALString;

procedure ShellExecute(const AWnd: HWND; const AOperation, AFileName: String; const AParameters: String = ''; const ADirectory: String = ''; const AShowCmd: Integer = SW_SHOWNORMAL);
var
  ExecInfo: TShellExecuteInfo;
  NeedUninitialize: Boolean;
begin
  Assert(AFileName <> '');

  NeedUninitialize := SUCCEEDED(CoInitializeEx(nil, COINIT_APARTMENTTHREADED or COINIT_DISABLE_OLE1DDE));
  try
    FillChar(ExecInfo, SizeOf(ExecInfo), 0);
    ExecInfo.cbSize := SizeOf(ExecInfo);

    ExecInfo.Wnd := AWnd;
    ExecInfo.lpVerb := Pointer(AOperation);
    ExecInfo.lpFile := PChar(AFileName);
    ExecInfo.lpParameters := Pointer(AParameters);
    ExecInfo.lpDirectory := Pointer(ADirectory);
    ExecInfo.nShow := AShowCmd;
    ExecInfo.fMask := SEE_MASK_FLAG_DDEWAIT //SEE_MASK_NOASYNC { = SEE_MASK_FLAG_DDEWAIT для старых версий Delphi }
                   or SEE_MASK_FLAG_NO_UI;

    {$WARN SYMBOL_PLATFORM OFF}
    Win32Check(ShellExecuteEx(@ExecInfo));
    {$WARN SYMBOL_PLATFORM ON}
  finally
    if NeedUninitialize then
      CoUninitialize;
  end;
end;

procedure ExecCmdLine(const ACmdLine: string);
const
  ACmdShow: UINT = SW_SHOWNORMAL;
var
  SI: TStartupInfo;
  PI: TProcessInformation;
  CmdLine: string;
begin
  Assert(ACmdLine <> '');

  CmdLine := ACmdLine;

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

procedure OpenUrlInBrowser(const URL: string);
begin
  Assert(URL <> '');
  ShellExecute(0, '', URL);
end;

procedure OpenFileInDefaultProgram(const AFullFileName: string);
begin
  Assert(AFullFileName <> '');
  ShellExecute(0, '', AFullFileName);
end;

procedure SelectFileInExplorer(const AFullFileName: String);
begin
  Assert(AFullFileName <> '');
  ExecCmdLine('explorer /select,' + AFullFileName);
end;

procedure SelectPathInExplorer(const APath: string);
begin
  Assert(APath <> '');
  ExecCmdLine('explorer /root,' + APath);
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
