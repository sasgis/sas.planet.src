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

unit u_InetFunc;

interface

procedure OpenUrlInBrowser(const AUrl: string);
procedure OpenFileInProgram(const AFullFileName: string; const AProgram: string);
procedure OpenFileInDefaultProgram(const AFullFileName: string);

procedure SelectFileInExplorer(const AFullFileName: string);
procedure SelectPathInExplorer(const APath: string);

implementation

uses
  Windows,
  ActiveX,
  ShellAPI,
  SysUtils;

procedure RunShellExecuteEx(
  const AWnd: HWND;
  const AOperation: string;
  const AFileName: string;
  const AParameters: string = '';
  const ADirectory: string = '';
  const AShowCmd: Integer = SW_SHOWNORMAL
);
var
  VExecInfo: TShellExecuteInfo;
  VNeedUninitialize: Boolean;
begin
  Assert(AFileName <> '');

  VNeedUninitialize := SUCCEEDED(CoInitializeEx(nil, COINIT_APARTMENTTHREADED or COINIT_DISABLE_OLE1DDE));
  try
    FillChar(VExecInfo, SizeOf(VExecInfo), 0);

    VExecInfo.cbSize := SizeOf(VExecInfo);
    VExecInfo.Wnd := AWnd;
    VExecInfo.lpVerb := PChar(AOperation);
    VExecInfo.lpFile := PChar(AFileName);
    VExecInfo.lpParameters := PChar(AParameters);
    VExecInfo.lpDirectory := PChar(ADirectory);
    VExecInfo.nShow := AShowCmd;
    VExecInfo.fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;

    {$WARN SYMBOL_PLATFORM OFF}
    Win32Check(ShellExecuteEx(@VExecInfo));
    {$WARN SYMBOL_PLATFORM ON}
  finally
    if VNeedUninitialize then begin
      CoUninitialize;
    end;
  end;
end;

procedure ExecCmdLine(const ACmdLine: string);
var
  SI: TStartupInfo;
  PI: TProcessInformation;
  VCmdLine: string;
begin
  Assert(ACmdLine <> '');

  VCmdLine := ACmdLine;
  UniqueString(VCmdLine);

  FillChar(SI, SizeOf(SI), 0);
  FillChar(PI, SizeOf(PI), 0);
  SI.cb := SizeOf(SI);
  SI.dwFlags := STARTF_USESHOWWINDOW;
  SI.wShowWindow := SW_SHOWNORMAL;

  SetLastError(ERROR_INVALID_PARAMETER);
  {$WARN SYMBOL_PLATFORM OFF}
  Win32Check(CreateProcess(nil, PChar(VCmdLine), nil, nil, False, CREATE_DEFAULT_ERROR_MODE {$IFDEF UNICODE}or CREATE_UNICODE_ENVIRONMENT{$ENDIF}, nil, nil, SI, PI));
  {$WARN SYMBOL_PLATFORM ON}
  CloseHandle(PI.hThread);
  CloseHandle(PI.hProcess);
end;

procedure OpenUrlInBrowser(const AUrl: string);
begin
  Assert(AUrl <> '');
  RunShellExecuteEx(0, '', AUrl);
end;

procedure OpenFileInProgram(const AFullFileName: string; const AProgram: string);
begin
  Assert(AFullFileName <> '');
  Assert(AProgram <> '');
  RunShellExecuteEx(0, '', AProgram, AFullFileName);
end;

procedure OpenFileInDefaultProgram(const AFullFileName: string);
begin
  Assert(AFullFileName <> '');
  RunShellExecuteEx(0, '', AFullFileName);
end;

procedure SelectFileInExplorer(const AFullFileName: string);
begin
  Assert(AFullFileName <> '');
  ExecCmdLine('explorer /select,' + AFullFileName);
end;

procedure SelectPathInExplorer(const APath: string);
begin
  Assert(APath <> '');
  ExecCmdLine('explorer /root,' + APath);
end;

end.
