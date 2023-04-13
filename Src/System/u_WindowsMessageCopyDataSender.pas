{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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

unit u_WindowsMessageCopyDataSender;

interface

type
  TWindowsMessageCopyDataSender = record
    class function SendArgs(const AArgs: AnsiString): Boolean; static;
    class function SendCmdLineArgs: Boolean; static;
  end;

implementation

uses
  Windows,
  Messages,
  u_CmdLineArgProcessorAPI;

{ TWindowsMessageCopyDataSender }

type
  TFNWndEnumProc = function(hWnd: HWND; lParam: LPARAM): BOOL; stdcall;

function EnumWindows(lpEnumFunc: TFNWndEnumProc; lParam: LPARAM): BOOL; stdcall; external  user32;

function SendMessageTimeoutW(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM;
  fuFlags, uTimeout: UINT; lpdwResult: PDWORD_PTR): LRESULT; stdcall; external user32;

function EnumWindowsProc(hWnd: HWND; lParam: LPARAM): BOOL; stdcall;
var
  VOutVal: DWORD_PTR;
  VRetVal: LRESULT;
begin
  VRetVal := SendMessageTimeoutW(hWnd, WM_FRIEND_OR_FOE, 0, 0, SMTO_BLOCK or SMTO_ABORTIFHUNG, 200, @VOutVal);
  if (VRetVal <> 0) and (VOutVal = WM_FRIEND_OR_FOE) then begin
    PHandle(lParam)^ := hWnd; // Save SASPlanet window handle
    Result := False; // Stop enumeration
  end else begin
    Result := True; // Continue
  end;
end;

function GetSasPlanetWindowHandle(out AHandle: HWND): Boolean;
begin
  AHandle := INVALID_HANDLE_VALUE;
  EnumWindows(EnumWindowsProc, LPARAM(@AHandle));

  Result := AHandle <> INVALID_HANDLE_VALUE;
end;

class function TWindowsMessageCopyDataSender.SendArgs(const AArgs: AnsiString): Boolean;
var
  VHandle: HWND;
  VCopyData: TCopyDataStruct;
  VRecievedStr: AnsiString;
begin
  if not GetSasPlanetWindowHandle(VHandle) then begin
    Result := False;
    Exit;
  end;

  if AArgs[Length(AArgs)] <> #0 then begin
    VRecievedStr := AArgs + #0;
  end else begin
    VRecievedStr := AArgs;
  end;

  VCopyData.dwData := 0;
  VCopyData.cbData := Length(VRecievedStr) - 1;
  VCopyData.lpData := PAnsiChar(VRecievedStr);

  Result := SendMessage(VHandle, WM_COPYDATA, 0, LPARAM(@VCopyData)) = cCmdLineArgProcessorOk;
end;

class function TWindowsMessageCopyDataSender.SendCmdLineArgs: Boolean;
var
  I: Integer;
  VArgStr: string;
begin
  Result := False;

  VArgStr := '';
  for I := 1 to ParamCount do begin
    if VArgStr <> '' then begin
      VArgStr := VArgStr + ' "' + ParamStr(I) + '"';
    end else begin
      VArgStr := '"' + ParamStr(I) + '"';
    end;
  end;

  if VArgStr <> '' then begin
    Result := SendArgs(AnsiString(VArgStr));
  end
end;

end.
