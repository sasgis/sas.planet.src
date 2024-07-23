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

unit u_Dialogs;

interface

uses
  Windows;

// https://learn.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-messagebox

const
  MB_OK               = Windows.MB_OK;
  MB_OKCANCEL         = Windows.MB_OKCANCEL;
  MB_ABORTRETRYIGNORE = Windows.MB_ABORTRETRYIGNORE;
  MB_YESNOCANCEL      = Windows.MB_YESNOCANCEL;
  MB_YESNO            = Windows.MB_YESNO;
  MB_RETRYCANCEL      = Windows.MB_RETRYCANCEL;

  ID_OK               = Windows.ID_OK;
  ID_CANCEL           = Windows.ID_CANCEL;
  ID_ABORT            = Windows.ID_ABORT;
  ID_RETRY            = Windows.ID_RETRY;
  ID_IGNORE           = Windows.ID_IGNORE;
  ID_YES              = Windows.ID_YES;
  ID_NO               = Windows.ID_NO;
  ID_CLOSE            = Windows.ID_CLOSE;
  ID_HELP             = Windows.ID_HELP;

function ShowQuestionMessage(const AMessage: string; const AButtons: UINT): Integer; overload;
function ShowQuestionMessage(const AHandle: HWND; const AMessage: string; const AButtons: UINT): Integer; overload;

function ShowQuestionMessageSync(const AMessage: string; const AButtons: UINT): Integer; overload;
function ShowQuestionMessageSync(const AHandle: HWND; const AMessage: string; const AButtons: UINT): Integer; overload;

function ShowInfoMessage(const AMessage: string; const AButtons: UINT = MB_OK): Integer; overload;
function ShowInfoMessage(const AHandle: HWND; const AMessage: string; const AButtons: UINT = MB_OK): Integer; overload;

function ShowInfoMessageSync(const AMessage: string; const AButtons: UINT = MB_OK): Integer; overload;
function ShowInfoMessageSync(const AHandle: HWND; const AMessage: string; const AButtons: UINT = MB_OK): Integer; overload;

function ShowWarningMessage(const AMessage: string; const AButtons: UINT = MB_OK): Integer; overload;
function ShowWarningMessage(const AHandle: HWND; const AMessage: string; const AButtons: UINT = MB_OK): Integer; overload;

function ShowWarningMessageSync(const AMessage: string; const AButtons: UINT = MB_OK): Integer; overload;
function ShowWarningMessageSync(const AHandle: HWND; const AMessage: string; const AButtons: UINT = MB_OK): Integer; overload;

function ShowErrorMessage(const AMessage: string; const AButtons: UINT = MB_OK): Integer; overload;
function ShowErrorMessage(const AHandle: HWND; const AMessage: string; const AButtons: UINT = MB_OK): Integer; overload;

function ShowErrorMessageSync(const AMessage: string; const AButtons: UINT = MB_OK): Integer; overload;
function ShowErrorMessageSync(const AHandle: HWND; const AMessage: string; const AButtons: UINT = MB_OK): Integer; overload;

implementation

uses
  Forms,
  Classes,
  u_ResStrings;

const
  CQuestionMessageFlags = MB_ICONQUESTION or MB_TOPMOST;
  CInfoMessageFlags     = MB_ICONINFORMATION or MB_TOPMOST;
  CWarningMessageFlags  = MB_ICONWARNING or MB_TOPMOST;
  CErrorMessageFlags    = MB_ICONERROR or MB_TOPMOST;

function ShowQuestionMessage(const AMessage: string; const AButtons: UINT): Integer;
begin
  Result := Application.MessageBox(PChar(AMessage), PChar(SAS_MSG_coution), AButtons or CQuestionMessageFlags);
end;

function ShowQuestionMessage(const AHandle: HWND; const AMessage: string; const AButtons: UINT): Integer;
begin
  Result := Windows.MessageBox(AHandle, PChar(AMessage), PChar(SAS_MSG_coution), AButtons or CQuestionMessageFlags);
end;

function ShowQuestionMessageSync(const AMessage: string; const AButtons: UINT): Integer;
var
  VResult: Integer;
begin
  TThread.Synchronize(nil,
    procedure
    begin
      VResult := ShowQuestionMessage(AMessage, AButtons);
    end
  );
  Result := VResult;
end;

function ShowQuestionMessageSync(const AHandle: HWND; const AMessage: string; const AButtons: UINT): Integer;
var
  VResult: Integer;
begin
  TThread.Synchronize(nil,
    procedure
    begin
      VResult := ShowQuestionMessage(AHandle, AMessage, AButtons);
    end
  );
  Result := VResult;
end;

function ShowInfoMessage(const AMessage: string; const AButtons: UINT): Integer;
begin
  Result := Application.MessageBox(PChar(AMessage), PChar(SAS_MSG_information), AButtons or CInfoMessageFlags);
end;

function ShowInfoMessage(const AHandle: HWND; const AMessage: string; const AButtons: UINT): Integer;
begin
  Result := Windows.MessageBox(AHandle, PChar(AMessage), PChar(SAS_MSG_information), AButtons or CInfoMessageFlags);
end;

function ShowInfoMessageSync(const AMessage: string; const AButtons: UINT): Integer;
var
  VResult: Integer;
begin
  TThread.Synchronize(nil,
    procedure
    begin
      VResult := ShowInfoMessage(AMessage, AButtons);
    end
  );
  Result := VResult;
end;

function ShowInfoMessageSync(const AHandle: HWND; const AMessage: string; const AButtons: UINT): Integer;
var
  VResult: Integer;
begin
  TThread.Synchronize(nil,
    procedure
    begin
      VResult := ShowInfoMessage(AHandle, AMessage, AButtons);
    end
  );
  Result := VResult;
end;

function ShowWarningMessage(const AMessage: string; const AButtons: UINT): Integer;
begin
  Result := Application.MessageBox(PChar(AMessage), PChar(SAS_MSG_warning), AButtons or CWarningMessageFlags);
end;

function ShowWarningMessage(const AHandle: HWND; const AMessage: string; const AButtons: UINT): Integer;
begin
  Result := Windows.MessageBox(AHandle, PChar(AMessage), PChar(SAS_MSG_warning), AButtons or CWarningMessageFlags);
end;

function ShowWarningMessageSync(const AMessage: string; const AButtons: UINT): Integer;
var
  VResult: Integer;
begin
  TThread.Synchronize(nil,
    procedure
    begin
      VResult := ShowWarningMessage(AMessage, AButtons);
    end
  );
  Result := VResult;
end;

function ShowWarningMessageSync(const AHandle: HWND; const AMessage: string; const AButtons: UINT): Integer;
var
  VResult: Integer;
begin
  TThread.Synchronize(nil,
    procedure
    begin
      VResult := ShowWarningMessage(AHandle, AMessage, AButtons);
    end
  );
  Result := VResult;
end;

function ShowErrorMessage(const AMessage: string; const AButtons: UINT): Integer;
begin
  Result := Application.MessageBox(PChar(AMessage), PChar(SAS_MSG_error), AButtons or CErrorMessageFlags);
end;

function ShowErrorMessage(const AHandle: HWND; const AMessage: string; const AButtons: UINT): Integer;
begin
  Result := Windows.MessageBox(AHandle, PChar(AMessage), PChar(SAS_MSG_error), AButtons or CErrorMessageFlags);
end;

function ShowErrorMessageSync(const AMessage: string; const AButtons: UINT): Integer;
var
  VResult: Integer;
begin
  TThread.Synchronize(nil,
    procedure
    begin
      VResult := ShowErrorMessage(AMessage, AButtons);
    end
  );
  Result := VResult;
end;

function ShowErrorMessageSync(const AHandle: HWND; const AMessage: string; const AButtons: UINT): Integer;
var
  VResult: Integer;
begin
  TThread.Synchronize(nil,
    procedure
    begin
      VResult := ShowErrorMessage(AHandle, AMessage, AButtons);
    end
  );
  Result := VResult;
end;

end.
