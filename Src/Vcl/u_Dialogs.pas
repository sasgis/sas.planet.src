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

function ShowQuestionMessage(const AMessage: string; const AButtons: UINT): Integer;

function ShowInfoMessage(const AMessage: string; const AButtons: UINT = MB_OK): Integer;
function ShowInfoMessageSync(const AMessage: string; const AButtons: UINT = MB_OK): Integer;

function ShowErrorMessage(const AMessage: string; const AButtons: UINT = MB_OK): Integer;
function ShowErrorMessageSync(const AMessage: string; const AButtons: UINT = MB_OK): Integer;

implementation

uses
  Forms,
  Classes,
  u_ResStrings;

function ShowQuestionMessage(const AMessage: string; const AButtons: UINT): Integer;
begin
  Result := Application.MessageBox(PChar(AMessage), PChar(SAS_MSG_coution), AButtons + MB_ICONQUESTION + MB_TOPMOST);
end;

function ShowInfoMessage(const AMessage: string; const AButtons: UINT): Integer;
begin
  Result := Application.MessageBox(PChar(AMessage), PChar(SAS_MSG_information), AButtons + MB_ICONINFORMATION + MB_TOPMOST);
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

function ShowErrorMessage(const AMessage: string; const AButtons: UINT): Integer;
begin
  Result := Application.MessageBox(PChar(AMessage), PChar(SAS_MSG_error), AButtons + MB_ICONERROR + MB_TOPMOST);
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

end.
