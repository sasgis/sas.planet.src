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

unit u_MarkSystemErrorHandler;

interface

uses
  SysUtils,
  i_Notifier;

procedure CatchException(const E: Exception; const ANotifier: INotifierInternal);

implementation

uses
  {$IFDEF EUREKALOG}
  ExceptionLog,
  ECore;
  {$ELSE}
  i_MarkSystemErrorMsg,
  u_MarkSystemErrorMsg;
  {$ENDIF}

procedure CatchException(const E: Exception; const ANotifier: INotifierInternal);
{$IFNDEF EUREKALOG}
var
  VErrorMsg: IMarkSystemErrorMsg;
{$ENDIF}
begin
  {$IFDEF EUREKALOG}
  ShowLastExceptionData;
  {$ELSE}
  VErrorMsg := TMarkSystemErrorMsg.Create(E.ClassName + ': ' + E.Message);
  ANotifier.Notify(VErrorMsg);
  {$ENDIF}
end;

end.
