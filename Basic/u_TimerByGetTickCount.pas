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

unit u_TimerByGetTickCount;

interface


uses
  i_Timer;

function MakeTimerByGetTickCount: ITimer;

implementation

uses
  Windows;

{ TTimerByGetTickCount }

type
  TTimerByGetTickCount = class(TInterfacedObject, ITimer)
  private
    function GetFreq: Int64;
    function CurrentTime: Int64;
  public
    constructor Create;
  end;

constructor TTimerByGetTickCount.Create;
begin
  inherited Create;
end;

function TTimerByGetTickCount.CurrentTime: Int64;
begin
  Result := GetTickCount;
end;

function TTimerByGetTickCount.GetFreq: Int64;
begin
  Result := 1000;
end;

function MakeTimerByGetTickCount: ITimer;
begin
  Result := TTimerByGetTickCount.Create;
end;

end.
