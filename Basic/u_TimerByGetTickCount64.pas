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

unit u_TimerByGetTickCount64;

interface

uses
  i_Timer;

function MakeTimerByGetTickCount64: ITimer;

implementation

uses
  Windows;

type
  TGetTickCount64  = function: Int64; stdcall;

{ TTimerByGetTickCount64 }

type
  TTimerByGetTickCount64 = class(TInterfacedObject, ITimer)
  private
    FGetTickCount64: TGetTickCount64;
  private
    function GetFreq: Int64;
    function CurrentTime: Int64;
  public
    constructor Create(const AGetTickCount64: TGetTickCount64);
  end;

constructor TTimerByGetTickCount64.Create(const AGetTickCount64: TGetTickCount64);
begin
  Assert(Assigned(AGetTickCount64));
  inherited Create;
  FGetTickCount64 := AGetTickCount64;
end;

function TTimerByGetTickCount64.CurrentTime: Int64;
begin
  Result := FGetTickCount64;
end;

function TTimerByGetTickCount64.GetFreq: Int64;
begin
  Result := 1000;
end;

const
  CTimerDLLName = 'Kernel32.dll';
  CTimerFunctionName = 'GetTickCount64';

function MakeTimerByGetTickCount64: ITimer;
var
  VHandle: HMODULE;
  VFunction: TGetTickCount64;
begin
  Result := nil;
  VHandle := GetModuleHandle(CTimerDLLName);
  if VHandle <> 0 then begin
    VFunction := GetProcAddress(VHandle, CTimerFunctionName);
    if Assigned(VFunction) then begin
      Result := TTimerByGetTickCount64.Create(VFunction);
    end;
  end;
end;

end.

