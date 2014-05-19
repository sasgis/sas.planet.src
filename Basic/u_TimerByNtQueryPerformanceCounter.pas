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

unit u_TimerByNtQueryPerformanceCounter;

interface

uses
  i_Timer;

function MakeTimerByNtQueryPerformanceCounter: ITimer;

implementation

uses
  Windows;

type
  PLARGE_INTEGER = ^Int64;

  TNtQueryPerformanceCounter = function (
    PerformanceCounter: PLARGE_INTEGER;
    PerformanceFrequency: PLARGE_INTEGER
  ): LongInt; stdcall;

{ TTimerByNtQueryPerformanceCounter }

type
  TTimerByNtQueryPerformanceCounter = class(TInterfacedObject, ITimer)
  private
    FNtQueryPerformanceCounterPtr: TNtQueryPerformanceCounter;
    FFreq: Int64;
  private
    function GetFreq: Int64;
    function CurrentTime: Int64;
  public
    constructor Create(
      const ANtQueryPerformanceCounterPtr: TNtQueryPerformanceCounter;
      const AFreq: Int64
    );
  end;

constructor TTimerByNtQueryPerformanceCounter.Create(
  const ANtQueryPerformanceCounterPtr: TNtQueryPerformanceCounter;
  const AFreq: Int64
);
begin
  Assert(Assigned(ANtQueryPerformanceCounterPtr));
  Assert(AFreq <> 0);
  inherited Create;
  FNtQueryPerformanceCounterPtr := ANtQueryPerformanceCounterPtr;
  FFreq := AFreq;
end;

function TTimerByNtQueryPerformanceCounter.CurrentTime: Int64;
var
  VCounter, VFreq: Int64;
begin
  if (0 = FNtQueryPerformanceCounterPtr(@VCounter, @VFreq)) then begin
    Assert(VFreq = FFreq);
    Result := VCounter;
  end else begin
    Result := 0;
  end;
end;

function TTimerByNtQueryPerformanceCounter.GetFreq: Int64;
begin
  Result := FFreq;
end;

const
  CTimerDLLName = 'ntdll.dll';
  CTimerFunctionName = 'NtQueryPerformanceCounter';

function MakeTimerByNtQueryPerformanceCounter: ITimer;
var
  VHandle: HMODULE;
  VFunction: TNtQueryPerformanceCounter;
  VCounter, VFreq: Int64;
begin
  Result := nil;
  VHandle := GetModuleHandle(CTimerDLLName);
  if VHandle <> 0 then begin
    VFunction := GetProcAddress(VHandle, CTimerFunctionName);
    if Assigned(VFunction) then begin
      if (VFunction(@VCounter, @VFreq) = 0) then begin
        if (VFreq > 0) then begin
          Result :=
            TTimerByNtQueryPerformanceCounter.Create(
              VFunction,
              VFreq
            );
        end;
      end;
    end;
  end;
end;

end.
