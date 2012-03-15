{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_InternalPerformanceCounter;

interface

uses
  Windows,
  i_InternalPerformanceCounter;

type
  TInternalPerformanceCounter = class(TInterfacedObject, IInternalPerformanceCounter)
  private
    FQueryPerfCntrFunc: Pointer;

    FId: Integer;
    FName: string;
    FCounter: Cardinal;
    FTotal: Int64;
    FFreq: Int64;
  protected
    function GetId: Integer;
    function GetName: string;

    function StartOperation: TInternalPerformanceCounterContext;
    procedure FinishOperation(const AContext: TInternalPerformanceCounterContext);

    function GetCounter: Cardinal;
    function GetTotalTime: TDateTime;
    function GetStaticData: IInternalPerformanceCounterStaticData;
  public
    constructor Create(const AName: string);
  end;

  TInternalPerformanceCounterStaticData = class(TInterfacedObject, IInternalPerformanceCounterStaticData)
  private
    FId: Integer;
    FName: string;
    FCounter: Cardinal;
    FTotalTime: TDateTime;
  protected
    function GetId: Integer;
    function GetName: string;
    function GetCounter: Cardinal;
    function GetTotalTime: TDateTime;
  public
    constructor Create(
      AId: Integer;
      AName: string;
      ACounter: Cardinal;
      ATotalTime: TDateTime
    );
  end;

implementation

uses
  SysUtils;

type
  PLARGE_INTEGER = ^Int64;
  TNtQueryPerformanceCounter = function (PerformanceCounter: PLARGE_INTEGER; PerformanceFrequency: PLARGE_INTEGER): LongInt; stdcall;

{ TInternalPerformanceCounter }

constructor TInternalPerformanceCounter.Create(const AName: string);
var FDummy: Int64;
begin
  FId := Integer(Self);
  FName := AName;

  FQueryPerfCntrFunc := GetProcAddress(GetModuleHandle('ntdll.dll'), 'NtQueryPerformanceCounter');

  FCounter := 0;
  FTotal := 0;

  if (nil=FQueryPerfCntrFunc) or (0 <> TNtQueryPerformanceCounter(FQueryPerfCntrFunc)(@FDummy, @FFreq)) then
    FFreq := 0;
end;

procedure TInternalPerformanceCounter.FinishOperation(const AContext: TInternalPerformanceCounterContext);
var
  VCounter, VFreq: Int64;
begin
  if AContext <> 0 then
  if (0 = TNtQueryPerformanceCounter(FQueryPerfCntrFunc)(@VCounter, @VFreq)) then begin
    // check
    Assert(VFreq=FFreq);
    // accumulate
    Inc(FCounter);
    FTotal := FTotal + (VCounter-AContext);
  end;
end;

function TInternalPerformanceCounter.GetCounter: Cardinal;
begin
  Result := FCounter;
end;

function TInternalPerformanceCounter.GetId: Integer;
begin
  Result := FId;
end;

function TInternalPerformanceCounter.GetTotalTime: TDateTime;
begin
  if (0=FFreq) then
    Result := 0
  else
    Result := FTotal / FFreq / 24 / 60 / 60;
end;

function TInternalPerformanceCounter.GetName: string;
begin
  Result := FName;
end;

function TInternalPerformanceCounter.GetStaticData: IInternalPerformanceCounterStaticData;
begin
  Result :=
    TInternalPerformanceCounterStaticData.Create(
      FId,
      FName,
      FCounter,
      GetTotalTime
    );
end;

function TInternalPerformanceCounter.StartOperation: TInternalPerformanceCounterContext;
begin
  if (nil=FQueryPerfCntrFunc) or (0 <> TNtQueryPerformanceCounter(FQueryPerfCntrFunc)(@Result, nil)) then
    Result := 0;
end;

{ TInternalPerformanceCounterStaticData }

constructor TInternalPerformanceCounterStaticData.Create(AId: Integer;
  AName: string; ACounter: Cardinal; ATotalTime: TDateTime);
begin
  FId := AId;
  FName := AName;
  FCounter := ACounter;
  FTotalTime := ATotalTime;
end;

function TInternalPerformanceCounterStaticData.GetCounter: Cardinal;
begin
  Result := FCounter;
end;

function TInternalPerformanceCounterStaticData.GetId: Integer;
begin
  Result := FId;
end;

function TInternalPerformanceCounterStaticData.GetName: string;
begin
  Result := FName;
end;

function TInternalPerformanceCounterStaticData.GetTotalTime: TDateTime;
begin
  Result := FTotalTime;
end;

end.
