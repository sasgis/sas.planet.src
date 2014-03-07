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

unit u_InternalPerformanceCounter;

interface

uses
  i_InternalPerformanceCounter;

type
  TInternalPerformanceCounter = class(TInterfacedObject, IInternalPerformanceCounter)
  private
    FQueryPerfCntrFunc: Pointer;
    FMainThreadID: THandle;
    FId: Integer;
    FName: string;

    FCounter: Cardinal;
    FTotal: Int64;
    FCounterInMain: Cardinal;
    FTotalInMain: Int64;
    FMin: Int64;
    FMax: Int64;
    FFreq: Int64;
  private
    function StartOperation: TInternalPerformanceCounterContext;
    procedure FinishOperation(const AContext: TInternalPerformanceCounterContext);

    function GetStaticData: IInternalPerformanceCounterStaticData;
  public
    constructor Create(
      const AName: string;
      const AMainThreadID: THandle;
      const AQueryPerfCntrFunc: Pointer
    );
  end;

  TInternalPerformanceCounterFactory = class(TInterfacedObject, IInternalPerformanceCounterFactory)
  private
    FNtQPC: Pointer;
    FMainThreadID: THandle;
  private
    function Build(const AName: string): IInternalPerformanceCounter;
  public
    constructor Create;
  end;

implementation

uses
  Windows,
  u_InternalPerformanceCounterStaticData,
  u_QueryPerfCounter;

{ TInternalPerformanceCounter }

constructor TInternalPerformanceCounter.Create(
  const AName: string;
  const AMainThreadID: THandle;
  const AQueryPerfCntrFunc: Pointer
);
var
  VDummy: Int64;
begin
  inherited Create;
  FId := Integer(Self);
  FMainThreadID := AMainThreadID;
  FName := AName;
  FQueryPerfCntrFunc := AQueryPerfCntrFunc;

  FCounter := 0;
  FTotal := 0;
  FCounterInMain := 0;
  FTotalInMain := 0;
  FMin := $7FFFFFFFFFFFFFF;
  FMax := 0;

  if (nil = FQueryPerfCntrFunc) or (0 <> TNtQueryPerformanceCounter(FQueryPerfCntrFunc)(@VDummy, @FFreq)) then begin
    FFreq := 0;
  end;
end;

procedure TInternalPerformanceCounter.FinishOperation(const AContext: TInternalPerformanceCounterContext);
var
  VCounter, VFreq, VOperationCounter: Int64;
begin
  if AContext <> 0 then begin
    if (0 = TNtQueryPerformanceCounter(FQueryPerfCntrFunc)(@VCounter, @VFreq)) then begin
      // check
      Assert(VFreq = FFreq);
      // accumulate
      Inc(FCounter);
      VOperationCounter := VCounter - AContext;
      FTotal := FTotal + VOperationCounter;
      if GetCurrentThreadId = FMainThreadID then begin
        Inc(FCounterInMain);
        FTotalInMain := FTotalInMain + VOperationCounter;
      end;
      if VOperationCounter > FMax then begin
        FMax := VOperationCounter;
      end;
      if VOperationCounter < FMin then begin
        FMin := VOperationCounter;
      end;
    end;
  end;
end;

function TInternalPerformanceCounter.GetStaticData: IInternalPerformanceCounterStaticData;
var
  VTotalTime: Double;
  VTotalTimeInMain: Double;
  VMaxTime: Double;
  VMinTime: Double;
begin
  if (FFreq = 0) or (FTotal = 0) then begin
    VTotalTime := 0;
    VTotalTimeInMain := 0;
    VMaxTime := 0;
    VMinTime := 0;
  end else begin
    VTotalTime := FTotal / FFreq / 24 / 60 / 60;
    VTotalTimeInMain := FTotalInMain / FFreq / 24 / 60 / 60;
    VMaxTime := FMax / FFreq / 24 / 60 / 60;
    VMinTime := FMin / FFreq / 24 / 60 / 60;
  end;
  Result :=
    TInternalPerformanceCounterStaticData.Create(
      FId,
      FName,
      FCounter,
      VTotalTime,
      FCounterInMain,
      VTotalTimeInMain,
      VMaxTime,
      VMinTime
    );
end;

function TInternalPerformanceCounter.StartOperation: TInternalPerformanceCounterContext;
begin
  if (nil = FQueryPerfCntrFunc) or (0 <> TNtQueryPerformanceCounter(FQueryPerfCntrFunc)(@Result, nil)) then begin
    Result := 0;
  end;
end;

{ TInternalPerformanceCounterFactory }

constructor TInternalPerformanceCounterFactory.Create;
begin
  inherited Create;

  FMainThreadID := GetCurrentThreadId;
  FNtQPC := NtQueryPerformanceCounterPtr;
end;

function TInternalPerformanceCounterFactory.Build(
  const AName: string
): IInternalPerformanceCounter;
begin
  Result := TInternalPerformanceCounter.Create(AName, FMainThreadID, FNtQPC);
end;

end.
