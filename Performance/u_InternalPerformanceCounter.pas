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
  i_InternalPerformanceCounter;

type
  TInternalPerformanceCounter = class(TInterfacedObject, IInternalPerformanceCounter)
  private
    FQueryPerfCntrFunc: Pointer;

    FId: Integer;
    FName: string;
    FCounter: Cardinal;
    FTotal: Int64;
    FMin: Int64;
    FMax: Int64;
    FFreq: Int64;
  private
    function GetId: Integer;
    function GetName: string;

    function StartOperation: TInternalPerformanceCounterContext;
    procedure FinishOperation(const AContext: TInternalPerformanceCounterContext);

    function GetCounter: Cardinal;
    function GetTotalTime: TDateTime;
    function GetMaxTime: TDateTime;
    function GetMinTime: TDateTime;
    function GetStaticData: IInternalPerformanceCounterStaticData;
  public
    constructor Create(
      const AName: string;
      const AQueryPerfCntrFunc: Pointer
    );
  end;

  TInternalPerformanceCounterFactory = class(TInterfacedObject, IInternalPerformanceCounterFactory)
  private
    FNtQPC: Pointer;
  private
    function Build(const AName: string): IInternalPerformanceCounter;
  public
    constructor Create;
  end;

implementation

uses
  u_InternalPerformanceCounterStaticData,
  u_QueryPerfCounter;

{ TInternalPerformanceCounter }

constructor TInternalPerformanceCounter.Create(
  const AName: string;
  const AQueryPerfCntrFunc: Pointer
);
var
  VDummy: Int64;
begin
  inherited Create;
  FId := Integer(Self);
  FName := AName;

  FQueryPerfCntrFunc := AQueryPerfCntrFunc;

  FCounter := 0;
  FTotal := 0;
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
      if VOperationCounter > FMax then begin
        FMax := VOperationCounter;
      end;
      if VOperationCounter < FMin then begin
        FMin := VOperationCounter;
      end;
    end;
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
  if (FFreq = 0) then begin
    Result := 0;
  end else begin
    Result := FTotal / FFreq / 24 / 60 / 60;
  end;
end;

function TInternalPerformanceCounter.GetMaxTime: TDateTime;
begin
  if (FFreq = 0) or (FTotal = 0) then begin
    Result := 0;
  end else begin
    Result := FMax / FFreq / 24 / 60 / 60;
  end;
end;

function TInternalPerformanceCounter.GetMinTime: TDateTime;
begin
  if (FFreq = 0) or (FTotal = 0) then begin
    Result := 0;
  end else begin
    Result := FMin / FFreq / 24 / 60 / 60;
  end;
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
      GetTotalTime,
      GetMaxTime,
      GetMinTime
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

  FNtQPC := NtQueryPerformanceCounterPtr;
end;

function TInternalPerformanceCounterFactory.Build(
  const AName: string
): IInternalPerformanceCounter;
begin
  Result := TInternalPerformanceCounter.Create(AName, FNtQPC);
end;

end.
