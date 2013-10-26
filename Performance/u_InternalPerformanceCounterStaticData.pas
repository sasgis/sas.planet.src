unit u_InternalPerformanceCounterStaticData;

interface

uses
  i_InternalPerformanceCounter;

type
  TInternalPerformanceCounterStaticData = class(TInterfacedObject, IInternalPerformanceCounterStaticData)
  private
    FId: Integer;
    FName: string;
    FCounter: Cardinal;
    FTotalTime: TDateTime;
    FCounterInMain: Cardinal;
    FTotalTimeInMain: TDateTime;
    FMaxTime: TDateTime;
    FMinTime: TDateTime;
  private
    function GetId: Integer;
    function GetName: string;
    function GetCounter: Cardinal;
    function GetTotalTime: TDateTime;
    function GetCounterInMain: Cardinal;
    function GetTotalTimeInMain: TDateTime;
    function GetMaxTime: TDateTime;
    function GetMinTime: TDateTime;
  public
    constructor Create(
      AId: Integer;
      const AName: string;
      ACounter: Cardinal;
      const ATotalTime: TDateTime;
      const ACounterInMain: Cardinal;
      const ATotalTimeInMain: TDateTime;
      const AMaxTime: TDateTime;
      const AMinTime: TDateTime
    );
  end;

implementation

{ TInternalPerformanceCounterStaticData }

constructor TInternalPerformanceCounterStaticData.Create(
  AId: Integer;
  const AName: string;
  ACounter: Cardinal;
  const ATotalTime: TDateTime;
  const ACounterInMain: Cardinal;
  const ATotalTimeInMain: TDateTime;
  const AMaxTime: TDateTime;
  const AMinTime: TDateTime
);
begin
  inherited Create;
  FId := AId;
  FName := AName;
  FCounter := ACounter;
  FTotalTime := ATotalTime;
  FCounterInMain := ACounterInMain;
  FTotalTimeInMain := ATotalTimeInMain;
  FMaxTime := AMaxTime;
  FMinTime := AMinTime;
end;

function TInternalPerformanceCounterStaticData.GetCounter: Cardinal;
begin
  Result := FCounter;
end;

function TInternalPerformanceCounterStaticData.GetCounterInMain: Cardinal;
begin
  Result := FCounterInMain;
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

function TInternalPerformanceCounterStaticData.GetTotalTimeInMain: TDateTime;
begin
  Result := FTotalTimeInMain;
end;

function TInternalPerformanceCounterStaticData.GetMaxTime: TDateTime;
begin
  Result := FMaxTime;
end;

function TInternalPerformanceCounterStaticData.GetMinTime: TDateTime;
begin
  Result := FMinTime;
end;

end.
