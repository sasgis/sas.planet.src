unit u_InternalPerformanceCounter;

interface

uses
  Windows,
  SyncObjs,
  ActiveX,
  i_InternalPerformanceCounter;

type
  TInternalPerformanceCounter = class(TInterfacedObject, IInternalPerformanceCounter)
  private
    FCS: TCriticalSection;

    FName: string;
    FCounter: Cardinal;
    FTotalTime: TDateTime;
    FLastTimeInSeconds: Double;
  protected
    function GetName: string;

    function StartOperation: TInternalPerformanceCounterContext;
    procedure FinishOperation(AContext: TInternalPerformanceCounterContext);

    function GetCounter: Cardinal;
    function GetTotalTime: TDateTime;
    function GetLastTimeInSeconds: Double;
  public
    constructor Create(AName: string);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TInternalPerformanceCounter }

constructor TInternalPerformanceCounter.Create(AName: string);
begin
  FName := AName;
  FCS := TCriticalSection.Create;
end;

destructor TInternalPerformanceCounter.Destroy;
begin
  FreeAndNil(FCS);
  inherited;
end;

procedure TInternalPerformanceCounter.FinishOperation(
  AContext: TInternalPerformanceCounterContext);
var
  VPerformanceCounterEnd: Int64;
  VPerformanceCounterFr: Int64;
  VUpdateTimeInSeconds: Double;
  VUpdateTime: TDateTime;
begin
  if AContext <> 0 then begin
    QueryPerformanceCounter(VPerformanceCounterEnd);
    QueryPerformanceFrequency(VPerformanceCounterFr);
    VUpdateTimeInSeconds := (VPerformanceCounterEnd - AContext) / VPerformanceCounterFr;
    VUpdateTime := VUpdateTimeInSeconds/24/60/60;
    FCS.Acquire;
    try
      Inc(FCounter);
      FTotalTime := FTotalTime + VUpdateTime;
      FLastTimeInSeconds := VUpdateTimeInSeconds;
    finally
      FCS.Release;
    end;
  end;
end;

function TInternalPerformanceCounter.GetCounter: Cardinal;
begin
  Result := FCounter;
end;

function TInternalPerformanceCounter.GetLastTimeInSeconds: Double;
begin
  Result := FLastTimeInSeconds;
end;

function TInternalPerformanceCounter.GetTotalTime: TDateTime;
begin
  Result := FTotalTime;
end;

function TInternalPerformanceCounter.GetName: string;
begin
  Result := FName;
end;

function TInternalPerformanceCounter.StartOperation: TInternalPerformanceCounterContext;
begin
  if not QueryPerformanceCounter(Result) then begin
    Result := 0;
  end;
end;

end.
