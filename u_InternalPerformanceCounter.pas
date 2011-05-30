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

    FId: Integer;
    FName: string;
    FCounter: Cardinal;
    FTotalTime: TDateTime;
    FLastTimeInSeconds: Double;
  protected
    function GetId: Integer;
    function GetName: string;

    function StartOperation: TInternalPerformanceCounterContext;
    procedure FinishOperation(AContext: TInternalPerformanceCounterContext);

    function GetCounter: Cardinal;
    function GetTotalTime: TDateTime;
    function GetLastTimeInSeconds: Double;
    function GetStaticData: IInternalPerformanceCounterStaticData;
  public
    constructor Create(AName: string);
    destructor Destroy; override;
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

{ TInternalPerformanceCounter }

constructor TInternalPerformanceCounter.Create(AName: string);
begin
  FId := Integer(Self);
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

function TInternalPerformanceCounter.GetId: Integer;
begin
  Result := FId;
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

function TInternalPerformanceCounter.GetStaticData: IInternalPerformanceCounterStaticData;
begin
  Result :=
    TInternalPerformanceCounterStaticData.Create(
      FId,
      FName,
      FCounter,
      FTotalTime
    );
end;

function TInternalPerformanceCounter.StartOperation: TInternalPerformanceCounterContext;
begin
  if not QueryPerformanceCounter(Result) then begin
    Result := 0;
  end;
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
