unit u_BenchmarkTestRunner;

interface

uses
  i_Timer,
  i_BenchmarkItem,
  i_BenchmarkItemList,
  i_BenchmarkResult,
  i_BenchmarkResultList,
  i_BenchmarkTestRunner;

type
  TBenchmarkTestRunner = class(TInterfacedObject, IBenchmarkTestRunner)
  private
    FTimer: ITimer;
  private
    function RunSingleTestSingleRun(
      const ATest: IBenchmarkItem
    ): Double;
    function RunSingleTest(
      const ATest: IBenchmarkItem;
      const ARunCount: Integer
    ): IBenchmarkResult;
  private
    function RunTests(
      const AList: IBenchmarkItemList;
      const ARunCount: Integer
    ): IBenchmarkResultList;
  public
    constructor Create(
      const ATimer: ITimer
    );
  end;

implementation

uses
  i_InterfaceListSimple,
  u_InterfaceListSimple,
  u_BenchmarkResult,
  u_BenchmarkResultList;

const
  CMinRunTimeInSec: Double = 0.050;

{ TBenchmarkTestRunner }

constructor TBenchmarkTestRunner.Create(const ATimer: ITimer);
begin
  Assert(Assigned(ATimer));
  inherited Create;
  FTimer := ATimer;
end;

function TBenchmarkTestRunner.RunSingleTest(
  const ATest: IBenchmarkItem;
  const ARunCount: Integer
): IBenchmarkResult;
var
  i: Integer;
  VWarnUpRps: Double;
  VResults: array of Double;
begin
  SetLength(VResults, ARunCount);
  ATest.SetUp;
  try
    VWarnUpRps := RunSingleTestSingleRun(ATest);
    for i := 0 to ARunCount - 1 do begin
      VResults[i] := RunSingleTestSingleRun(ATest);
    end;
  finally
    ATest.TearDown;
  end;
  Result :=
    TBenchmarkResult.Create(
      ATest,
      VWarnUpRps,
      VResults
    );
end;

function TBenchmarkTestRunner.RunSingleTestSingleRun(
  const ATest: IBenchmarkItem
): Double;
var
  VStartTime: Int64;
  VMinEndTime: Int64;
  VCurrTime: Int64;
  VFrec: Int64;
  VCnt: Integer;
begin
  VFrec := FTimer.Freq;
  VMinEndTime := Trunc(VFrec * CMinRunTimeInSec);
  VCnt := 0;
  VStartTime := FTimer.CurrentTime;
  VMinEndTime := VMinEndTime + VStartTime;
  repeat
    ATest.RunOneStep;
    Inc(VCnt);
    VCurrTime := FTimer.CurrentTime;
  until VCurrTime >= VMinEndTime;
  Result := (VFrec * VCnt) / (VCurrTime - VStartTime);
end;

function TBenchmarkTestRunner.RunTests(
  const AList: IBenchmarkItemList;
  const ARunCount: Integer
): IBenchmarkResultList;
var
  VList: IInterfaceListSimple;
  i: Integer;
begin
  Assert(Assigned(AList));
  Assert(ARunCount > 0);
  for i := 0 to AList.Count - 1 do begin
    VList.Add(RunSingleTest(AList.Items[i], ARunCount));
  end;
  Result := TBenchmarkResultList.Create(VList.MakeStaticAndClear);
end;

end.
