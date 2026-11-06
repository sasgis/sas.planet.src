unit u_TestFilter_Test;

interface

uses
  TestFramework,
  u_TestFilter;

type
  // A fake test case used to build synthetic test trees for the filter tests.
  // Its methods are never executed - only their names are used.
  TFakeTestCase = class(TTestCase)
  published
    procedure TestAlpha;
    procedure TestBeta;
  end;

  TSASTestFilterTest = class(TTestCase)
  private
    FFilter: TSASTestFilter;
    FRoot: ITestSuite;

    function EnabledNames: string;
    function ListText: string;
    procedure CheckApply(
      const AMasks: array of string;
      const AExpectedCount: Integer;
      const AExpectedNames: string
    );
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNoMasksKeepsAll;
    procedure TestFilterByClass;
    procedure TestFilterByMethod;
    procedure TestFilterByFullPath;
    procedure TestMaskIsCaseInsensitive;
    procedure TestMultipleMasks;
    procedure TestFilterNoMatch;
    procedure TestCollectListAll;
    procedure TestCollectListFiltered;
    procedure TestCollectListNoMatch;
  end;

implementation

uses
  Classes,
  SysUtils;

{ TFakeTestCase }

procedure TFakeTestCase.TestAlpha;
begin
  //
end;

procedure TFakeTestCase.TestBeta;
begin
  //
end;

{ TSASTestFilterTest }

procedure TSASTestFilterTest.SetUp;
begin
  inherited SetUp;
  FFilter := TSASTestFilter.Create;
  FRoot := TTestSuite.Create('Root', [
    TTestSuite.Create('SuiteAlpha', [TFakeTestCase.Create('TestAlpha'), TFakeTestCase.Create('TestBeta')]),
    TTestSuite.Create('SuiteBeta', [TFakeTestCase.Create('TestAlpha')])
  ]);
end;

procedure TSASTestFilterTest.TearDown;
begin
  FRoot := nil;
  FreeAndNil(FFilter);
  inherited TearDown;
end;

function TSASTestFilterTest.EnabledNames: string;
var
  I, J: Integer;
  VSuite, VLeaf: ITest;
begin
  Result := '';
  for I := 0 to FRoot.Tests.Count - 1 do begin
    VSuite := FRoot.Tests[I] as ITest;
    for J := 0 to VSuite.Tests.Count - 1 do begin
      VLeaf := VSuite.Tests[J] as ITest;
      if VLeaf.Enabled then begin
        if Result <> '' then begin
          Result := Result + ',';
        end;
        Result := Result + VSuite.Name + '.' + VLeaf.Name;
      end;
    end;
  end;
end;

function TSASTestFilterTest.ListText: string;
var
  VList: TStringList;
begin
  VList := TStringList.Create;
  try
    FFilter.CollectList(FRoot, VList);
    Result := VList.Text;
  finally
    VList.Free;
  end;
end;

procedure TSASTestFilterTest.CheckApply(
  const AMasks: array of string;
  const AExpectedCount: Integer;
  const AExpectedNames: string
);
var
  I: Integer;
begin
  for I := Low(AMasks) to High(AMasks) do begin
    FFilter.Masks.Add(AMasks[I]);
  end;
  CheckEquals(AExpectedCount, FFilter.Apply(FRoot));
  CheckEquals(AExpectedNames, EnabledNames);
end;

procedure TSASTestFilterTest.TestNoMasksKeepsAll;
begin
  CheckFalse(FFilter.HasMasks);
  CheckApply([], 3, 'SuiteAlpha.TestAlpha,SuiteAlpha.TestBeta,SuiteBeta.TestAlpha');
end;

procedure TSASTestFilterTest.TestFilterByClass;
begin
  CheckApply(['SuiteAlpha'], 2, 'SuiteAlpha.TestAlpha,SuiteAlpha.TestBeta');
end;

procedure TSASTestFilterTest.TestFilterByMethod;
begin
  CheckApply(['TestBeta'], 1, 'SuiteAlpha.TestBeta');
end;

procedure TSASTestFilterTest.TestFilterByFullPath;
begin
  CheckApply(['SuiteBeta.TestAlpha'], 1, 'SuiteBeta.TestAlpha');
end;

procedure TSASTestFilterTest.TestMaskIsCaseInsensitive;
begin
  CheckApply(['suitebeta.TESTALPHA'], 1, 'SuiteBeta.TestAlpha');
end;

procedure TSASTestFilterTest.TestMultipleMasks;
begin
  CheckApply(['TestBeta', 'SuiteBeta'], 2, 'SuiteAlpha.TestBeta,SuiteBeta.TestAlpha');
end;

procedure TSASTestFilterTest.TestFilterNoMatch;
begin
  CheckApply(['NoSuchTest'], 0, '');
  // Suites without enabled children must be disabled as well.
  CheckFalse((FRoot.Tests[0] as ITest).Enabled);
  CheckFalse((FRoot.Tests[1] as ITest).Enabled);
end;

procedure TSASTestFilterTest.TestCollectListAll;
begin
  CheckEquals(
    'SuiteAlpha'  + sLineBreak +
    '  TestAlpha' + sLineBreak +
    '  TestBeta'  + sLineBreak +
    'SuiteBeta'   + sLineBreak +
    '  TestAlpha' + sLineBreak,
    ListText
  );
end;

procedure TSASTestFilterTest.TestCollectListFiltered;
begin
  FFilter.Masks.Add('SuiteBeta');
  CheckEquals(
    'SuiteBeta'   + sLineBreak +
    '  TestAlpha' + sLineBreak,
    ListText
  );
end;

procedure TSASTestFilterTest.TestCollectListNoMatch;
begin
  FFilter.Masks.Add('NoSuchTest');
  CheckEquals('', ListText);
end;

initialization
  RegisterTest(TSASTestFilterTest.Suite);

end.
