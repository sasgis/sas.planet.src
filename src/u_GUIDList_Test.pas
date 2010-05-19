unit u_GUIDList_Test;
{

  Delphi DUnit Test Case
  ----------------------
}

interface

uses
  ActiveX,
  TestFramework,
  u_GUIDList,
  i_IGUIDList,
  i_IGUIDList_Test;

type
  // Test methods for class TGUIDList

  TestTGUIDList = class(TestIGUIDList)
  public
    procedure SetUp; override;
  end;

implementation

{ TestTGUIDList }
procedure TestTGUIDList.SetUp;
begin
  FGUIDList := TGUIDList.Create;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTGUIDList.Suite);
end.

