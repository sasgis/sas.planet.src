unit u_GUIDList_Test;
{

  Delphi DUnit Test Case
  ----------------------
}

interface

uses
  ActiveX,
  TestFramework,
  i_IGUIDList_Test,
  i_IGUIDObjectList_Test;

type
  // Test methods for class TGUIDList

  TestTGUIDInterfaceList = class(TestIGUIDInterfaceList)
  public
    procedure SetUp; override;
  end;

  TestTGUIDObjectList = class(TestIGUIDObjectList)
  public
    procedure SetUp; override;
  end;

implementation

uses
  u_GUIDInterfaceList,
  u_GUIDObjectList;

{ TestTGUIDList }
procedure TestTGUIDInterfaceList.SetUp;
begin
  inherited;
  FGUIDList := TGUIDInterfaceList.Create;
end;

{ TestTGUIDObjectList }

procedure TestTGUIDObjectList.SetUp;
begin
  inherited;
  FGUIDList := TGUIDObjectList.Create;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTGUIDInterfaceList.Suite);
  RegisterTest(TestTGUIDObjectList.Suite);
end.

