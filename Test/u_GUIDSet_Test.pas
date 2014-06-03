unit u_GUIDSet_Test;

{

  Delphi DUnit Test Case
  ----------------------
}

interface

uses
  TestFramework,
  i_IGUIDInterfaceSet_Test,
  i_IGUIDObjectSet_Test;

type
  TestTGUIDInterfaceSet = class(TestIGUIDInterfaceSet)
  public
    procedure SetUp; override;
  end;

  TestTGUIDObjectSet = class(TestIGUIDObjectSet)
  public
    procedure SetUp; override;
  end;

implementation

uses
  u_GUIDInterfaceSet,
  u_GUIDObjectSet;

{ TestTGUIDInterfaceSet }
procedure TestTGUIDInterfaceSet.SetUp;
begin
  inherited;
  FGUIDList := TGUIDInterfaceSet.Create;
end;

{ TestTGUIDObjectSet }

procedure TestTGUIDObjectSet.SetUp;
begin
  inherited;
  FGUIDList := TGUIDObjectSet.Create;
end;

initialization
  RegisterTest(TestTGUIDInterfaceSet.Suite);
  RegisterTest(TestTGUIDObjectSet.Suite);
end.
