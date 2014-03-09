unit u_GUIDSetStatic_Test;

{

  Delphi DUnit Test Case
  ----------------------
}

interface

uses
  TestFramework;

type
  TestTGUIDSetStatic = class(TTestCase)
  published
    procedure TestEmpty;
    procedure TestOneItem;
    procedure TestTwoEqual;
    procedure TestThreeUniqu;
    procedure TestFourWithEqual;
    procedure TestFiveSorted;
    procedure TestSixUnsorted;
  end;

implementation

uses
  SysUtils,
  i_GUIDListStatic,
  u_GUIDListStatic;

const
  G1: TGUID = '{357CDEAB-14FE-449E-B282-A5B96094BE81}';
  G2: TGUID = '{357CDEAB-14FE-449E-B282-A5B96094BE82}';
  G3: TGUID = '{357CDEAB-14FE-449E-B282-A5B96094BE83}';
  G4: TGUID = '{357CDEAB-14FE-449E-B282-A5B96094BE84}';
  G5: TGUID = '{86EA601F-EA2D-4E26-BDBE-1C4F65444CA5}';
  G6: TGUID = '{F81CCA0A-D467-4962-A9F7-2A50B4BFDD46}';
  G7: TGUID = '{F81CCA0A-D467-4962-A9F7-2A50B4BFDD47}';

{ TestTGUIDSetStatic }

procedure TestTGUIDSetStatic.TestEmpty;
var
  VList: IGUIDSetStatic;
begin
  VList := TGUIDSetStatic.CreateAndSort([], 0);
  CheckEquals(0, VList.Count);

  CheckFalse(VList.IsExists(G1));
  CheckFalse(VList.IsExists(G2));
  CheckFalse(VList.IsExists(G3));
  CheckFalse(VList.IsExists(G4));
  CheckFalse(VList.IsExists(G5));
  CheckFalse(VList.IsExists(G6));
  CheckFalse(VList.IsExists(G7));
end;

procedure TestTGUIDSetStatic.TestOneItem;
var
  VList: IGUIDSetStatic;
begin
  VList := TGUIDSetStatic.CreateAndSort([G1], 1);
  CheckEquals(1, VList.Count);
  CheckEqualsString(GUIDToString(G1), GUIDToString(VList.Items[0]));

  CheckTrue(VList.IsExists(G1));
  CheckFalse(VList.IsExists(G2));
  CheckFalse(VList.IsExists(G3));
  CheckFalse(VList.IsExists(G4));
  CheckFalse(VList.IsExists(G5));
  CheckFalse(VList.IsExists(G6));
  CheckFalse(VList.IsExists(G7));
end;

procedure TestTGUIDSetStatic.TestTwoEqual;
var
  VList: IGUIDSetStatic;
begin
  VList := TGUIDSetStatic.CreateAndSort([G1, G1], 2);
  CheckEquals(1, VList.Count);
  CheckEqualsString(GUIDToString(G1), GUIDToString(VList.Items[0]));

  CheckTrue(VList.IsExists(G1));
  CheckFalse(VList.IsExists(G2));
  CheckFalse(VList.IsExists(G3));
  CheckFalse(VList.IsExists(G4));
  CheckFalse(VList.IsExists(G5));
  CheckFalse(VList.IsExists(G6));
  CheckFalse(VList.IsExists(G7));
end;

procedure TestTGUIDSetStatic.TestThreeUniqu;
var
  VList: IGUIDSetStatic;
begin
  VList := TGUIDSetStatic.CreateAndSort([G1, G5, G7], 3);
  CheckEquals(3, VList.Count);
  CheckEqualsString(GUIDToString(G1), GUIDToString(VList.Items[0]));
  CheckEqualsString(GUIDToString(G5), GUIDToString(VList.Items[1]));
  CheckEqualsString(GUIDToString(G7), GUIDToString(VList.Items[2]));

  CheckTrue(VList.IsExists(G1));
  CheckFalse(VList.IsExists(G2));
  CheckFalse(VList.IsExists(G3));
  CheckFalse(VList.IsExists(G4));
  CheckTrue(VList.IsExists(G5));
  CheckFalse(VList.IsExists(G6));
  CheckTrue(VList.IsExists(G7));
end;

procedure TestTGUIDSetStatic.TestFourWithEqual;
var
  VList: IGUIDSetStatic;
begin
  VList := TGUIDSetStatic.CreateAndSort([G1, G5, G7, G1], 4);
  CheckEquals(3, VList.Count);
  CheckEqualsString(GUIDToString(G1), GUIDToString(VList.Items[0]));
  CheckEqualsString(GUIDToString(G5), GUIDToString(VList.Items[1]));
  CheckEqualsString(GUIDToString(G7), GUIDToString(VList.Items[2]));

  CheckTrue(VList.IsExists(G1));
  CheckFalse(VList.IsExists(G2));
  CheckFalse(VList.IsExists(G3));
  CheckFalse(VList.IsExists(G4));
  CheckTrue(VList.IsExists(G5));
  CheckFalse(VList.IsExists(G6));
  CheckTrue(VList.IsExists(G7));
end;

procedure TestTGUIDSetStatic.TestFiveSorted;
var
  VList: IGUIDSetStatic;
begin
  VList := TGUIDSetStatic.CreateAndSort([G1, G2, G3, G5, G7], 5);
  CheckEquals(5, VList.Count);
  CheckEqualsString(GUIDToString(G1), GUIDToString(VList.Items[0]));
  CheckEqualsString(GUIDToString(G2), GUIDToString(VList.Items[1]));
  CheckEqualsString(GUIDToString(G3), GUIDToString(VList.Items[2]));
  CheckEqualsString(GUIDToString(G5), GUIDToString(VList.Items[3]));
  CheckEqualsString(GUIDToString(G7), GUIDToString(VList.Items[4]));

  CheckTrue(VList.IsExists(G1));
  CheckTrue(VList.IsExists(G2));
  CheckTrue(VList.IsExists(G3));
  CheckFalse(VList.IsExists(G4));
  CheckTrue(VList.IsExists(G5));
  CheckFalse(VList.IsExists(G6));
  CheckTrue(VList.IsExists(G7));
end;

procedure TestTGUIDSetStatic.TestSixUnsorted;
var
  VList: IGUIDSetStatic;
begin
  VList := TGUIDSetStatic.CreateAndSort([G7, G6, G5, G4, G3, G2], 6);
  CheckEquals(6, VList.Count);
  CheckEqualsString(GUIDToString(G2), GUIDToString(VList.Items[0]));
  CheckEqualsString(GUIDToString(G3), GUIDToString(VList.Items[1]));
  CheckEqualsString(GUIDToString(G4), GUIDToString(VList.Items[2]));
  CheckEqualsString(GUIDToString(G5), GUIDToString(VList.Items[3]));
  CheckEqualsString(GUIDToString(G6), GUIDToString(VList.Items[4]));
  CheckEqualsString(GUIDToString(G7), GUIDToString(VList.Items[5]));

  CheckFalse(VList.IsExists(G1));
  CheckTrue(VList.IsExists(G2));
  CheckTrue(VList.IsExists(G3));
  CheckTrue(VList.IsExists(G4));
  CheckTrue(VList.IsExists(G5));
  CheckTrue(VList.IsExists(G6));
  CheckTrue(VList.IsExists(G7));
end;

initialization
  RegisterTest(TestTGUIDSetStatic.Suite);
end.
