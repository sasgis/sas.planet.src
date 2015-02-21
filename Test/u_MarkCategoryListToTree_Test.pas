unit u_MarkCategoryListToTree_Test;

{

  Delphi DUnit Test Case
  ----------------------
}

interface

uses
  TestFramework,
  i_MarkCategoryList,
  i_MarkCategoryTree,
  i_MarkCategory,
  i_InterfaceListStatic;

type
  TestMarkCategoryListToTree = class(TTestCase)
  published
    procedure TestSimpleList;
    procedure TestTree;
  end;

implementation

uses
  i_InterfaceListSimple,
  u_InterfaceListSimple,
  u_MarkCategoryList,
  u_MarkCategory,
  u_MarkSystemHelpers;

{ TestMarkCategoryListToTree }

procedure TestMarkCategoryListToTree.TestSimpleList;
var
  VList: IInterfaceListSimple;
  VCategory: IMarkCategory;
  VTree: IMarkCategoryTree;
begin
  VList := TInterfaceListSimple.Create;
  VCategory := TMarkCategory.Create('T1', True, 0, 24);
  VList.Add(VCategory);
  VCategory := TMarkCategory.Create('T2', True, 0, 24);
  VList.Add(VCategory);
  VCategory := TMarkCategory.Create('T3', True, 0, 24);
  VList.Add(VCategory);
  VTree := CategoryListToCategoryTree(TMarkCategoryList.Build(VList.MakeStaticCopy));
  CheckEqualsString('', VTree.Name);
  CheckEquals(3, VTree.SubItemCount);
  CheckNull(VTree.MarkCategory);

  CheckSame(VList.Items[0], VTree.SubItem[0].MarkCategory);
  CheckSame(VList.Items[1], VTree.SubItem[1].MarkCategory);
  CheckSame(VList.Items[2], VTree.SubItem[2].MarkCategory);

  CheckEqualsString(IMarkCategory(VList.Items[0]).Name, VTree.SubItem[0].Name);
  CheckEqualsString(IMarkCategory(VList.Items[1]).Name, VTree.SubItem[1].Name);
  CheckEqualsString(IMarkCategory(VList.Items[2]).Name, VTree.SubItem[2].Name);

  CheckEquals(0, VTree.SubItem[0].SubItemCount);
  CheckEquals(0, VTree.SubItem[1].SubItemCount);
  CheckEquals(0, VTree.SubItem[2].SubItemCount);
end;

procedure TestMarkCategoryListToTree.TestTree;
var
  VList: IInterfaceListSimple;
  VCategory: IMarkCategory;
  VTree: IMarkCategoryTree;
begin
  VList := TInterfaceListSimple.Create;
  VCategory := TMarkCategory.Create('T1\TT1\TTT1\TTTT1\TTTTT1', True, 0, 24);
  VList.Add(VCategory);
  VCategory := TMarkCategory.Create('T2', True, 0, 24);
  VList.Add(VCategory);
  VCategory := TMarkCategory.Create('T2\TT1', True, 0, 24);
  VList.Add(VCategory);
  VCategory := TMarkCategory.Create('T2\TT1\TTT1', True, 0, 24);
  VList.Add(VCategory);
  VCategory := TMarkCategory.Create('T2\TT1\TTT2', True, 0, 24);
  VList.Add(VCategory);
  VCategory := TMarkCategory.Create('T2\TT2\TTT1', True, 0, 24);
  VList.Add(VCategory);
  VCategory := TMarkCategory.Create('T3\TT1', True, 0, 24);
  VList.Add(VCategory);
  VCategory := TMarkCategory.Create('T3\TT1\TTT1', True, 0, 24);
  VList.Add(VCategory);
  VTree := CategoryListToCategoryTree(TMarkCategoryList.Build(VList.MakeStaticCopy));
  CheckEqualsString('', VTree.Name);
  CheckNull(VTree.MarkCategory);
  CheckEquals(3, VTree.SubItemCount);

  CheckNull(VTree.SubItem[0].MarkCategory);
  CheckEqualsString('T1', VTree.SubItem[0].Name);
  CheckEquals(1, VTree.SubItem[0].SubItemCount);

  CheckNull(VTree.SubItem[0].SubItem[0].MarkCategory);
  CheckEqualsString('TT1', VTree.SubItem[0].SubItem[0].Name);
  CheckEquals(1, VTree.SubItem[0].SubItem[0].SubItemCount);

  CheckNull(VTree.SubItem[0].SubItem[0].SubItem[0].MarkCategory);
  CheckEqualsString('TTT1', VTree.SubItem[0].SubItem[0].SubItem[0].Name);
  CheckEquals(1, VTree.SubItem[0].SubItem[0].SubItem[0].SubItemCount);

  CheckNull(VTree.SubItem[0].SubItem[0].SubItem[0].SubItem[0].MarkCategory);
  CheckEqualsString('TTTT1', VTree.SubItem[0].SubItem[0].SubItem[0].SubItem[0].Name);
  CheckEquals(1, VTree.SubItem[0].SubItem[0].SubItem[0].SubItem[0].SubItemCount);

  CheckEqualsString('TTTTT1', VTree.SubItem[0].SubItem[0].SubItem[0].SubItem[0].SubItem[0].Name);
  CheckSame(VList.Items[0], VTree.SubItem[0].SubItem[0].SubItem[0].SubItem[0].SubItem[0].MarkCategory);
  CheckEquals(0, VTree.SubItem[0].SubItem[0].SubItem[0].SubItem[0].SubItem[0].SubItemCount);

  CheckSame(VList.Items[1], VTree.SubItem[1].MarkCategory);
  CheckEqualsString('T2', VTree.SubItem[1].Name);
  CheckEquals(2, VTree.SubItem[1].SubItemCount);

  CheckSame(VList.Items[2], VTree.SubItem[1].SubItem[0].MarkCategory);
  CheckEqualsString('TT1', VTree.SubItem[1].SubItem[0].Name);
  CheckEquals(2, VTree.SubItem[1].SubItem[0].SubItemCount);

  CheckSame(VList.Items[3], VTree.SubItem[1].SubItem[0].SubItem[0].MarkCategory);
  CheckEqualsString('TTT1', VTree.SubItem[1].SubItem[0].SubItem[0].Name);
  CheckEquals(0, VTree.SubItem[1].SubItem[0].SubItem[0].SubItemCount);

  CheckSame(VList.Items[4], VTree.SubItem[1].SubItem[0].SubItem[1].MarkCategory);
  CheckEqualsString('TTT2', VTree.SubItem[1].SubItem[0].SubItem[1].Name);
  CheckEquals(0, VTree.SubItem[1].SubItem[0].SubItem[1].SubItemCount);

  CheckNull(VTree.SubItem[1].SubItem[1].MarkCategory);
  CheckEqualsString('TT2', VTree.SubItem[1].SubItem[1].Name);
  CheckEquals(1, VTree.SubItem[1].SubItem[1].SubItemCount);

  CheckSame(VList.Items[5], VTree.SubItem[1].SubItem[1].SubItem[0].MarkCategory);
  CheckEqualsString('TTT1', VTree.SubItem[1].SubItem[1].SubItem[0].Name);
  CheckEquals(0, VTree.SubItem[1].SubItem[1].SubItem[0].SubItemCount);

  CheckNull(VTree.SubItem[2].MarkCategory);
  CheckEqualsString('T3', VTree.SubItem[2].Name);
  CheckEquals(1, VTree.SubItem[2].SubItemCount);

  CheckSame(VList.Items[6], VTree.SubItem[2].SubItem[0].MarkCategory);
  CheckEqualsString('TT1', VTree.SubItem[2].SubItem[0].Name);
  CheckEquals(1, VTree.SubItem[2].SubItem[0].SubItemCount);

  CheckSame(VList.Items[7], VTree.SubItem[2].SubItem[0].SubItem[0].MarkCategory);
  CheckEqualsString('TTT1', VTree.SubItem[2].SubItem[0].SubItem[0].Name);
  CheckEquals(0, VTree.SubItem[2].SubItem[0].SubItem[0].SubItemCount);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestMarkCategoryListToTree.Suite);
end.
