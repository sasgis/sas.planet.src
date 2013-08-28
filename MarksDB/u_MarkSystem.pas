unit u_MarkSystem;

interface

uses
  Classes,
  i_Mark,
  i_MarkCategory,
  i_MarkSystem,
  i_ReadWriteState,
  i_MarkDb,
  i_MarkCategoryDB,
  i_ImportConfig,
  i_HashFunction,
  i_InterfaceListStatic,
  i_AppearanceOfMarkFactory,
  i_VectorItemSubset,
  i_VectorItemSubsetBuilder,
  i_VectorItemTree,
  i_StaticTreeItem,
  i_PathConfig,
  i_MarkPicture,
  i_VectorItemsFactory,
  i_InterfaceListSimple,
  i_InternalPerformanceCounter,
  i_NotifierOperation,
  i_HtmlToHintTextConverter,
  i_MarkSystemImplChangeable,
  i_MarkFactory,
  i_MarkCategoryFactory,
  i_StaticTreeBuilder,
  u_BaseInterfacedObject;

type
  TMarkSystem = class(TBaseInterfacedObject, IMarkSystem)
  private
    FMarkPictureList: IMarkPictureList;
    FSystemImpl: IMarkSystemImplChangeable;
    FMarkDb: IMarkDb;
    FCategoryDB: IMarkCategoryDB;
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;

    FCategoryTreeBuilder: IStaticTreeBuilder;
    procedure PrepareFromTreeForImport(
      const AMarkList: IInterfaceListSimple;
      const ADataItemTree: IVectorItemTree;
      const AImportConfig: IImportConfig
    );

  private
    function GetState: IReadWriteStateChangeble;
    function GetMarkDb: IMarkDb;
    function GetCategoryDB: IMarkCategoryDB;

    function GetStringIdByMark(const AMark: IMark): string;
    function GetMarkByStringId(const AId: string): IMark;
    function GetMarkCategoryByStringId(const AId: string): IMarkCategory;

    function GetVisibleCategories(AZoom: Byte): IInterfaceListStatic;
    function GetVisibleCategoriesIgnoreZoom: IInterfaceListStatic;
    procedure DeleteCategoryWithMarks(const ACategory: IMarkCategory);

    function ImportItemsTree(
      const ADataItemTree: IVectorItemTree;
      const AImportConfig: IImportConfig
    ): IInterfaceListStatic;

    function CategoryTreeToMarkTree(const ACategoryTree: IStaticTreeItem): IVectorItemTree;
    function CategoryListToStaticTree(const AList: IInterfaceListStatic): IStaticTreeItem;
  public
    constructor Create(
      const ABasePath: IPathConfig;
      const AMarkPictureList: IMarkPictureList;
      const AMarkFactory: IMarkFactory;
      const AMarkCategoryFactory: IMarkCategoryFactory;
      const AHashFunction: IHashFunction;
      const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
      const AVectorItemsFactory: IVectorItemsFactory;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const APerfCounterList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AHintConverter: IHtmlToHintTextConverter
    );

  end;

implementation

uses
  SysUtils,
  i_Category,
  i_VectorDataItemSimple,
  i_MarkSystemImpl,
  u_StaticTreeBuilderBase,
  u_InterfaceListSimple,
  u_VectorItemTree,
  u_MarkDbByImpl,
  u_MarkCategoryDbByImpl,
  u_MarkSystemImplChangeable;

type
  TStaticTreeByCategoryListBuilder = class(TStaticTreeBuilderBaseBySlash)
  protected
    procedure ProcessItems(
      const ASource: IInterface;
      AList: TStringList
    ); override;
    function GetNameFromItem(
      const ASource: IInterface;
      const AItem: IInterface
    ): string; override;
  end;

{ TStaticTreeByCategoryListBuilder }

function TStaticTreeByCategoryListBuilder.GetNameFromItem(
  const ASource: IInterface;
  const AItem: IInterface
): string;
begin
  Result := (AItem as ICategory).Name;
end;

procedure TStaticTreeByCategoryListBuilder.ProcessItems(
  const ASource: IInterface;
  AList: TStringList
);
var
  VList: IInterfaceListStatic;
  i: Integer;
begin
  inherited;
  VList := ASource as IInterfaceListStatic;
  for i := 0 to VList.Count - 1 do begin
    ProcessItem(ASource, VList.Items[i], AList);
  end;
end;

{ TMarkSystem }

constructor TMarkSystem.Create(
  const ABasePath: IPathConfig;
  const AMarkPictureList: IMarkPictureList;
  const AMarkFactory: IMarkFactory;
  const AMarkCategoryFactory: IMarkCategoryFactory;
  const AHashFunction: IHashFunction;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
  const AVectorItemsFactory: IVectorItemsFactory;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const APerfCounterList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AHintConverter: IHtmlToHintTextConverter
);
var
  VPerfCounterList: IInternalPerformanceCounterList;
  VLoadDbCounter: IInternalPerformanceCounter;
  VSaveDbCounter: IInternalPerformanceCounter;
begin
  inherited Create;
  FMarkPictureList := AMarkPictureList;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;

  if APerfCounterList <> nil then begin
    VPerfCounterList := APerfCounterList.CreateAndAddNewSubList('MarksDb');
    VLoadDbCounter := VPerfCounterList.CreateAndAddNewCounter('LoadDb');
    VSaveDbCounter := VPerfCounterList.CreateAndAddNewCounter('SaveDb');
  end;
  FSystemImpl :=
    TMarkSystemImplChangeable.Create(
      ABasePath,
      AMarkPictureList,
      AHashFunction,
      AAppearanceOfMarkFactory,
      AVectorItemsFactory,
      AVectorItemSubsetBuilderFactory,
      AMarkFactory,
      VLoadDbCounter,
      VSaveDbCounter,
      AAppStartedNotifier,
      AHintConverter
    );
  FMarkDb := TMarkDbByImpl.Create(FSystemImpl, AMarkFactory);
  FCategoryDB := TMarkCategoryDbByImpl.Create(FSystemImpl, AMarkCategoryFactory);

  FCategoryTreeBuilder := TStaticTreeByCategoryListBuilder.Create('\', '');
end;

function TMarkSystem.CategoryListToStaticTree(
  const AList: IInterfaceListStatic): IStaticTreeItem;
begin
  Result := FCategoryTreeBuilder.BuildStatic(AList);
end;

procedure TMarkSystem.DeleteCategoryWithMarks(const ACategory: IMarkCategory);
var
  VMarkIdList: IInterfaceListStatic;
begin
  VMarkIdList := FMarkDb.GetMarkIdListByCategory(ACategory);
  FMarkDb.UpdateMarkList(VMarkIdList, nil);
  FCategoryDB.UpdateCategory(ACategory, nil);
end;

function TMarkSystem.GetCategoryDB: IMarkCategoryDB;
begin
  Result := FCategoryDB;
end;

function TMarkSystem.GetMarkByStringId(const AId: string): IMark;
var
  VImpl: IMarkSystemImpl;
begin
  Result := nil;
  if AId <> '' then begin
    VImpl := FSystemImpl.GetStatic;
    if VImpl <> nil then begin
      Result := VImpl.GetMarkByStringId(AId);
    end;
  end;
end;

function TMarkSystem.GetMarkCategoryByStringId(
  const AId: string): IMarkCategory;
var
  VImpl: IMarkSystemImpl;
begin
  Result := nil;
  if AId <> '' then begin
    VImpl := FSystemImpl.GetStatic;
    if VImpl <> nil then begin
      Result := VImpl.GetMarkCategoryByStringId(AId);
    end;
  end;
end;

function TMarkSystem.GetMarkDb: IMarkDb;
begin
  Result := FMarkDb;
end;

function TMarkSystem.GetState: IReadWriteStateChangeble;
begin
  Result := FSystemImpl.State;
end;

function TMarkSystem.GetStringIdByMark(const AMark: IMark): string;
var
  VImpl: IMarkSystemImpl;
begin
  Result := '';
  if AMark <> nil then begin
    VImpl := FSystemImpl.GetStatic;
    if VImpl <> nil then begin
      Result := VImpl.GetStringIdByMark(AMark);
    end;
  end;
end;

function TMarkSystem.GetVisibleCategories(AZoom: Byte): IInterfaceListStatic;
var
  VTmp: IInterfaceListSimple;
  VList: IInterfaceListStatic;
  VCategory: IMarkCategory;
  i: Integer;
begin
  Result := nil;
  VTmp := TInterfaceListSimple.Create;
  VList := FCategoryDB.GetCategoriesList;
  for i := 0 to VList.Count - 1 do begin
    VCategory := IMarkCategory(VList[i]);
    if (VCategory.Visible) and
      (VCategory.AfterScale <= AZoom + 1) and
      (VCategory.BeforeScale >= AZoom + 1) then begin
      VTmp.Add(VCategory);
    end;
  end;
  Result := VTmp.MakeStaticAndClear;
end;

function TMarkSystem.GetVisibleCategoriesIgnoreZoom: IInterfaceListStatic;
var
  VTmp: IInterfaceListSimple;
  VList: IInterfaceListStatic;
  VCategory: IMarkCategory;
  i: Integer;
begin
  VTmp := TInterfaceListSimple.Create;
  VList := FCategoryDB.GetCategoriesList;
  for i := 0 to VList.Count - 1 do begin
    VCategory := IMarkCategory(VList[i]);
    if VCategory.Visible then begin
      VTmp.Add(VCategory);
    end;
  end;
  Result := VTmp.MakeStaticAndClear;
end;

function TMarkSystem.ImportItemsTree(
  const ADataItemTree: IVectorItemTree;
  const AImportConfig: IImportConfig
): IInterfaceListStatic;
var
  VMarkList: IInterfaceListSimple;
begin
  Assert(Assigned(ADataItemTree));
  Assert(Assigned(AImportConfig));
  Assert(AImportConfig.CategoryParams.IsAddAllInRootCategory);
  Result := nil;
  VMarkList := TInterfaceListSimple.Create;
  PrepareFromTreeForImport(VMarkList, ADataItemTree, AImportConfig);
  if VMarkList.Count > 0 then begin
    Result := FMarkDb.UpdateMarkList(nil, VMarkList.MakeStaticAndClear);
  end;
end;

procedure TMarkSystem.PrepareFromTreeForImport(
  const AMarkList: IInterfaceListSimple;
  const ADataItemTree: IVectorItemTree;
  const AImportConfig: IImportConfig
);
var
  VItem: IVectorDataItemSimple;
  VPoint: IVectorDataItemPoint;
  VLine: IVectorDataItemLine;
  VPoly: IVectorDataItemPoly;
  i: Integer;
  VMark: IMark;
  VName: string;
  VCategory: ICategory;
begin
  VCategory := AImportConfig.RootCategory;

  for i := 0 to ADataItemTree.Items.Count - 1 do begin
    VMark := nil;
    VItem := ADataItemTree.Items.Items[i];
    VName := VItem.Name;
    if (VName = '') and (ADataItemTree.Name <> '') then begin
      if ADataItemTree.Items.Count > 1 then begin
        VName := ADataItemTree.Name + '-' + IntToStr(i + 1);
      end else begin
        VName := ADataItemTree.Name;
      end;
    end else begin
      if AImportConfig.CategoryParams.IsIgnoreMarkIfExistsWithSameNameInCategory then begin
        if FMarkDb.GetMarkByName(VName, VCategory) <> nil then begin
          Continue;
        end;
      end;
    end;
    if Supports(VItem, IVectorDataItemPoint, VPoint) then begin
      VMark :=
        FMarkDb.Factory.PreparePoint(
          VPoint,
          VName,
          AImportConfig.PointParams,
          VCategory
        );
    end else if Supports(VItem, IVectorDataItemLine, VLine) then begin
      VMark :=
        FMarkDb.Factory.PrepareLine(
          VLine,
          VName,
          AImportConfig.LineParams,
          VCategory
        );
    end else if Supports(VItem, IVectorDataItemPoly, VPoly) then begin
      VMark :=
        FMarkDb.Factory.PreparePoly(
          VPoly,
          VName,
          AImportConfig.PolyParams,
          VCategory
        );
    end;
    if VMark <> nil then begin
      AMarkList.Add(VMark);
    end;
  end;
  for i := 0 to ADataItemTree.SubTreeItemCount - 1 do begin
    PrepareFromTreeForImport(
      AMarkList,
      ADataItemTree.GetSubTreeItem(i),
      AImportConfig
    );
  end;
end;

function TMarkSystem.CategoryTreeToMarkTree(
  const ACategoryTree: IStaticTreeItem
): IVectorItemTree;
var
  VCategory: IMarkCategory;
  VMarkSubset: IVectorItemSubset;
  VSubItems: IInterfaceListStatic;
  i: Integer;
  VTemp: IInterfaceListSimple;
begin
  Assert(Assigned(ACategoryTree));
  Result := nil;
  VMarkSubset := nil;
  if Assigned(ACategoryTree) then begin
    if Supports(ACategoryTree.Data, IMarkCategory, VCategory) then begin
      VMarkSubset := FMarkDb.GetMarkSubsetByCategory(VCategory, True);
    end;

    VSubItems := nil;
    if ACategoryTree.SubItemCount > 0 then begin
      VTemp := TInterfaceListSimple.Create;
      for i := 0 to ACategoryTree.SubItemCount - 1 do begin
        VTemp.Add(CategoryTreeToMarkTree(ACategoryTree.SubItem[i]));
      end;
      VSubItems := VTemp.MakeStaticAndClear;
    end;

    Result :=
      TVectorItemTree.Create(
        ACategoryTree.Name,
        VMarkSubset,
        VSubItems
      );
  end;
end;

end.
