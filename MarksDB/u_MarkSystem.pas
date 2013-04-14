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
  i_VectorItemSubset,
  i_StaticTreeItem,
  i_PathConfig,
  i_MarkPicture,
  i_VectorItemsFactory,
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

    FCategoryTreeBuilder: IStaticTreeBuilder;
    FMarkSubsetTreeBuilder: IStaticTreeBuilder;
  private
    function GetState: IReadWriteStateChangeble;
    function GetMarkDb: IMarkDb;
    function GetCategoryDB: IMarkCategoryDB;

    function GetMarkByStringId(const AId: string): IMark;
    function GetMarkCategoryByStringId(const AId: string): IMarkCategory;

    function GetVisibleCategories(AZoom: Byte): IInterfaceList;
    function GetVisibleCategoriesIgnoreZoom: IInterfaceList;
    procedure DeleteCategoryWithMarks(const ACategory: IMarkCategory);

    function ImportItemsList(
      const ADataItemList: IVectorItemSubset;
      const AImportConfig: IImportConfig;
      const ANamePrefix: string
    ): IInterfaceList;

    function MarkSubsetToStaticTree(const ASubset: IVectorItemSubset): IStaticTreeItem;
    function CategoryListToStaticTree(const AList: IInterfaceList): IStaticTreeItem;
  public
    constructor Create(
      const ABasePath: IPathConfig;
      const AMarkPictureList: IMarkPictureList;
      const AMarkFactory: IMarkFactory;
      const AMarkCategoryFactory: IMarkCategoryFactory;
      const AVectorItemsFactory: IVectorItemsFactory;
      const APerfCounterList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AHintConverter: IHtmlToHintTextConverter
    );

  end;

implementation

uses
  ActiveX,
  SysUtils,
  i_Category,
  i_VectorDataItemSimple,
  i_MarkSystemImpl,
  u_StaticTreeBuilderBase,
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
  VList: IInterfaceList;
  i: Integer;
begin
  inherited;
  VList := ASource as IInterfaceList;
  for i := 0 to VList.Count - 1 do begin
    ProcessItem(ASource, VList.Items[i], AList);
  end;
end;

type
  TStaticTreeByMarkSubsetBuilder = class(TStaticTreeBuilderBaseBySlash)
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

{ TStaticTreeByMarkSubsetBuilder }

function TStaticTreeByMarkSubsetBuilder.GetNameFromItem(
  const ASource: IInterface;
  const AItem: IInterface
): string;
var
  VMark: IMark;
begin
  VMark := AItem as IMark;
  if VMark.Category <> nil then begin
    Result := VMark.Category.Name + LevelsSeparator + VMark.Name;
  end else begin
    Result := LevelsSeparator + VMark.Name;
  end;
end;

procedure TStaticTreeByMarkSubsetBuilder.ProcessItems(
  const ASource: IInterface;
  AList: TStringList
);
var
  VSubset: IVectorItemSubset;
  VEnum: IEnumUnknown;
  VMark: IMark;
  i: Cardinal;
begin
  inherited;
  VSubset := ASource as IVectorItemSubset;
  VEnum := VSubset.GetEnum;
  while (VEnum.Next(1, VMark, @i) = S_OK) do begin
    ProcessItem(ASource, VMark, AList);
  end;
end;

{ TMarkSystem }

constructor TMarkSystem.Create(
  const ABasePath: IPathConfig;
  const AMarkPictureList: IMarkPictureList;
  const AMarkFactory: IMarkFactory;
  const AMarkCategoryFactory: IMarkCategoryFactory;
  const AVectorItemsFactory: IVectorItemsFactory;
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

  if APerfCounterList <> nil then begin
    VPerfCounterList := APerfCounterList.CreateAndAddNewSubList('MarksDb');
    VLoadDbCounter := VPerfCounterList.CreateAndAddNewCounter('LoadDb');
    VSaveDbCounter := VPerfCounterList.CreateAndAddNewCounter('SaveDb');
  end;
  FSystemImpl :=
    TMarkSystemImplChangeable.Create(
      ABasePath,
      AMarkPictureList,
      AVectorItemsFactory,
      VLoadDbCounter,
      VSaveDbCounter,
      AAppStartedNotifier,
      AHintConverter
    );
  FMarkDb := TMarkDbByImpl.Create(FSystemImpl, AMarkFactory);
  FCategoryDB := TMarkCategoryDbByImpl.Create(FSystemImpl, AMarkCategoryFactory);

  FCategoryTreeBuilder := TStaticTreeByCategoryListBuilder.Create('\', '');
  FMarkSubsetTreeBuilder := TStaticTreeByMarkSubsetBuilder.Create('\', '');
end;

function TMarkSystem.CategoryListToStaticTree(
  const AList: IInterfaceList): IStaticTreeItem;
begin
  Result := FCategoryTreeBuilder.BuildStatic(AList);
end;

procedure TMarkSystem.DeleteCategoryWithMarks(const ACategory: IMarkCategory);
var
  VMarkIdList: IInterfaceList;
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

function TMarkSystem.GetVisibleCategories(AZoom: Byte): IInterfaceList;
var
  VList: IInterfaceList;
  VCategory: IMarkCategory;
  i: Integer;
begin
  Result := TInterfaceList.Create;
  VList := FCategoryDB.GetCategoriesList;
  for i := 0 to VList.Count - 1 do begin
    VCategory := IMarkCategory(VList[i]);
    if (VCategory.Visible) and
      (VCategory.AfterScale <= AZoom + 1) and
      (VCategory.BeforeScale >= AZoom + 1) then begin
      Result.Add(VCategory);
    end;
  end;
end;

function TMarkSystem.GetVisibleCategoriesIgnoreZoom: IInterfaceList;
var
  VList: IInterfaceList;
  VCategory: IMarkCategory;
  i: Integer;
begin
  Result := TInterfaceList.Create;
  VList := FCategoryDB.GetCategoriesList;
  for i := 0 to VList.Count - 1 do begin
    VCategory := IMarkCategory(VList[i]);
    if VCategory.Visible then begin
      Result.Add(VCategory);
    end;
  end;
end;

function TMarkSystem.ImportItemsList(const ADataItemList: IVectorItemSubset;
  const AImportConfig: IImportConfig;
  const ANamePrefix: string): IInterfaceList;
var
  VItem: IVectorDataItemSimple;
  VPoint: IVectorDataItemPoint;
  VLine: IVectorDataItemLine;
  VPoly: IVectorDataItemPoly;
  i: Integer;
  VMark: IMark;
  VMarkList: IInterfaceList;
  VName: string;
  VPic: IMarkPicture;
  VCategory: ICategory;
begin
  Result := nil;

  VPic := nil;
  if AImportConfig.PointParams <> nil then begin
    VPic :=
      FMarkPictureList.FindByNameOrDefault(
        AImportConfig.PointParams.Template.PicName
      );
  end;
  VCategory := AImportConfig.RootCategory;

  VMarkList := TInterfaceList.Create;
  for i := 0 to ADataItemList.Count - 1 do begin
    VMark := nil;
    VItem := ADataItemList.GetItem(i);
    VName := VItem.Name;
    if (VName = '') and (ANamePrefix <> '') then begin
      if ADataItemList.Count > 1 then begin
        VName := ANamePrefix + '-' + IntToStr(i + 1);
      end else begin
        VName := ANamePrefix;
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
          VCategory,
          VPic
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
      VMarkList.Add(VMark);
    end;
  end;
  if VMarkList.Count > 0 then begin
    Result := FMarkDb.UpdateMarkList(nil, VMarkList);
  end;
end;

function TMarkSystem.MarkSubsetToStaticTree(
  const ASubset: IVectorItemSubset): IStaticTreeItem;
begin
  Result := FMarkSubsetTreeBuilder.BuildStatic(ASubset);
end;

end.
