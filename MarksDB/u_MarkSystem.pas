{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_MarkSystem;

interface

uses
  i_VectorDataItemSimple,
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
  i_GeometryLonLatFactory,
  i_InterfaceListSimple,
  i_InternalPerformanceCounter,
  i_NotifierOperation,
  i_HtmlToHintTextConverter,
  i_MarkSystemImplChangeable,
  i_MarkFactory,
  i_MarkCategoryFactory,
  u_BaseInterfacedObject;

type
  TMarkSystem = class(TBaseInterfacedObject, IMarkSystem)
  private
    FMarkPictureList: IMarkPictureList;
    FSystemImpl: IMarkSystemImplChangeable;
    FMarkDb: IMarkDb;
    FCategoryDB: IMarkCategoryDB;
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;

    procedure PrepareFromTreeForImport(
      const AMarkList: IInterfaceListSimple;
      const ADataItemTree: IVectorItemTree;
      const AImportConfig: IImportConfig
    );

  private
    function GetState: IReadWriteStateChangeble;
    function GetMarkDb: IMarkDb;
    function GetCategoryDB: IMarkCategoryDB;

    function GetStringIdByMark(const AMark: IVectorDataItem): string;
    function GetMarkByStringId(const AId: string): IVectorDataItem;
    function GetMarkCategoryByStringId(const AId: string): IMarkCategory;

    procedure DeleteCategoryWithMarks(const ACategory: IMarkCategory);

    function ImportItemsTree(
      const ADataItemTree: IVectorItemTree;
      const AImportConfig: IImportConfig
    ): IInterfaceListStatic;

    function CategoryTreeToMarkTree(
      const ACategoryTree: IStaticTreeItem;
      const AIncludeHiddenMarks: Boolean
    ): IVectorItemTree;
  public
    constructor Create(
      const ABasePath: IPathConfig;
      const AMarkPictureList: IMarkPictureList;
      const AMarkFactory: IMarkFactory;
      const AMarkCategoryFactory: IMarkCategoryFactory;
      const AHashFunction: IHashFunction;
      const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
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
  i_GeometryLonLat,
  i_MarkSystemImpl,
  u_InterfaceListSimple,
  u_VectorItemTree,
  u_MarkDbByImpl,
  u_MarkCategoryDbByImpl,
  u_MarkSystemImplChangeable;

{ TMarkSystem }

constructor TMarkSystem.Create(
  const ABasePath: IPathConfig;
  const AMarkPictureList: IMarkPictureList;
  const AMarkFactory: IMarkFactory;
  const AMarkCategoryFactory: IMarkCategoryFactory;
  const AHashFunction: IHashFunction;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
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
      AVectorGeometryLonLatFactory,
      AVectorItemSubsetBuilderFactory,
      AMarkFactory,
      VLoadDbCounter,
      VSaveDbCounter,
      AAppStartedNotifier,
      AHintConverter
    );
  FMarkDb := TMarkDbByImpl.Create(FSystemImpl, AMarkFactory);
  FCategoryDB := TMarkCategoryDbByImpl.Create(FSystemImpl, AMarkCategoryFactory);
end;

procedure TMarkSystem.DeleteCategoryWithMarks(const ACategory: IMarkCategory);
var
  VMarkIdList: IInterfaceListStatic;
begin
  VMarkIdList := FMarkDb.GetMarkIdListByCategory(ACategory);
  if Assigned(VMarkIdList) and (VMarkIdList.Count > 0) then begin
    FMarkDb.UpdateMarkList(VMarkIdList, nil);
  end;
  FCategoryDB.UpdateCategory(ACategory, nil);
end;

function TMarkSystem.GetCategoryDB: IMarkCategoryDB;
begin
  Result := FCategoryDB;
end;

function TMarkSystem.GetMarkByStringId(const AId: string): IVectorDataItem;
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

function TMarkSystem.GetStringIdByMark(const AMark: IVectorDataItem): string;
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
  VItem: IVectorDataItem;
  i: Integer;
  VMark: IVectorDataItem;
  VName: string;
  VCategory: ICategory;
  VParams: IImportMarkParams;
begin
  VCategory := AImportConfig.RootCategory;

  if Assigned(ADataItemTree.Items) then begin
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
      VParams := nil;
      if Supports(VItem.Geometry, IGeometryLonLatPoint) then begin
        VParams := AImportConfig.PointParams;
      end else if Supports(VItem.Geometry, IGeometryLonLatSingleLine) then begin
        VParams := AImportConfig.LineParams;
      end else if Supports(VItem.Geometry, IGeometryLonLatMultiLine) then begin
        VParams := AImportConfig.LineParams;
      end else if Supports(VItem.Geometry, IGeometryLonLatSinglePolygon) then begin
        VParams := AImportConfig.PolyParams;
      end else if Supports(VItem.Geometry, IGeometryLonLatMultiPolygon) then begin
        VParams := AImportConfig.PolyParams;
      end;
      VMark :=
        FMarkDb.Factory.PrepareMark(
          VItem,
          VName,
          VParams,
          VCategory
        );
      if VMark <> nil then begin
        AMarkList.Add(VMark);
      end;
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
  const ACategoryTree: IStaticTreeItem;
  const AIncludeHiddenMarks: Boolean
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
      VMarkSubset := FMarkDb.GetMarkSubsetByCategory(VCategory, AIncludeHiddenMarks);
    end;

    VSubItems := nil;
    if ACategoryTree.SubItemCount > 0 then begin
      VTemp := TInterfaceListSimple.Create;
      for i := 0 to ACategoryTree.SubItemCount - 1 do begin
        VTemp.Add(CategoryTreeToMarkTree(ACategoryTree.SubItem[i], AIncludeHiddenMarks));
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
