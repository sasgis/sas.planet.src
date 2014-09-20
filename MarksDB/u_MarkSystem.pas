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
      const AAppClosingNotifier: INotifierOneOperation;
      const AHintConverter: IHtmlToHintTextConverter
    );
  end;

implementation

uses
  SysUtils,
  i_Category,
  i_GeometryLonLat,
  i_MarkSystemImpl,
  i_MarkSystemImplFactory,
  u_InterfaceListSimple,
  u_VectorItemTree,
  u_MarkSystemImplFactorySML,
  u_MarkSystemImplFactoryChangeable,
  u_MarkDbByImpl,
  u_MarkCategoryDbByImpl,
  u_MarkSystemHelpers,
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
  const AAppClosingNotifier: INotifierOneOperation;
  const AHintConverter: IHtmlToHintTextConverter
);
var
  VPerfCounterList: IInternalPerformanceCounterList;
  VLoadDbCounter: IInternalPerformanceCounter;
  VSaveDbCounter: IInternalPerformanceCounter;
  VImplFactory: IMarkSystemImplFactory;
begin
  inherited Create;
  FMarkPictureList := AMarkPictureList;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;

  if APerfCounterList <> nil then begin
    VPerfCounterList := APerfCounterList.CreateAndAddNewSubList('MarksDb');
    VLoadDbCounter := VPerfCounterList.CreateAndAddNewCounter('LoadDb');
    VSaveDbCounter := VPerfCounterList.CreateAndAddNewCounter('SaveDb');
  end;

  VImplFactory :=
    TMarkSystemImplFactorySML.Create(
      AMarkPictureList,
      AHashFunction,
      AAppearanceOfMarkFactory,
      AVectorGeometryLonLatFactory,
      AVectorItemSubsetBuilderFactory,
      AMarkFactory,
      VLoadDbCounter,
      VSaveDbCounter,
      AHintConverter
    );
  FSystemImpl :=
    TMarkSystemImplChangeable.Create(
      ABasePath,
      TMarkSystemImplFactoryChangeableFaked.Create(VImplFactory),
      AAppStartedNotifier,
      AAppClosingNotifier
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
  VImpl: IMarkSystemImpl;
  VMarkList: IInterfaceListSimple;
begin
  Assert(Assigned(ADataItemTree));
  Assert(Assigned(AImportConfig));

  Result := nil;

  VImpl := FSystemImpl.GetStatic;
  if VImpl <> nil then begin
    VMarkList := TInterfaceListSimple.Create;

    PrepareFromTreeForImport(
      VMarkList,
      ADataItemTree,
      AImportConfig,
      VImpl.MarkDb,
      FMarkDb.Factory,
      VImpl.CategoryDB,
      FCategoryDB.Factory
    );

    if VMarkList.Count > 0 then begin
      Result := FMarkDb.UpdateMarkList(nil, VMarkList.MakeStaticAndClear);
    end;
  end;
end;

function TMarkSystem.CategoryTreeToMarkTree(
  const ACategoryTree: IStaticTreeItem;
  const AIncludeHiddenMarks: Boolean
): IVectorItemTree;
var
  VImpl: IMarkSystemImpl;
begin
  Result := nil;
  VImpl := FSystemImpl.GetStatic;
  if VImpl <> nil then begin
    Result :=
      CategoryTreeToMarkTreeHelper(
        VImpl.MarkDb,
        ACategoryTree,
        AIncludeHiddenMarks
      );
  end;
end;

end.
