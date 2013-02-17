{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_MarksSystem;

interface

uses
  Windows,
  Classes,
  i_PathConfig,
  i_LanguageManager,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_VectorItemsFactory,
  i_InternalPerformanceCounter,
  i_ReadWriteState,
  i_MarksSimple,
  i_MarkPicture,
  i_VectorDataItemSimple,
  i_ImportConfig,
  i_HtmlToHintTextConverter,
  i_MarksFactoryConfig,
  i_MarkCategory,
  i_MarkCategoryFactoryConfig,
  i_MarkCategoryDB,
  i_MarkCategoryDBSmlInternal,
  i_MarksDb,
  i_MarksSystem,
  i_MarksDbSmlInternal,
  i_StaticTreeItem,
  i_StaticTreeBuilder,
  u_BaseInterfacedObject;

type
  TMarksSystem = class(TBaseInterfacedObject, IMarksSystem)
  private
    FBasePath: IPathConfig;
    FMarksFactoryConfig: IMarksFactoryConfig;
    FState: IReadWriteStateChangeble;

    FMarksDb: IMarksDb;
    FMarksDbInternal: IMarksDbSmlInternal;
    FCategoryDB: IMarkCategoryDB;
    FCategoryDBInternal: IMarkCategoryDBSmlInternal;
    FCategoryTreeBuilder: IStaticTreeBuilder;
    FMarksSubsetTreeBuilder: IStaticTreeBuilder;
  private
    function GetState: IReadWriteStateChangeble;
    function GetMarksDb: IMarksDb;
    function GetCategoryDB: IMarkCategoryDB;
    function GetMarksFactoryConfig: IMarksFactoryConfig;

    function GetMarkByStringId(const AId: string): IMark;
    function GetMarkCategoryByStringId(const AId: string): IMarkCategory;

    function GetVisibleCategories(AZoom: Byte): IInterfaceList;
    function GetVisibleCategoriesIgnoreZoom: IInterfaceList;
    procedure DeleteCategoryWithMarks(const ACategory: IMarkCategory);

    function ImportItemsList(
      const ADataItemList: IVectorDataItemList;
      const AImportConfig: IImportConfig;
      const ANamePrefix: string
    ): IInterfaceList;

    function MarksSubsetToStaticTree(const ASubset: IMarksSubset): IStaticTreeItem;
    function CategoryListToStaticTree(const AList: IInterfaceList): IStaticTreeItem;

    procedure ReadConfig(const AConfigData: IConfigDataProvider);
    procedure WriteConfig(const AConfigData: IConfigDataWriteProvider);
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const ABasePath: IPathConfig;
      const AMarkPictureList: IMarkPictureList;
      const AVectorItemsFactory: IVectorItemsFactory;
      const APerfCounterList: IInternalPerformanceCounterList;
      const AHintConverter: IHtmlToHintTextConverter;
      const AFactoryConfig: IMarksFactoryConfig;
      const ACategoryFactoryConfig: IMarkCategoryFactoryConfig
    );
    destructor Destroy; override;
  end;


implementation

uses
  SysUtils,
  ActiveX,
  i_MarkFactory,
  i_MarkTemplate,
  u_StaticTreeBuilderBase,
  u_ReadWriteStateInternal,
  u_MarksSubset,
  u_MarksDb,
  u_MarkCategoryDB;

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
  TStaticTreeByMarksSubsetBuilder = class(TStaticTreeBuilderBaseBySlash)
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

{ TStaticTreeByMarksSubsetBuilder }

function TStaticTreeByMarksSubsetBuilder.GetNameFromItem(
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

procedure TStaticTreeByMarksSubsetBuilder.ProcessItems(
  const ASource: IInterface;
  AList: TStringList
);
var
  VSubset: IMarksSubset;
  VEnum: IEnumUnknown;
  VMark: IMark;
  i: Cardinal;
begin
  inherited;
  VSubset := ASource as IMarksSubset;
  VEnum := VSubset.GetEnum;
  while (VEnum.Next(1, VMark, @i) = S_OK) do begin
    ProcessItem(ASource, VMark, AList);
  end;
end;

{ TMarksSystem }

constructor TMarksSystem.Create(
  const ALanguageManager: ILanguageManager;
  const ABasePath: IPathConfig;
  const AMarkPictureList: IMarkPictureList;
  const AVectorItemsFactory: IVectorItemsFactory;
  const APerfCounterList: IInternalPerformanceCounterList;
  const AHintConverter: IHtmlToHintTextConverter;
  const AFactoryConfig: IMarksFactoryConfig;
  const ACategoryFactoryConfig: IMarkCategoryFactoryConfig
);
var
  VCategoryDb: TMarkCategoryDB;
  VMarksDb: TMarksDb;
  VState: TReadWriteStateInternal;
begin
  inherited Create;
  FBasePath := ABasePath;
  VState := TReadWriteStateInternal.Create;
  FState := VState;
  VCategoryDb := TMarkCategoryDB.Create(VState, FBasePath, ACategoryFactoryConfig);
  FCategoryDB := VCategoryDb;
  FCategoryDBInternal := VCategoryDb;
  FMarksFactoryConfig := AFactoryConfig;
  VMarksDb :=
    TMarksDb.Create(
      VState,
      ABasePath,
      AMarkPictureList,
      FCategoryDBInternal,
      APerfCounterList.CreateAndAddNewSubList('MarksDb'),
      AVectorItemsFactory,
      AHintConverter,
      FMarksFactoryConfig
    );
  FMarksDb := VMarksDb;
  FMarksDbInternal := VMarksDb;
  FCategoryTreeBuilder := TStaticTreeByCategoryListBuilder.Create('\', '');
  FMarksSubsetTreeBuilder := TStaticTreeByMarksSubsetBuilder.Create('\', '');
end;

destructor TMarksSystem.Destroy;
begin
  FMarksDb := nil;
  FMarksDbInternal := nil;
  FCategoryDB := nil;
  FCategoryDBInternal := nil;
  FMarksFactoryConfig := nil;
  inherited;
end;

function TMarksSystem.CategoryListToStaticTree(
  const AList: IInterfaceList
): IStaticTreeItem;
begin
  Result := FCategoryTreeBuilder.BuildStatic(AList);
end;

procedure TMarksSystem.DeleteCategoryWithMarks(const ACategory: IMarkCategory);
var
  VMarkIdList: IInterfaceList;
begin
  VMarkIdList := FMarksDb.GetMarksIdListByCategory(ACategory);
  FMarksDb.UpdateMarksList(VMarkIdList, nil);
  FCategoryDB.UpdateCategory(ACategory, nil);
end;

function TMarksSystem.GetCategoryDB: IMarkCategoryDB;
begin
  Result := FCategoryDB;
end;

function TMarksSystem.GetMarkByStringId(const AId: string): IMark;
var
  VId: Integer;
begin
  Result := nil;
  if AId <> '' then begin
    if TryStrToInt(AId, VId) then begin
      if not Supports(FMarksDbInternal.GetById(VId), IMark, Result) then begin
        Result := nil;
      end;
    end;
  end;
end;

function TMarksSystem.GetMarkCategoryByStringId(
  const AId: string): IMarkCategory;
var
  VId: Integer;
begin
  Result := nil;
  if AId <> '' then begin
    if TryStrToInt(AId, VId) then begin
      if not Supports(FCategoryDBInternal.GetCategoryByID(VId), IMarkCategory, Result) then begin
        Result := nil;
      end;
    end;
  end;
end;

function TMarksSystem.GetMarksDb: IMarksDb;
begin
  Result := FMarksDb;
end;

function TMarksSystem.GetMarksFactoryConfig: IMarksFactoryConfig;
begin
  Result := FMarksFactoryConfig;
end;

function TMarksSystem.GetState: IReadWriteStateChangeble;
begin
  Result := FState;
end;

function TMarksSystem.GetVisibleCategories(AZoom: Byte): IInterfaceList;
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

function TMarksSystem.GetVisibleCategoriesIgnoreZoom: IInterfaceList;
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

function TMarksSystem.ImportItemsList(
  const ADataItemList: IVectorDataItemList;
  const AImportConfig: IImportConfig;
  const ANamePrefix: string
): IInterfaceList;
var
  VItem: IVectorDataItemSimple;
  VPoint: IVectorDataItemPoint;
  VLine: IVectorDataItemLine;
  VPoly: IVectorDataItemPoly;
  i: Integer;
  VFactory: IMarkFactory;
  VMark: IMark;
  VTemplateNewPoint: IMarkTemplatePoint;
  VTemplateNewLine: IMarkTemplateLine;
  VTemplateNewPoly: IMarkTemplatePoly;
  VMarksList: IInterfaceList;
  VName: string;
begin
  Result := nil;
  VFactory := FMarksDb.Factory;
  VTemplateNewPoint := AImportConfig.TemplateNewPoint;
  VTemplateNewLine := AImportConfig.TemplateNewLine;
  VTemplateNewPoly := AImportConfig.TemplateNewPoly;
  VMarksList := TInterfaceList.Create;
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
    end;
    if Supports(VItem, IVectorDataItemPoint, VPoint) then begin
      if VTemplateNewPoint <> nil then begin
        VMark :=
          VFactory.CreateNewPoint(
            VPoint.Point,
            VName,
            VPoint.Desc,
            VTemplateNewPoint
          );
      end;
    end else if Supports(VItem, IVectorDataItemLine, VLine) then begin
      if VTemplateNewLine <> nil then begin
        VMark :=
          VFactory.CreateNewLine(
            VLine.Line,
            VName,
            VLine.Desc,
            VTemplateNewLine
          );
      end;
    end else if Supports(VItem, IVectorDataItemPoly, VPoly) then begin
      if VTemplateNewPoly <> nil then begin
        VMark :=
          VFactory.CreateNewPoly(
            VPoly.Line,
            VName,
            VPoly.Desc,
            VTemplateNewPoly
          );
      end;
    end;
    if VMark <> nil then begin
      VMarksList.Add(VMark);
    end;
  end;
  if VMarksList.Count > 0 then begin
    Result := FMarksDb.UpdateMarksList(nil, VMarksList);
  end;
end;

function TMarksSystem.MarksSubsetToStaticTree(
  const ASubset: IMarksSubset
): IStaticTreeItem;
begin
  Result := FMarksSubsetTreeBuilder.BuildStatic(ASubset);
end;

procedure TMarksSystem.ReadConfig(const AConfigData: IConfigDataProvider);
begin
  FCategoryDBInternal.LoadCategoriesFromFile;
  FMarksDbInternal.LoadMarksFromFile;
end;

procedure TMarksSystem.WriteConfig(const AConfigData: IConfigDataWriteProvider);
begin
  FCategoryDBInternal.SaveCategory2File;
  FMarksDbInternal.SaveMarks2File;
end;

end.
