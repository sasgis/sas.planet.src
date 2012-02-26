{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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
  i_LanguageManager,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_VectorItmesFactory,
  i_MarksSimple,
  i_MarkPicture,
  i_HtmlToHintTextConverter,
  i_MarksFactoryConfig,
  i_MarkCategory,
  i_MarkCategoryFactoryConfig,
  i_MarkCategoryDB,
  i_MarkCategoryDBSmlInternal,
  i_MarksDb,
  i_MarksDbSmlInternal,
  i_StaticTreeItem,
  i_StaticTreeBuilder;

type
  TMarksSystem = class
  private
    FBasePath: string;
    FMarksFactoryConfig: IMarksFactoryConfig;
    FMarksDb: IMarksDb;
    FMarksDbInternal: IMarksDbSmlInternal;
    FCategoryDB: IMarkCategoryDB;
    FCategoryDBInternal: IMarkCategoryDBSmlInternal;
    FCategoryTreeBuilder: IStaticTreeBuilder;
    FMarksSubsetTreeBuilder: IStaticTreeBuilder;
  public
    constructor Create(
      ALanguageManager: ILanguageManager;
      const ABasePath: string;
      AMarkPictureList: IMarkPictureList;
      AVectorItmesFactory: IVectorItmesFactory;
      AHintConverter: IHtmlToHintTextConverter;
      ACategoryFactoryConfig: IMarkCategoryFactoryConfig
    );
    destructor Destroy; override;

    procedure ReadConfig(AConfigData: IConfigDataProvider);
    procedure WriteConfig(AConfigData: IConfigDataWriteProvider);

    property MarksDb: IMarksDb read FMarksDb;
    property CategoryDB: IMarkCategoryDB read FCategoryDB;
    property MarksFactoryConfig: IMarksFactoryConfig read FMarksFactoryConfig;

    function GetVisibleCategories(AZoom: Byte): IInterfaceList;
    function GetVisibleCategoriesIgnoreZoom: IInterfaceList;
    procedure DeleteCategoryWithMarks(ACategory: IMarkCategory);

    function MarksSubsetToStaticTree(ASubset: IMarksSubset): IStaticTreeItem;
    function CategoryListToStaticTree(AList: IInterfaceList): IStaticTreeItem;
  end;


implementation

uses
  ActiveX,
  u_StaticTreeBuilderBase,
  u_MarksDb,
  u_MarkCategoryDB,
  u_MarksFactoryConfig;

type
  TStaticTreeByCategoryListBuilder = class(TStaticTreeBuilderBaseBySlash)
  protected
    procedure ProcessItems(ASource: IInterface; AList: TStringList); override;
    function GetNameFromItem(
      ASource: IInterface;
      AItem: IInterface
    ): string; override;
  end;

{ TStaticTreeByCategoryListBuilder }

function TStaticTreeByCategoryListBuilder.GetNameFromItem(
  ASource: IInterface;
  AItem: IInterface
): string;
begin
  Result := (AItem as ICategory).Name;
end;

procedure TStaticTreeByCategoryListBuilder.ProcessItems(
  ASource: IInterface;
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
    procedure ProcessItems(ASource: IInterface; AList: TStringList); override;
    function GetNameFromItem(
      ASource: IInterface;
      AItem: IInterface
    ): string; override;
  end;

{ TStaticTreeByMarksSubsetBuilder }

function TStaticTreeByMarksSubsetBuilder.GetNameFromItem(
  ASource: IInterface;
  AItem: IInterface
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

procedure TStaticTreeByMarksSubsetBuilder.ProcessItems(ASource: IInterface;
  AList: TStringList);
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
  ALanguageManager: ILanguageManager;
  const ABasePath: string;
  AMarkPictureList: IMarkPictureList;
  AVectorItmesFactory: IVectorItmesFactory;
  AHintConverter: IHtmlToHintTextConverter;
  ACategoryFactoryConfig: IMarkCategoryFactoryConfig
);
var
  VCategoryDb: TMarkCategoryDB;
  VMarksDb: TMarksDb;
begin
  FBasePath := ABasePath;
  VCategoryDB := TMarkCategoryDB.Create(ABasePath, ACategoryFactoryConfig);
  FCategoryDB := VCategoryDb;
  FCategoryDBInternal := VCategoryDb;
  FMarksFactoryConfig :=
    TMarksFactoryConfig.Create(
      ALanguageManager,
      FCategoryDBInternal,
      AMarkPictureList
    );
  VMarksDb :=
    TMarksDb.Create(
      ABasePath,
      FCategoryDBInternal,
      AVectorItmesFactory,
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
  AList: IInterfaceList
): IStaticTreeItem;
begin
  Result := FCategoryTreeBuilder.BuildStatic(AList);
end;

procedure TMarksSystem.DeleteCategoryWithMarks(ACategory: IMarkCategory);
var
  VMarkIdList: IInterfaceList;
begin
  VMarkIdList := FMarksDb.GetMarskIdListByCategory(ACategory);
  FMarksDb.UpdateMarksList(VMarkIdList, nil);
  FCategoryDB.DeleteCategory(ACategory);
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
    if
      (VCategory.visible) and
      (VCategory.AfterScale <= AZoom + 1) and
      (VCategory.BeforeScale >= AZoom + 1)
    then begin
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
    if VCategory.visible then begin
      Result.Add(VCategory);
    end;
  end;
end;

function TMarksSystem.MarksSubsetToStaticTree(
  ASubset: IMarksSubset
): IStaticTreeItem;
begin
  Result := FMarksSubsetTreeBuilder.BuildStatic(ASubset);
end;

procedure TMarksSystem.ReadConfig(AConfigData: IConfigDataProvider);
begin
  FMarksFactoryConfig.ReadConfig(AConfigData);
  FCategoryDBInternal.LoadCategoriesFromFile;
  FMarksDbInternal.LoadMarksFromFile;
end;

procedure TMarksSystem.WriteConfig(AConfigData: IConfigDataWriteProvider);
begin
  FMarksFactoryConfig.WriteConfig(AConfigData);
  FCategoryDBInternal.SaveCategory2File;
  FMarksDbInternal.SaveMarks2File;
end;

end.



