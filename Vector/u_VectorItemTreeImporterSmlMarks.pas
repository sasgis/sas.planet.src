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

unit u_VectorItemTreeImporterSmlMarks;

interface

uses
  i_VectorItemTree,
  i_VectorItemTreeImporter,
  i_HashFunction,
  i_GeometryLonLatFactory,
  i_VectorItemSubsetBuilder,
  i_InternalPerformanceCounter,
  i_AppearanceOfMarkFactory,
  i_ReadWriteState,
  i_MarkPicture,
  i_HtmlToHintTextConverter,
  i_MarkCategory,
  i_MarkFactory,
  i_MarkDbImpl,
  i_MarkCategoryDBImpl,
  i_MarkSystemImpl,
  i_StaticTreeItem,
  u_BaseInterfacedObject;

type
  TVectorItemTreeImporterSmlMarks = class(TBaseInterfacedObject, IVectorItemTreeImporter)
  private
    FMarkPictureList: IMarkPictureList;
    FHashFunction: IHashFunction;
    FAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FMarkFactory: IMarkFactory;
    FLoadDbCounter: IInternalPerformanceCounter;
    FSaveDbCounter: IInternalPerformanceCounter;
    FHintConverter: IHtmlToHintTextConverter;
  private
    function CategoryTreeToMarkTree(
      const AMarkDB: IMarkDbImpl;
      const ACategoryTree: IStaticTreeItem;
      const AIncludeHiddenMarks: Boolean
    ): IVectorItemTree;
  private
    { IVectorItemTreeImporter }
    function ProcessImport(
        const AFileName: string
      ): IVectorItemTree;
  public
    constructor Create(
      const AMarkPictureList: IMarkPictureList;
      const AHashFunction: IHashFunction;
      const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AMarkFactory: IMarkFactory;
      const ALoadDbCounter: IInternalPerformanceCounter;
      const ASaveDbCounter: IInternalPerformanceCounter;
      const AHintConverter: IHtmlToHintTextConverter
    );
  end;

implementation

uses
  SysUtils,
  t_CommonTypes,  
  i_VectorItemSubset,
  i_InterfaceListSimple,
  i_StaticTreeBuilder,
  i_InterfaceListStatic,
  u_VectorItemTree,
  u_StaticTreeBuilderBase,
  u_InterfaceListSimple,
  u_MarkSystemSml;

{ TVectorItemTreeImporterSmlMarks }

constructor TVectorItemTreeImporterSmlMarks.Create(
  const AMarkPictureList: IMarkPictureList;
  const AHashFunction: IHashFunction;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AMarkFactory: IMarkFactory;
  const ALoadDbCounter: IInternalPerformanceCounter;
  const ASaveDbCounter: IInternalPerformanceCounter;
  const AHintConverter: IHtmlToHintTextConverter
);
begin
  inherited Create;
  FMarkPictureList := AMarkPictureList;
  FHashFunction := AHashFunction;
  FAppearanceOfMarkFactory := AAppearanceOfMarkFactory;
  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;
  FMarkFactory := AMarkFactory;
  FLoadDbCounter := ALoadDbCounter;
  FSaveDbCounter := ASaveDbCounter;
  FHintConverter := AHintConverter;
end;

function TVectorItemTreeImporterSmlMarks.CategoryTreeToMarkTree(
  const AMarkDB: IMarkDbImpl;
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
      VMarkSubset := AMarkDb.GetMarkSubsetByCategory(VCategory, AIncludeHiddenMarks);
    end;

    VSubItems := nil;
    if ACategoryTree.SubItemCount > 0 then begin
      VTemp := TInterfaceListSimple.Create;
      for i := 0 to ACategoryTree.SubItemCount - 1 do begin
        VTemp.Add(
          CategoryTreeToMarkTree(
            AMarkDB,
            ACategoryTree.SubItem[i],
            AIncludeHiddenMarks
          )
        );
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

function TVectorItemTreeImporterSmlMarks.ProcessImport(
  const AFileName: string
): IVectorItemTree;
var
  VSml: IMarkSystemImpl;
  VCategoiesList: IInterfaceListStatic;
  VCategoryTreeBuilder: IStaticTreeBuilder;
begin
  Result := nil;

  VSml := TMarkSystemSml.Create(
    ExtractFilePath(AFileName),
    FMarkPictureList,
    FHashFunction,
    FAppearanceOfMarkFactory,
    FVectorGeometryLonLatFactory,
    FVectorItemSubsetBuilderFactory,
    FMarkFactory,
    FLoadDbCounter,
    FSaveDbCounter,
    FHintConverter,
    True {ReadOnly}
  );

  if VSml.State.GetStatic.ReadAccess = asEnabled then begin
    VCategoiesList := VSml.CategoryDB.GetCategoriesList;
    if Assigned(VCategoiesList) then begin
      VCategoryTreeBuilder := TStaticTreeByCategoryListBuilder.Create('\', '');
      Result := CategoryTreeToMarkTree(
        VSml.MarkDb,
        VCategoryTreeBuilder.BuildStatic(VCategoiesList),
        True {IncludeHiddenMarks}
      );
    end;
  end;
end;

end.
