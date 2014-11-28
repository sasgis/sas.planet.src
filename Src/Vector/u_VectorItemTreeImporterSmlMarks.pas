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
  i_NotifierOperation,
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
  i_MarkFactory,
  i_MarkCategoryDBImpl,
  i_MarkSystemImpl,
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
    { IVectorItemTreeImporter }
    function ProcessImport(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AFileName: string;
      var AConfig: IInterface
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
  i_StaticTreeBuilder,
  i_InterfaceListStatic,
  u_StaticTreeBuilderBase,
  u_MarkSystemSml,
  u_MarkSystemHelpers;

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

function TVectorItemTreeImporterSmlMarks.ProcessImport(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AFileName: string;
  var AConfig: IInterface
): IVectorItemTree;
var
  VSml: IMarkSystemImpl;
  VCategoiesList: IInterfaceListStatic;
  VCategoryTreeBuilder: IStaticTreeBuilder;
begin
  Result := nil;

  VSml := TMarkSystemSml.Create(
    AOperationID,
    ACancelNotifier,
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
      Result := CategoryTreeToMarkTreeHelper(
        VSml.MarkDb,
        VCategoryTreeBuilder.BuildStatic(VCategoiesList),
        True {IncludeHiddenMarks}
      );
    end;
  end;
end;

end.
