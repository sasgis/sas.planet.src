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

unit u_VectorItemTreeExporterSmlMarks;

interface

uses
  i_VectorItemTree,
  i_VectorItemTreeExporter,
  i_HashFunction,
  i_GeometryLonLatFactory,
  i_VectorItemSubsetBuilder,
  i_InternalPerformanceCounter,
  i_AppearanceOfMarkFactory,
  i_ReadWriteState,
  i_MarkPicture,
  i_HtmlToHintTextConverter,
  i_MarkFactory,
  i_MarkDbImpl,
  i_MarkSystemImpl,
  i_ImportConfig,
  i_MarkCategoryFactory,
  u_BaseInterfacedObject;

type
  TVectorItemTreeExporterSmlMarks = class(TBaseInterfacedObject, IVectorItemTreeExporter)
  private
    FMarkPictureList: IMarkPictureList;
    FHashFunction: IHashFunction;
    FAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FMarkFactory: IMarkFactory;
    FCategoryFactory: IMarkCategoryFactory;
    FLoadDbCounter: IInternalPerformanceCounter;
    FSaveDbCounter: IInternalPerformanceCounter;
    FHintConverter: IHtmlToHintTextConverter;
  private
    function GetImportConfig(): IImportConfig;
  private
    { IVectorItemTreeExporter }
    procedure ProcessExport(
      const AFileName: string;
      const ATree: IVectorItemTree
    );
  public
    constructor Create(
      const AMarkPictureList: IMarkPictureList;
      const AHashFunction: IHashFunction;
      const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AMarkFactory: IMarkFactory;
      const ACategoryFactory: IMarkCategoryFactory;
      const ALoadDbCounter: IInternalPerformanceCounter;
      const ASaveDbCounter: IInternalPerformanceCounter;
      const AHintConverter: IHtmlToHintTextConverter
    );
  end;

implementation

uses
  SysUtils,
  t_CommonTypes,
  i_InterfaceListSimple,
  i_InterfaceListStatic,
  u_InterfaceListSimple,
  u_Category,
  u_ImportConfig,
  u_MarkSystemSml,
  u_MarkSystemHelpers;

{ TVectorItemTreeExporterSmlMarks }

constructor TVectorItemTreeExporterSmlMarks.Create(
  const AMarkPictureList: IMarkPictureList;
  const AHashFunction: IHashFunction;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AMarkFactory: IMarkFactory;
  const ACategoryFactory: IMarkCategoryFactory;
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
  FCategoryFactory := ACategoryFactory;
  FLoadDbCounter := ALoadDbCounter;
  FSaveDbCounter := ASaveDbCounter;
  FHintConverter := AHintConverter;
end;

function TVectorItemTreeExporterSmlMarks.GetImportConfig(): IImportConfig;
begin
  Result := TImportConfig.Create(

    TCategory.Create(''),

    TImportCategoryParams.Create(True, False, False, False), // ToDo

    TImportPointParams.Create(
      FAppearanceOfMarkFactory.CreatePointAppearance(0, 0, 0, '', nil, 0),
      False, False, False, False, False
    ),

    TImportLineParams.Create(
      FAppearanceOfMarkFactory.CreateLineAppearance(0, 0),
      False, False
    ),

    TImportPolyParams.Create(
      FAppearanceOfMarkFactory.CreatePolygonAppearance(0, 0, 0),
      False, False, False
    )
  );
end;

procedure TVectorItemTreeExporterSmlMarks.ProcessExport(
  const AFileName: string;
  const ATree: IVectorItemTree
);
var
  VSml: IMarkSystemImpl;
  VMarkList: IInterfaceListSimple;
begin
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
    FHintConverter
  );

  if VSml.State.GetStatic.WriteAccess = asEnabled then begin

    VMarkList := TInterfaceListSimple.Create;

    PrepareFromTreeForImport(
      VMarkList,
      ATree,
      GetImportConfig,
      VSml.MarkDb,
      FMarkFactory,
      VSml.CategoryDB,
      FCategoryFactory
    );

    if VMarkList.Count > 0 then begin
      VSml.MarkDb.UpdateMarkList(nil, VMarkList.MakeStaticAndClear);
    end;
  end;
end;

end.
