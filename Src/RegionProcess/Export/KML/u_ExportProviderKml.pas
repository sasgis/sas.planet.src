{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_ExportProviderKml;

interface

uses
  Types,
  Forms,
  i_LanguageManager,
  i_BitmapTileProvider,
  i_BitmapTileProviderBuilder,
  i_Bitmap32BufferFactory,
  i_TileIteratorFactory,
  i_TileStorageTypeList,
  i_TileFileNameGeneratorsList,
  i_RegionProcessProgressInfoInternalFactory,
  i_GeometryLonLat,
  i_RegionProcessTask,
  i_RegionProcessProgressInfo,
  i_MapType,
  i_MapTypeListChangeable,
  i_GlobalViewMainConfig,
  i_GeometryProjectedFactory,
  u_ExportProviderAbstract,
  fr_MapSelect,
  fr_ExportKml;

type
  TExportProviderKml = class(TExportProviderBase)
  private
    FTileStorageTypeList: ITileStorageTypeListStatic;
    FTileNameGenerator: ITileFileNameGeneratorsList;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FActiveMapsList: IMapTypeListChangeable;
    FBitmapTileProviderBuilder: IBitmapTileProviderBuilder;
    FViewConfig: IGlobalViewMainConfig;
    FGeometryProjectedFactory: IGeometryProjectedFactory;
    function PrepareBitmapTileProviders(
      const AMapType: IMapType;
      const APolygon: IGeometryLonLatPolygon;
      const AZoomArr: TByteDynArray
    ): TBitmapTileProviderDynArray;
  protected
    function CreateFrame: TFrame; override;
  protected
    function GetCaption: string; override;
    function PrepareTask(
      const APolygon: IGeometryLonLatPolygon;
      const AProgressInfo: IRegionProcessProgressInfoInternal
    ): IRegionProcessTask; override;
  public
    constructor Create(
      const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
      const ALanguageManager: ILanguageManager;
      const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
      const ATileIteratorFactory: ITileIteratorFactory;
      const ATileStorageTypeList: ITileStorageTypeListStatic;
      const ATileNameGenerator: ITileFileNameGeneratorsList;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const AActiveMapsList: IMapTypeListChangeable;
      const AViewConfig: IGlobalViewMainConfig;
      const AGeometryProjectedFactory: IGeometryProjectedFactory;
      const ABitmapTileProviderBuilder: IBitmapTileProviderBuilder
    );
  end;

implementation

uses
  Classes,
  SysUtils,
  i_RegionProcessParamsFrame,
  i_BitmapTileSaveLoad,
  i_BitmapLayerProvider,
  u_BitmapLayerProviderMapWithLayer,
  u_ExportTaskToKML,
  u_ResStrings;

{ TExportProviderKml }

constructor TExportProviderKml.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const ATileIteratorFactory: ITileIteratorFactory;
  const ATileStorageTypeList: ITileStorageTypeListStatic;
  const ATileNameGenerator: ITileFileNameGeneratorsList;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const AActiveMapsList: IMapTypeListChangeable;
  const AViewConfig: IGlobalViewMainConfig;
  const AGeometryProjectedFactory: IGeometryProjectedFactory;
  const ABitmapTileProviderBuilder: IBitmapTileProviderBuilder
);
begin
  inherited Create(
    AProgressFactory,
    ALanguageManager,
    AMapSelectFrameBuilder,
    ATileIteratorFactory
  );
  FTileStorageTypeList := ATileStorageTypeList;
  FTileNameGenerator := ATileNameGenerator;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FActiveMapsList := AActiveMapsList;
  FViewConfig := AViewConfig;
  FGeometryProjectedFactory := AGeometryProjectedFactory;
  FBitmapTileProviderBuilder := ABitmapTileProviderBuilder;
end;

function TExportProviderKml.CreateFrame: TFrame;
begin
  Result :=
    TfrExportKml.Create(
      Self.LanguageManager,
      Self.MapSelectFrameBuilder,
      FTileStorageTypeList,
      FTileNameGenerator,
      FBitmap32StaticFactory,
      FActiveMapsList
    );
  Assert(Supports(Result, IRegionProcessParamsFrameZoomArray));
  Assert(Supports(Result, IRegionProcessParamsFrameOneMap));
  Assert(Supports(Result, IRegionProcessParamsFrameTargetPath));
  Assert(Supports(Result, IRegionProcessParamsFrameImageProvider));
  Assert(Supports(Result, IRegionProcessParamsFrameKmlExport));
end;

function TExportProviderKml.GetCaption: string;
begin
  Result := SAS_STR_ExportGEKmlExportCaption;
end;

function TExportProviderKml.PrepareBitmapTileProviders(
  const AMapType: IMapType;
  const APolygon: IGeometryLonLatPolygon;
  const AZoomArr: TByteDynArray
): TBitmapTileProviderDynArray;
var
  I: Integer;
  VUniProvider: IBitmapTileUniProvider;
begin
  VUniProvider := (ParamsFrame as IRegionProcessParamsFrameImageProvider).Provider;
  if VUniProvider = nil then begin
    Result := nil;
    Exit;
  end;
  SetLength(Result, Length(AZoomArr));
  for I := 0 to Length(AZoomArr) - 1 do begin
    Result[I] :=
      FBitmapTileProviderBuilder.Build(
        (ParamsFrame as IRegionProcessParamsFrameKmlExport).UseMarks,
        (ParamsFrame as IRegionProcessParamsFrameKmlExport).UseRecolor,
        (ParamsFrame as IRegionProcessParamsFrameKmlExport).UseFillingMap,
        (ParamsFrame as IRegionProcessParamsFrameKmlExport).UseGrids,
        (ParamsFrame as IRegionProcessParamsFrameKmlExport).UsePreciseCropping,
        FViewConfig.BackGroundColor,
        FViewConfig.BackGroundColor,
        VUniProvider,
        APolygon,
        AMapType.TileStorage.ProjectionSet.Zooms[AZoomArr[I]]
      );
  end;
end;

function TExportProviderKml.PrepareTask(
  const APolygon: IGeometryLonLatPolygon;
  const AProgressInfo: IRegionProcessProgressInfoInternal
): IRegionProcessTask;
var
  VZoomArr: TByteDynArray;
  VMapType: IMapType;
  VBitmapTileProviderArr: TBitmapTileProviderDynArray;
begin
  inherited;
  VZoomArr := (ParamsFrame as IRegionProcessParamsFrameZoomArray).ZoomArray;
  VMapType := (ParamsFrame as IRegionProcessParamsFrameOneMap).MapType;
  VBitmapTileProviderArr := PrepareBitmapTileProviders(VMapType, APolygon, VZoomArr);

  Result :=
    TExportTaskToKML.Create(
      AProgressInfo,
      (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path,
      FGeometryProjectedFactory,
      Self.TileIteratorFactory,
      APolygon,
      VZoomArr,
      VMapType.TileStorage,
      VMapType.VersionRequest.GetStatic.BaseVersion,
      (ParamsFrame as IRegionProcessParamsFrameKmlExport).NotSaveNotExists,
      (ParamsFrame as IRegionProcessParamsFrameKmlExport).RelativePath,
      (ParamsFrame as IRegionProcessParamsFrameKmlExport).ExtractTilesFromStorage,
      (ParamsFrame as IRegionProcessParamsFrameKmlExport).TileFileNameGenerator,
      (ParamsFrame as IRegionProcessParamsFrameKmlExport).BitmapTileSaver,
      (ParamsFrame as IRegionProcessParamsFrameKmlExport).ContentTypeInfo,
      VBitmapTileProviderArr
    );
end;

end.
