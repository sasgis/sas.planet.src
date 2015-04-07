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

unit u_ProviderMapCombineKMZ;

interface

uses
  i_LanguageManager,
  i_LocalCoordConverter,
  i_CoordConverterFactory,
  i_CoordConverterList,
  i_BitmapLayerProvider,
  i_GeometryProjected,
  i_GeometryLonLat,
  i_RegionProcessProgressInfo,
  i_ArchiveReadWriteFactory,
  i_UseTilePrevZoomConfig,
  i_BitmapTileSaveLoadFactory,
  i_LocalCoordConverterFactorySimpe,
  i_BitmapPostProcessing,
  i_Bitmap32BufferFactory,
  i_UsedMarksConfig,
  i_MarksDrawConfig,
  i_MarkSystem,
  i_MapLayerGridsConfig,
  i_ValueToStringConverter,
  i_MapCalibration,
  i_MapTypeListChangeable,
  i_GeometryProjectedFactory,
  i_GeometryProjectedProvider,
  i_GlobalViewMainConfig,
  i_RegionProcessProgressInfoInternalFactory,
  u_ExportProviderAbstract,
  fr_MapSelect,
  u_ProviderMapCombine;

type
  TProviderMapCombineKMZ = class(TProviderMapCombineBase)
  private
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
    FBitmapFactory: IBitmap32StaticFactory;
    FArchiveReadWriteFactory: IArchiveReadWriteFactory;
  public
    constructor Create(
      const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
      const ALanguageManager: ILanguageManager;
      const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
      const AActiveMapsSet: IMapTypeListChangeable;
      const AViewConfig: IGlobalViewMainConfig;
      const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
      const AProjectionFactory: IProjectionInfoFactory;
      const ACoordConverterList: ICoordConverterList;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AProjectedGeometryProvider: IGeometryProjectedProvider;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      const AMarksShowConfig: IUsedMarksConfig;
      const AMarksDrawConfig: IMarksDrawConfig;
      const AMarksDB: IMarkSystem;
      const ABitmapFactory: IBitmap32StaticFactory;
      const ABitmapPostProcessing: IBitmapPostProcessingChangeable;
      const AGridsConfig: IMapLayerGridsConfig;
      const AValueToStringConverter: IValueToStringConverterChangeable;
      const AMapCalibrationList: IMapCalibrationList
    );
    procedure StartProcess(const APolygon: IGeometryLonLatPolygon); override;
  end;

implementation

uses
  Classes,
  Dialogs,
  Types,
  gnugettext,
  i_RegionProcessParamsFrame,
  i_ProjectionInfo,
  u_ThreadMapCombineKMZ,
  u_GeoFunc,
  u_ResStrings,
  fr_MapCombine;

{ TProviderMapCombineKMZ }

constructor TProviderMapCombineKMZ.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const AActiveMapsSet: IMapTypeListChangeable;
  const AViewConfig: IGlobalViewMainConfig;
  const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
  const AProjectionFactory: IProjectionInfoFactory;
  const ACoordConverterList: ICoordConverterList;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AProjectedGeometryProvider: IGeometryProjectedProvider;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
  const AMarksShowConfig: IUsedMarksConfig;
  const AMarksDrawConfig: IMarksDrawConfig;
  const AMarksDB: IMarkSystem;
  const ABitmapFactory: IBitmap32StaticFactory;
  const ABitmapPostProcessing: IBitmapPostProcessingChangeable;
  const AGridsConfig: IMapLayerGridsConfig;
  const AValueToStringConverter: IValueToStringConverterChangeable;
  const AMapCalibrationList: IMapCalibrationList
);
begin
  inherited Create(
    AProgressFactory,
    ALanguageManager,
    AMapSelectFrameBuilder,
    AActiveMapsSet,
    AViewConfig,
    AUseTilePrevZoomConfig,
    AProjectionFactory,
    ACoordConverterList,
    AVectorGeometryProjectedFactory,
    AProjectedGeometryProvider,
    AMarksShowConfig,
    AMarksDrawConfig,
    AMarksDB,
    ABitmapFactory,
    ABitmapPostProcessing,
    AGridsConfig,
    AValueToStringConverter,
    AMapCalibrationList,
    True,
    False,
    False,
    'kmz',
    gettext_NoExtract('KMZ for Garmin (JPEG Overlays)')
  );
  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;
  FBitmapFactory := ABitmapFactory;
  FArchiveReadWriteFactory := AArchiveReadWriteFactory;
end;

procedure TProviderMapCombineKMZ.StartProcess(const APolygon: IGeometryLonLatPolygon);
var
  VMapCalibrations: IMapCalibrationList;
  VFileName: string;
  VSplitCount: TPoint;
  VProjection: IProjectionInfo;
  VProjectedPolygon: IGeometryProjectedPolygon;
  VImageProvider: IBitmapLayerProvider;
  VProgressInfo: IRegionProcessProgressInfoInternal;
  VMapRect: TRect;
  VMapSize: TPoint;
  VMapPieceSize: TPoint;
  VKmzImgesCount: TPoint;
  VThread: TThread;
begin
  VProjection := PrepareProjection;
  VProjectedPolygon := PreparePolygon(VProjection, APolygon);
  VImageProvider := PrepareImageProvider(APolygon, VProjection, VProjectedPolygon);
  VMapCalibrations := (ParamsFrame as IRegionProcessParamsFrameMapCalibrationList).MapCalibrationList;
  VFileName := PrepareTargetFileName;
  VSplitCount := (ParamsFrame as IRegionProcessParamsFrameMapCombine).SplitCount;

  VMapRect := PrepareTargetRect(VProjection, VProjectedPolygon);
  VMapSize := RectSize(VMapRect);
  VMapPieceSize.X := VMapSize.X div VSplitCount.X;
  VMapPieceSize.Y := VMapSize.Y div VSplitCount.Y;
  VKmzImgesCount.X := ((VMapPieceSize.X - 1) div 1024) + 1;
  VKmzImgesCount.Y := ((VMapPieceSize.Y - 1) div 1024) + 1;
  if ((VKmzImgesCount.X * VKmzImgesCount.Y) > 100) then begin
    ShowMessage(SAS_MSG_GarminMax1Mp);
  end;

  VProgressInfo := ProgressFactory.Build(APolygon);
  VThread :=
    TThreadMapCombineKMZ.Create(
      VProgressInfo,
      APolygon,
      VProjection,
      VMapRect,
      VImageProvider,
      FBitmapFactory,
      VMapCalibrations,
      VFileName,
      VSplitCount,
      FBitmapTileSaveLoadFactory,
      FArchiveReadWriteFactory,
      (ParamsFrame as IRegionProcessParamsFrameMapCombineJpg).Quality
    );
  VThread.Resume;
end;

end.
