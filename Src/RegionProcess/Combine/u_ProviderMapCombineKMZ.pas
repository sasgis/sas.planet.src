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
  i_ProjectionSetList,
  i_ProjectionSetChangeable,
  i_GeometryLonLat,
  i_RegionProcessProgressInfo,
  i_ArchiveReadWriteFactory,
  i_UseTilePrevZoomConfig,
  i_BitmapTileSaveLoadFactory,
  i_BitmapPostProcessing,
  i_Bitmap32BufferFactory,
  i_UsedMarksConfig,
  i_MarksDrawConfig,
  i_MarkSystem,
  i_MapType,
  i_FillingMapLayerConfig,
  i_FillingMapPolygon,
  i_MapLayerGridsConfig,
  i_ValueToStringConverter,
  i_MapCalibration,
  i_MapTypeListChangeable,
  i_GeometryProjectedFactory,
  i_GeometryProjectedProvider,
  i_VectorItemSubsetBuilder,
  i_GlobalViewMainConfig,
  i_RegionProcessProgressInfoInternalFactory,
  i_BitmapMapCombiner,
  u_ExportProviderAbstract,
  fr_MapSelect,
  u_ProviderMapCombine;

type
  TProviderMapCombineKMZ = class(TProviderMapCombineBase)
  private
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
    FBitmapFactory: IBitmap32StaticFactory;
    FArchiveReadWriteFactory: IArchiveReadWriteFactory;
  protected
    function PrepareMapCombiner(
      const AProgressInfo: IRegionProcessProgressInfoInternal
    ): IBitmapMapCombiner; override;
    function Validate(const APolygon: IGeometryLonLatPolygon): Boolean; override;
  public
    constructor Create(
      const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
      const ALanguageManager: ILanguageManager;
      const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
      const AActiveMapsSet: IMapTypeListChangeable;
      const AViewConfig: IGlobalViewMainConfig;
      const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
      const AProjectionSet: IProjectionSetChangeable;
      const AProjectionSetList: IProjectionSetList;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AProjectedGeometryProvider: IGeometryProjectedProvider;
      const AVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      const AMarksShowConfig: IUsedMarksConfig;
      const AMarksDrawConfig: IMarksDrawConfig;
      const AMarksDB: IMarkSystem;
      const ABitmapFactory: IBitmap32StaticFactory;
      const ABitmapPostProcessing: IBitmapPostProcessingChangeable;
      const AFillingMapConfig: IFillingMapLayerConfig;
      const AFillingMapType: IMapTypeChangeable;
      const AFillingMapPolygon: IFillingMapPolygon;
      const AGridsConfig: IMapLayerGridsConfig;
      const AValueToStringConverter: IValueToStringConverterChangeable;
      const AMapCalibrationList: IMapCalibrationList
    );
  end;

implementation

uses
  Dialogs,
  Types,
  Math,
  gnugettext,
  t_CommonTypes,
  t_GeoTypes,
  i_Projection,
  u_ThreadMapCombineBase,
  u_BitmapMapCombinerKMZ,
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
  const AProjectionSet: IProjectionSetChangeable;
  const AProjectionSetList: IProjectionSetList;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AProjectedGeometryProvider: IGeometryProjectedProvider;
  const AVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
  const AMarksShowConfig: IUsedMarksConfig;
  const AMarksDrawConfig: IMarksDrawConfig;
  const AMarksDB: IMarkSystem;
  const ABitmapFactory: IBitmap32StaticFactory;
  const ABitmapPostProcessing: IBitmapPostProcessingChangeable;
  const AFillingMapConfig: IFillingMapLayerConfig;
  const AFillingMapType: IMapTypeChangeable;
  const AFillingMapPolygon: IFillingMapPolygon;
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
    AProjectionSet,
    AProjectionSetList,
    AVectorGeometryProjectedFactory,
    AProjectedGeometryProvider,
    AVectorSubsetBuilderFactory,
    AMarksShowConfig,
    AMarksDrawConfig,
    AMarksDB,
    ABitmapFactory,
    ABitmapPostProcessing,
    AFillingMapConfig,
    AFillingMapType,
    AFillingMapPolygon,
    AGridsConfig,
    AValueToStringConverter,
    AMapCalibrationList,
    Point(0, 0),
    Point(10240, 10240),
    True,
    False,
    False,
    stsUnicode,
    'kmz',
    gettext_NoExtract('KMZ for Garmin (JPEG Overlays)')
  );
  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;
  FBitmapFactory := ABitmapFactory;
  FArchiveReadWriteFactory := AArchiveReadWriteFactory;
end;

function TProviderMapCombineKMZ.PrepareMapCombiner(
  const AProgressInfo: IRegionProcessProgressInfoInternal
): IBitmapMapCombiner;
var
  VProgressUpdate: IBitmapCombineProgressUpdate;
begin
  VProgressUpdate := TBitmapCombineProgressUpdate.Create(AProgressInfo);
  Result :=
    TBitmapMapCombinerKMZ.Create(
      VProgressUpdate,
      FBitmapFactory,
      FBitmapTileSaveLoadFactory,
      FArchiveReadWriteFactory,
      (ParamsFrame as IRegionProcessParamsFrameMapCombineJpg).Quality
    );
end;

function TProviderMapCombineKMZ.Validate(
  const APolygon: IGeometryLonLatPolygon
): Boolean;
var
  VSplitCount: TPoint;
  VProjection: IProjection;
  VLonLatRect: TDoubleRect;
  VPixelRect: TRect;
  VPixelSize: TPoint;
  VKmzImgesCount: TPoint;
begin
  Result := inherited Validate(APolygon);
  if not Result then begin
    Exit;
  end;

  if not Assigned(APolygon) then begin
    Assert(False, _('Polygon isn''t selected'));
    Result := False;
    Exit;
  end;
  VSplitCount := (ParamsFrame as IRegionProcessParamsFrameMapCombine).SplitCount;
  VProjection := PrepareProjection;
  if not Assigned(VProjection) then begin
    Assert(False, _('Projection isn''t selected'));
    Result := False;
    Exit;
  end;
  VLonLatRect := APolygon.Bounds.Rect;
  VProjection.ProjectionType.ValidateLonLatRect(VLonLatRect);
  VPixelRect :=
    RectFromDoubleRect(
      VProjection.LonLatRect2PixelRectFloat(VLonLatRect),
      rrOutside
    );
  VPixelSize := RectSize(VPixelRect);
  VPixelSize.X := Trunc(VPixelSize.X / VSplitCount.X);
  VPixelSize.Y := Trunc(VPixelSize.Y / VSplitCount.Y);

  VKmzImgesCount.X := ((VPixelSize.X - 1) div 1024) + 1;
  VKmzImgesCount.Y := ((VPixelSize.Y - 1) div 1024) + 1;
  if ((VKmzImgesCount.X * VKmzImgesCount.Y) > 100) then begin
    ShowMessage(SAS_MSG_GarminMax1Mp);
  end;

  Result := True;
end;

end.
