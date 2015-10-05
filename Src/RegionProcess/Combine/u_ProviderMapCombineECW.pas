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

unit u_ProviderMapCombineECW;

interface

uses
  i_LanguageManager,
  i_ProjectionSetList,
  i_ProjectionSetChangeable,
  i_BitmapTileProvider,
  i_GeometryProjected,
  i_GeometryLonLat,
  i_RegionProcessProgressInfo,
  i_UseTilePrevZoomConfig,
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
  i_VectorItemSubsetBuilder,
  i_GlobalViewMainConfig,
  i_RegionProcessProgressInfoInternalFactory,
  u_ExportProviderAbstract,
  fr_MapSelect,
  u_ProviderMapCombine;

type
  TProviderMapCombineECW = class(TProviderMapCombineBase)
  private
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
  Types,
  gnugettext,
  i_RegionProcessParamsFrame,
  i_Projection,
  u_ThreadMapCombineBase,
  u_ThreadMapCombineECW,
  fr_MapCombine;

{ TProviderMapCombineECW }

constructor TProviderMapCombineECW.Create(
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
    AGridsConfig,
    AValueToStringConverter,
    AMapCalibrationList,
    Point(100, 100),
    Point(MaxInt, MaxInt),
    True,
    False,
    False,
    'ecw',
    gettext_NoExtract('ECW (Enhanced Compression Wavelet)')
  );
end;

procedure TProviderMapCombineECW.StartProcess(const APolygon: IGeometryLonLatPolygon);
var
  VMapCalibrations: IMapCalibrationList;
  VFileName: string;
  VSplitCount: TPoint;
  VProjection: IProjection;
  VProjectedPolygon: IGeometryProjectedPolygon;
  VImageProvider: IBitmapTileProvider;
  VProgressInfo: IRegionProcessProgressInfoInternal;
  VThread: TThread;
  VProgressUpdate: IBitmapCombineProgressUpdate;
  VCombiner: IBitmapMapCombiner;
begin
  VProjection := PrepareProjection;
  VProjectedPolygon := PreparePolygon(VProjection, APolygon);
  VImageProvider := PrepareImageProvider(APolygon, VProjection, VProjectedPolygon);
  VMapCalibrations := (ParamsFrame as IRegionProcessParamsFrameMapCalibrationList).MapCalibrationList;
  VFileName := PrepareTargetFileName;
  VSplitCount := (ParamsFrame as IRegionProcessParamsFrameMapCombine).SplitCount;

  VProgressInfo := ProgressFactory.Build(APolygon);
  VProgressUpdate := TBitmapCombineProgressUpdate.Create(VProgressInfo);
  VCombiner :=
    TBitmapMapCombinerECW.Create(
      VProgressUpdate,
      (ParamsFrame as IRegionProcessParamsFrameMapCombineJpg).Quality
    );
  VThread :=
    TThreadMapCombineBase.Create(
      VProgressInfo,
      APolygon,
      PrepareTargetRect(VProjection, VProjectedPolygon),
      VCombiner,
      VImageProvider,
      VMapCalibrations,
      VFileName,
      VSplitCount,
      Self.ClassName + 'Thread'
    );
  VThread.Resume;
end;

end.
