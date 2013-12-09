unit u_ProviderMapCombineKMZ;

interface

uses
  GR32,
  i_LanguageManager,
  i_LocalCoordConverter,
  i_CoordConverterFactory,
  i_CoordConverterList,
  i_BitmapLayerProvider,
  i_VectorItemProjected,
  i_VectorItemLonLat,
  i_RegionProcessProgressInfo,
  i_ArchiveReadWriteFactory,
  i_MapTypeSet,
  i_UseTilePrevZoomConfig,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_BitmapTileSaveLoadFactory,
  i_LocalCoordConverterFactorySimpe,
  i_BitmapPostProcessing,
  i_Bitmap32StaticFactory,
  i_UsedMarksConfig,
  i_MarksDrawConfig,
  i_MarkSystem,
  i_MapCalibration,
  i_VectorItemsFactory,
  i_ProjectedGeometryProvider,
  i_GlobalViewMainConfig,
  i_RegionProcessProgressInfoInternalFactory,
  u_ExportProviderAbstract,
  u_ProviderMapCombine;

type
  TProviderMapCombineKMZ = class(TProviderMapCombineBase)
  private
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
    FArchiveReadWriteFactory: IArchiveReadWriteFactory;
  public
    constructor Create(
      const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
      const ALanguageManager: ILanguageManager;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AViewConfig: IGlobalViewMainConfig;
      const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
      const AProjectionFactory: IProjectionInfoFactory;
      const ACoordConverterList: ICoordConverterList;
      const AVectorGeometryProjectedFactory: IVectorGeometryProjectedFactory;
      const AProjectedGeometryProvider: IProjectedGeometryProvider;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      const AMarksShowConfig: IUsedMarksConfig;
      const AMarksDrawConfig: IMarksDrawConfig;
      const AMarksDB: IMarkSystem;
      const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      const ABitmapFactory: IBitmap32StaticFactory;
      const ABitmapPostProcessing: IBitmapPostProcessingChangeable;
      const AMapCalibrationList: IMapCalibrationList
    );
    procedure StartProcess(const APolygon: IGeometryLonLatMultiPolygon); override;
  end;

implementation

uses
  Classes,
  Dialogs,
  gnugettext,
  i_RegionProcessParamsFrame,
  u_ThreadMapCombineKMZ,
  u_ResStrings,
  fr_MapCombine;

{ TProviderMapCombineKMZ }

constructor TProviderMapCombineKMZ.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMainMapsConfig: IMainMapsConfig; const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AViewConfig: IGlobalViewMainConfig;
  const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
  const AProjectionFactory: IProjectionInfoFactory;
  const ACoordConverterList: ICoordConverterList;
  const AVectorGeometryProjectedFactory: IVectorGeometryProjectedFactory;
  const AProjectedGeometryProvider: IProjectedGeometryProvider;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
  const AMarksShowConfig: IUsedMarksConfig;
  const AMarksDrawConfig: IMarksDrawConfig; const AMarksDB: IMarkSystem;
  const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  const ABitmapFactory: IBitmap32StaticFactory;
  const ABitmapPostProcessing: IBitmapPostProcessingChangeable;
  const AMapCalibrationList: IMapCalibrationList);
begin
  inherited Create(
      AProgressFactory,
      ALanguageManager,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AViewConfig,
      AUseTilePrevZoomConfig,
      AProjectionFactory,
      ACoordConverterList,
      AVectorGeometryProjectedFactory,
      AProjectedGeometryProvider,
      AMarksShowConfig,
      AMarksDrawConfig,
      AMarksDB,
      ALocalConverterFactory,
      ABitmapFactory,
      ABitmapPostProcessing,
      AMapCalibrationList,
      True,
      False,
      False,
      'kmz',
      gettext_NoExtract('KMZ for Garmin (JPEG Overlays)')
  );
  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;
  FArchiveReadWriteFactory := AArchiveReadWriteFactory;
end;

procedure TProviderMapCombineKMZ.StartProcess(const APolygon: IGeometryLonLatMultiPolygon);
var
  VMapCalibrations: IMapCalibrationList;
  VFileName: string;
  VSplitCount: TPoint;
  VProjectedPolygon: IProjectedPolygon;
  VTargetConverter: ILocalCoordConverter;
  VImageProvider: IBitmapLayerProvider;
  VProgressInfo: IRegionProcessProgressInfoInternal;
  VMapSize: TPoint;
  VMapPieceSize: TPoint;
  VKmzImgesCount: TPoint;
  VThread: TThread;
begin
  VProjectedPolygon := PreparePolygon(APolygon);
  VTargetConverter := PrepareTargetConverter(VProjectedPolygon);
  VImageProvider := PrepareImageProvider(APolygon, VProjectedPolygon);
  VMapCalibrations := (ParamsFrame as IRegionProcessParamsFrameMapCalibrationList).MapCalibrationList;
  VFileName := PrepareTargetFileName;
  VSplitCount := (ParamsFrame as IRegionProcessParamsFrameMapCombine).SplitCount;

  VMapSize := VTargetConverter.GetLocalRectSize;
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
      VTargetConverter,
      VImageProvider,
      LocalConverterFactory,
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

