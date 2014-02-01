unit u_ProviderMapCombineJPG;

interface

uses
  GR32,
  i_LanguageManager,
  i_LocalCoordConverter,
  i_CoordConverterFactory,
  i_CoordConverterList,
  i_BitmapLayerProvider,
  i_GeometryProjected,
  i_GeometryLonLat,
  i_RegionProcessProgressInfo,
  i_MapTypeSet,
  i_UseTilePrevZoomConfig,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_LocalCoordConverterFactorySimpe,
  i_BitmapPostProcessing,
  i_Bitmap32StaticFactory,
  i_UsedMarksConfig,
  i_MarksDrawConfig,
  i_MarkSystem,
  i_MapCalibration,
  i_VectorGeometryProjectedFactory,
  i_ProjectedGeometryProvider,
  i_GlobalViewMainConfig,
  i_RegionProcessProgressInfoInternalFactory,
  u_ExportProviderAbstract,
  u_ProviderMapCombine;

type
  TProviderMapCombineJPG = class(TProviderMapCombineBase)
  private
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
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AProjectedGeometryProvider: IProjectedGeometryProvider;
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
  gnugettext,
  i_RegionProcessParamsFrame,
  u_ThreadMapCombineJPG,
  fr_MapCombine;

{ TProviderMapCombineJPG }

constructor TProviderMapCombineJPG.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMainMapsConfig: IMainMapsConfig; const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AViewConfig: IGlobalViewMainConfig;
  const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
  const AProjectionFactory: IProjectionInfoFactory;
  const ACoordConverterList: ICoordConverterList;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AProjectedGeometryProvider: IProjectedGeometryProvider;
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
      True,
      False,
      'jpg',
      gettext_NoExtract('JPEG (Joint Photographic Experts Group)')
  );
end;

procedure TProviderMapCombineJPG.StartProcess(const APolygon: IGeometryLonLatMultiPolygon);
var
  VMapCalibrations: IMapCalibrationList;
  VFileName: string;
  VSplitCount: TPoint;
  VProjectedPolygon: IGeometryProjectedMultiPolygon;
  VTargetConverter: ILocalCoordConverter;
  VImageProvider: IBitmapLayerProvider;
  VProgressInfo: IRegionProcessProgressInfoInternal;
  VBGColor: TColor32;
  VThread: TThread;
begin
  VProjectedPolygon := PreparePolygon(APolygon);
  VTargetConverter := PrepareTargetConverter(VProjectedPolygon);
  VImageProvider := PrepareImageProvider(APolygon, VProjectedPolygon);
  VMapCalibrations := (ParamsFrame as IRegionProcessParamsFrameMapCalibrationList).MapCalibrationList;
  VFileName := PrepareTargetFileName;
  VSplitCount := (ParamsFrame as IRegionProcessParamsFrameMapCombine).SplitCount;
  VBGColor := (ParamsFrame as IRegionProcessParamsFrameMapCombine).BGColor;

  VProgressInfo := ProgressFactory.Build(APolygon);
  VThread :=
    TThreadMapCombineJPG.Create(
      VProgressInfo,
      APolygon,
      VTargetConverter,
      VImageProvider,
      LocalConverterFactory,
      VMapCalibrations,
      VFileName,
      VSplitCount,
      VBGColor,
      (ParamsFrame as IRegionProcessParamsFrameMapCombineJpg).Quality,
      (ParamsFrame as IRegionProcessParamsFrameMapCombineJpg).IsSaveGeoRefInfoToExif
    );
  VThread.Resume;
end;

end.

