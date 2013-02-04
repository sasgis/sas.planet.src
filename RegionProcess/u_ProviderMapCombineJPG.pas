unit u_ProviderMapCombineJPG;

interface

uses
  GR32,
  i_NotifierTime,
  i_NotifierOperation,
  i_LanguageManager,
  i_LocalCoordConverter,
  i_CoordConverterFactory,
  i_CoordConverterList,
  i_BitmapLayerProvider,
  i_VectorItemProjected,
  i_VectorItemLonLat,
  i_RegionProcessProgressInfo,
  i_MapTypes,
  i_MapViewGoto,
  i_UseTilePrevZoomConfig,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_LocalCoordConverterFactorySimpe,
  i_BitmapPostProcessing,
  i_Bitmap32StaticFactory,
  i_UsedMarksConfig,
  i_MarksDrawConfig,
  i_MarksSystem,
  i_MapCalibration,
  i_VectorItemsFactory,
  i_GlobalViewMainConfig,
  u_ExportProviderAbstract,
  u_ProviderMapCombine;

type
  TProviderMapCombineJPG = class(TProviderMapCombineBase)
  private
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AViewConfig: IGlobalViewMainConfig;
      const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
      const AAppClosingNotifier: INotifierOneOperation;
      const ATimerNoifier: INotifierTime;
      const AProjectionFactory: IProjectionInfoFactory;
      const ACoordConverterList: ICoordConverterList;
      const AVectorItemsFactory: IVectorItemsFactory;
      const AMarksShowConfig: IUsedMarksConfig;
      const AMarksDrawConfig: IMarksDrawConfig;
      const AMarksDB: IMarksSystem;
      const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      const ABitmapFactory: IBitmap32StaticFactory;
      const ABitmapPostProcessing: IBitmapPostProcessingChangeable;
      const AMapCalibrationList: IMapCalibrationList
    );
    procedure StartProcess(const APolygon: ILonLatPolygon; const AMapGoto: IMapViewGoto ); override;
  end;

implementation

uses
  gnugettext,
  i_RegionProcessParamsFrame,
  u_ThreadMapCombineJPG,
  fr_MapCombine;

{ TProviderMapCombineJPG }

constructor TProviderMapCombineJPG.Create(
  const ALanguageManager: ILanguageManager;
  const AMainMapsConfig: IMainMapsConfig; const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AViewConfig: IGlobalViewMainConfig;
  const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
  const AAppClosingNotifier: INotifierOneOperation;
  const ATimerNoifier: INotifierTime;
  const AProjectionFactory: IProjectionInfoFactory;
  const ACoordConverterList: ICoordConverterList;
  const AVectorItemsFactory: IVectorItemsFactory;
  const AMarksShowConfig: IUsedMarksConfig;
  const AMarksDrawConfig: IMarksDrawConfig; const AMarksDB: IMarksSystem;
  const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  const ABitmapFactory: IBitmap32StaticFactory;
  const ABitmapPostProcessing: IBitmapPostProcessingChangeable;
  const AMapCalibrationList: IMapCalibrationList);
begin
  inherited Create(
      ALanguageManager,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AViewConfig,
      AUseTilePrevZoomConfig,
      AAppClosingNotifier,
      ATimerNoifier,
      AProjectionFactory,
      ACoordConverterList,
      AVectorItemsFactory,
      AMarksShowConfig,
      AMarksDrawConfig,
      AMarksDB,
      ALocalConverterFactory,
      ABitmapFactory,
      ABitmapPostProcessing,
      AMapCalibrationList,
      True,
      False,
      'jpg',
      gettext_NoExtract('JPEG (Joint Photographic Experts Group)')
  );
end;

procedure TProviderMapCombineJPG.StartProcess(const APolygon: ILonLatPolygon; const AMapGoto: IMapViewGoto );
var
  VMapCalibrations: IMapCalibrationList;
  VFileName: string;
  VSplitCount: TPoint;
  VProjectedPolygon: IProjectedPolygon;
  VTargetConverter: ILocalCoordConverter;
  VImageProvider: IBitmapLayerProvider;
  VProgressInfo: IRegionProcessProgressInfoInternal;
  VBGColor: TColor32;
begin
  VProjectedPolygon := PreparePolygon(APolygon);
  VTargetConverter := PrepareTargetConverter(VProjectedPolygon);
  VImageProvider := PrepareImageProvider(APolygon, VProjectedPolygon);
  VMapCalibrations := (ParamsFrame as IRegionProcessParamsFrameMapCalibrationList).MapCalibrationList;
  VFileName := PrepareTargetFileName;
  VSplitCount := (ParamsFrame as IRegionProcessParamsFrameMapCombine).SplitCount;
  VBGColor := (ParamsFrame as IRegionProcessParamsFrameMapCombine).BGColor;

  VProgressInfo := PrepareProcessInfo(APolygon, AMapGoto);
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
    (ParamsFrame as IRegionProcessParamsFrameMapCombineJpg).Quality
  );
end;

end.

