unit u_ProviderMapCombineECW;

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
  i_UseTilePrevZoomConfig,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_LocalCoordConverterFactorySimpe,
  i_BitmapPostProcessingConfig,
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
  TProviderMapCombineECW = class(TProviderMapCombineBase)
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
      const ABitmapPostProcessingConfig: IBitmapPostProcessingConfig;
      const AMapCalibrationList: IMapCalibrationList
    );
    procedure StartProcess(const APolygon: ILonLatPolygon); override;
  end;

implementation

uses
  gnugettext,
  i_RegionProcessParamsFrame,
  u_ThreadMapCombineECW,
  fr_MapCombine;

{ TProviderMapCombineECW }

constructor TProviderMapCombineECW.Create(
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
  const ABitmapPostProcessingConfig: IBitmapPostProcessingConfig;
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
      ABitmapPostProcessingConfig,
      AMapCalibrationList,
      True,
      False,
      'ecw',
      gettext_NoExtract('ECW (Enhanced Compression Wavelet)')
  );
end;

procedure TProviderMapCombineECW.StartProcess(const APolygon: ILonLatPolygon);
var
  VMapCalibrations: IMapCalibrationList;
  VFileName: string;
  VSplitCount: TPoint;
  VProjectedPolygon: IProjectedPolygon;
  VTargetConverter: ILocalCoordConverter;
  VImageProvider: IBitmapLayerProvider;
  VCancelNotifier: INotifierOperation;
  VOperationID: Integer;
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

  PrepareProcessInfo(VCancelNotifier, VOperationID, VProgressInfo);
  TThreadMapCombineECW.Create(
    VCancelNotifier,
    VOperationID,
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

