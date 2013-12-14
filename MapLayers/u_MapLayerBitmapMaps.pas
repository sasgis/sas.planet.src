unit u_MapLayerBitmapMaps;

interface

uses
  GR32_Image,
  i_NotifierTime,
  i_NotifierOperation,
  i_TileError,
  i_BitmapPostProcessing,
  i_LocalCoordConverterChangeable,
  i_LocalCoordConverterFactorySimpe,
  i_UseTilePrevZoomConfig,
  i_Bitmap32StaticFactory,
  i_ThreadConfig,
  i_MapTypes,
  i_MapTypeSetChangeable,
  i_MapTypeListChangeable,
  i_InternalPerformanceCounter,
  i_ImageResamplerConfig,
  u_TiledLayerWithThreadBase;

type
  TMapLayerBitmapMaps = class(TTiledLayerWithThreadBase)
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const APosition: ILocalCoordConverterChangeable;
      const AView: ILocalCoordConverterChangeable;
      const ATileMatrixDraftResamplerConfig: IImageResamplerConfig;
      const AConverterFactory: ILocalCoordConverterFactorySimpe;
      const AMainMap: IMapTypeChangeable;
      const ALayesList: IMapTypeListChangeable;
      const AAllActiveMapsSet: IMapTypeSetChangeable;
      const APostProcessing: IBitmapPostProcessingChangeable;
      const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
      const AThreadConfig: IThreadConfig;
      const ABitmapFactory: IBitmap32StaticFactory;
      const AErrorLogger: ITileErrorLogger;
      const ATimerNoifier: INotifierTime
    );
  end;

implementation

uses
  i_TileMatrix,
  i_BitmapLayerProviderChangeable,
  i_ObjectWithListener,
  u_TileMatrixFactory,
  u_SourceDataUpdateInRectByMapsSet,
  u_BitmapLayerProviderChangeableForMainLayer;

{ TMapMainLayerNew }

constructor TMapLayerBitmapMaps.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const APosition: ILocalCoordConverterChangeable;
  const AView: ILocalCoordConverterChangeable;
  const ATileMatrixDraftResamplerConfig: IImageResamplerConfig;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  const AMainMap: IMapTypeChangeable;
  const ALayesList: IMapTypeListChangeable;
  const AAllActiveMapsSet: IMapTypeSetChangeable;
  const APostProcessing: IBitmapPostProcessingChangeable;
  const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
  const AThreadConfig: IThreadConfig;
  const ABitmapFactory: IBitmap32StaticFactory;
  const AErrorLogger: ITileErrorLogger;
  const ATimerNoifier: INotifierTime
);
var
  VTileMatrixFactory: ITileMatrixFactory;
  VProvider: IBitmapLayerProviderChangeable;
  VSourceChangeNotifier: IObjectWithListener;
begin
  VTileMatrixFactory :=
    TTileMatrixFactory.Create(
      ATileMatrixDraftResamplerConfig,
      ABitmapFactory,
      AConverterFactory
    );
  VProvider :=
    TBitmapLayerProviderChangeableForMainLayer.Create(
      AMainMap,
      ALayesList,
      APostProcessing,
      AUseTilePrevZoomConfig,
      ABitmapFactory,
      AErrorLogger
    );

  VSourceChangeNotifier :=
    TSourceDataUpdateInRectByMapsSet.Create(AAllActiveMapsSet);
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    APosition,
    AView,
    VTileMatrixFactory,
    VProvider,
    VSourceChangeNotifier,
    ATimerNoifier,
    AThreadConfig
  );
end;

end.
