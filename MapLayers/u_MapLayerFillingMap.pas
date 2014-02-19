unit u_MapLayerFillingMap;

interface

uses
  GR32_Image,
  i_NotifierTime,
  i_NotifierOperation,
  i_InternalPerformanceCounter,
  i_LocalCoordConverterChangeable,
  i_LocalCoordConverterFactorySimpe,
  i_Bitmap32StaticFactory,
  i_ImageResamplerConfig,
  i_FillingMapLayerConfig,
  u_TiledLayerWithThreadBase;

type
  TMapLayerFillingMap = class(TTiledLayerWithThreadBase)
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
      const ATimerNoifier: INotifierTime;
      const ABitmapFactory: IBitmap32StaticFactory;
      const AConfig: IFillingMapLayerConfig
    );
  end;

implementation

uses
  i_TileMatrix,
  i_BitmapLayerProviderChangeable,
  i_ObjectWithListener,
  u_TileMatrixFactory,
  u_SourceDataUpdateInRectByFillingMap,
  u_BitmapLayerProviderChangeableForFillingMap;

{ TMapLayerFillingMap }

constructor TMapLayerFillingMap.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const APosition: ILocalCoordConverterChangeable;
  const AView: ILocalCoordConverterChangeable;
  const ATileMatrixDraftResamplerConfig: IImageResamplerConfig;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  const ATimerNoifier: INotifierTime;
  const ABitmapFactory: IBitmap32StaticFactory;
  const AConfig: IFillingMapLayerConfig
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
    TBitmapLayerProviderChangeableForFillingMap.Create(ABitmapFactory, AConfig);
  VSourceChangeNotifier := TSourceDataUpdateInRectByFillingMap.Create(AConfig);
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
    AConfig.ThreadConfig,
    Self.ClassName
  );
end;

end.
