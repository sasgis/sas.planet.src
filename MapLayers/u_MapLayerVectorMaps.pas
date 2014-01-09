unit u_MapLayerVectorMaps;

interface

uses
  GR32_Image,
  i_NotifierTime,
  i_NotifierOperation,
  i_LocalCoordConverterChangeable,
  i_LocalCoordConverterFactorySimpe,
  i_InternalPerformanceCounter,
  i_KmlLayerConfig,
  i_Bitmap32StaticFactory,
  i_ImageResamplerConfig,
  i_MarkerDrawable,
  i_VectorItemSubsetChangeable,
  i_ProjectedGeometryProvider,
  u_TiledLayerWithThreadBase;

type
  TMapLayerVectorMaps = class(TTiledLayerWithThreadBase)
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
      const AProjectedProvider: IProjectedGeometryProvider;
      const ATimerNoifier: INotifierTime;
      const AVectorItems: IVectorItemSubsetChangeable;
      const ABitmapFactory: IBitmap32StaticFactory;
      const AConfig: IKmlLayerConfig
    );
  end;

implementation

uses
  i_TileMatrix,
  i_BitmapLayerProviderChangeable,
  u_TileMatrixFactory,
  u_MarkerDrawableSimpleSquare,
  u_MarkerDrawableChangeableSimple,
  u_BitmapLayerProviderChangeableForVectorMaps;

{ TWikiLayerNew }

constructor TMapLayerVectorMaps.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier, AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const APosition: ILocalCoordConverterChangeable;
  const AView: ILocalCoordConverterChangeable;
  const ATileMatrixDraftResamplerConfig: IImageResamplerConfig;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  const AProjectedProvider: IProjectedGeometryProvider;
  const ATimerNoifier: INotifierTime;
  const AVectorItems: IVectorItemSubsetChangeable;
  const ABitmapFactory: IBitmap32StaticFactory;
  const AConfig: IKmlLayerConfig
);
var
  VTileMatrixFactory: ITileMatrixFactory;
  VProvider: IBitmapLayerProviderChangeable;
  VPointMarker: IMarkerDrawableChangeable;
begin
  VTileMatrixFactory :=
    TTileMatrixFactory.Create(
      ATileMatrixDraftResamplerConfig,
      ABitmapFactory,
      AConverterFactory
    );
  VPointMarker :=
    TMarkerDrawableChangeableSimple.Create(
      TMarkerDrawableSimpleSquare,
      AConfig.PointMarkerConfig
    );
  VProvider :=
    TBitmapLayerProviderChangeableForVectorMaps.Create(
      AConfig.DrawConfig,
      VPointMarker,
      ABitmapFactory,
      AProjectedProvider,
      AVectorItems
    );
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    APosition,
    AView,
    VTileMatrixFactory,
    VProvider,
    nil,
    ATimerNoifier,
    AConfig.ThreadConfig
  );
end;

end.
