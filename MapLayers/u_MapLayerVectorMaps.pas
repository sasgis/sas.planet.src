unit u_MapLayerVectorMaps;

interface

uses
  GR32_Image,
  i_NotifierTime,
  i_NotifierOperation,
  i_LocalCoordConverterChangeable,
  i_LocalCoordConverterFactorySimpe,
  i_InternalPerformanceCounter,
  i_ThreadConfig,
  i_VectorItemDrawConfig,
  i_Bitmap32StaticFactory,
  i_ImageResamplerConfig,
  i_MarkerDrawable,
  i_VectorItemSubsetChangeable,
  i_GeometryProjectedProvider,
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
      const AProjectedProvider: IGeometryProjectedProvider;
      const ATimerNoifier: INotifierTime;
      const AVectorItems: IVectorItemSubsetChangeable;
      const ABitmapFactory: IBitmap32StaticFactory;
      const APointMarker: IMarkerDrawableChangeable;
      const ADrawConfig: IVectorItemDrawConfig;
      const AThreadConfig: IThreadConfig
    );
  end;

implementation

uses
  i_TileMatrix,
  i_BitmapLayerProviderChangeable,
  u_TileMatrixFactory,
  u_BitmapLayerProviderChangeableForVectorMaps;

{ TMapLayerVectorMaps }

constructor TMapLayerVectorMaps.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier, AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const APosition: ILocalCoordConverterChangeable;
  const AView: ILocalCoordConverterChangeable;
  const ATileMatrixDraftResamplerConfig: IImageResamplerConfig;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  const AProjectedProvider: IGeometryProjectedProvider;
  const ATimerNoifier: INotifierTime;
  const AVectorItems: IVectorItemSubsetChangeable;
  const ABitmapFactory: IBitmap32StaticFactory;
  const APointMarker: IMarkerDrawableChangeable;
  const ADrawConfig: IVectorItemDrawConfig;
  const AThreadConfig: IThreadConfig
);
var
  VTileMatrixFactory: ITileMatrixFactory;
  VProvider: IBitmapLayerProviderChangeable;
begin
  VTileMatrixFactory :=
    TTileMatrixFactory.Create(
      ATileMatrixDraftResamplerConfig,
      ABitmapFactory,
      AConverterFactory
    );
  VProvider :=
    TBitmapLayerProviderChangeableForVectorMaps.Create(
      ADrawConfig,
      APointMarker,
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
    AThreadConfig,
    Self.ClassName
  );
end;

end.
