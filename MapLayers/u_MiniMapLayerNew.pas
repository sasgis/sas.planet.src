unit u_MiniMapLayerNew;

interface

uses
  GR32_Image,
  i_Notifier,
  i_NotifierOperation,
  i_TileError,
  i_BitmapPostProcessingConfig,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_LocalCoordConverterFactorySimpe,
  i_BitmapLayerProvider,
  i_UseTilePrevZoomConfig,
  i_ThreadConfig,
  i_MapTypes,
  i_MapTypeListChangeable,
  i_InternalPerformanceCounter,
  i_MiniMapLayerConfig,
  i_ImageResamplerConfig,
  u_TiledLayerWithThreadBase;

type
  TMiniMapLayerNew = class(TTiledLayerWithThreadBase)
  private
    FConfig: IMiniMapLayerConfig;
    FErrorLogger: ITileErrorLogger;
    FPostProcessingConfig: IBitmapPostProcessingConfig;
    FUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
    FMainMap: IMapTypeChangeable;
    FLayesList: IMapTypeListChangeable;

    procedure OnMainMapChange;
    procedure OnLayerListChange;
    procedure OnConfigChange;
  protected
    function CreateLayerProvider(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALayerConverter: ILocalCoordConverter
    ): IBitmapLayerProvider; override;
    procedure StartThreads; override;
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
      const AConfig: IMiniMapLayerConfig;
      const AMainMap: IMapTypeChangeable;
      const ALayesList: IMapTypeListChangeable;
      const APostProcessingConfig: IBitmapPostProcessingConfig;
      const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
      const AThreadConfig: IThreadConfig;
      const AErrorLogger: ITileErrorLogger;
      const ATimerNoifier: INotifier
    );
  end;

implementation

uses
  i_TileMatrix,
  u_TileMatrixFactory,
  u_ListenerByEvent,
  u_BitmapLayerProviderForViewMaps;

{ TMapMainLayerNew }

constructor TMiniMapLayerNew.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const APosition: ILocalCoordConverterChangeable;
  const AView: ILocalCoordConverterChangeable;
  const ATileMatrixDraftResamplerConfig: IImageResamplerConfig;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  const AConfig: IMiniMapLayerConfig;
  const AMainMap: IMapTypeChangeable;
  const ALayesList: IMapTypeListChangeable;
  const APostProcessingConfig: IBitmapPostProcessingConfig;
  const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
  const AThreadConfig: IThreadConfig;
  const AErrorLogger: ITileErrorLogger;
  const ATimerNoifier: INotifier
);
var
  VTileMatrixFactory: ITileMatrixFactory;
begin
  VTileMatrixFactory :=
    TTileMatrixFactory.Create(
      ATileMatrixDraftResamplerConfig,
      AConverterFactory
    );
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    APosition,
    AView,
    VTileMatrixFactory,
    ATimerNoifier,
    False,
    AThreadConfig
  );
  FMainMap := AMainMap;
  FLayesList := ALayesList;
  FErrorLogger := AErrorLogger;
  FPostProcessingConfig := APostProcessingConfig;
  FUseTilePrevZoomConfig := AUseTilePrevZoomConfig;
  FConfig := AConfig;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnMainMapChange),
    FMainMap.ChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnLayerListChange),
    FLayesList.ChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FUseTilePrevZoomConfig.GetChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FPostProcessingConfig.GetChangeNotifier
  );
end;

function TMiniMapLayerNew.CreateLayerProvider(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALayerConverter: ILocalCoordConverter
): IBitmapLayerProvider;
var
  VMainMap: IMapType;
  VPostProcessingConfig: IBitmapPostProcessing;
  VLayersList: IMapTypeListStatic;
  VUsePrevConfig: IUseTilePrevZoomTileConfigStatic;
begin
  VMainMap := FMainMap.GetStatic;
  VLayersList := FLayesList.List;
  VUsePrevConfig := FUseTilePrevZoomConfig.GetStatic;
  VPostProcessingConfig := FPostProcessingConfig.GetStatic;

  Result :=
    TBitmapLayerProviderForViewMaps.Create(
      VMainMap,
      VLayersList,
      VUsePrevConfig.UsePrevZoomAtMap,
      VUsePrevConfig.UsePrevZoomAtLayer,
      True,
      VPostProcessingConfig,
      FErrorLogger
    );
end;

procedure TMiniMapLayerNew.OnConfigChange;
begin
  ViewUpdateLock;
  try
    Visible := FConfig.Visible;
    SetNeedUpdateLayerProvider;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMiniMapLayerNew.OnLayerListChange;
begin
  ViewUpdateLock;
  try
    SetNeedUpdateLayerProvider;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMiniMapLayerNew.OnMainMapChange;
begin
  ViewUpdateLock;
  try
    SetNeedUpdateLayerProvider;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMiniMapLayerNew.StartThreads;
begin
  OnConfigChange;
  inherited;
end;

end.
