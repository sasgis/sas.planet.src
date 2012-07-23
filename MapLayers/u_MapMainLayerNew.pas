unit u_MapMainLayerNew;

interface

uses
  GR32_Image,
  i_Notifier,
  i_NotifierOperation,
  i_TileError,
  i_BitmapPostProcessingConfig,
  i_ActiveMapsConfig,
  i_MainMapLayerConfig,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_BitmapLayerProvider,
  i_MapTypes,
  i_MapTypeListChangeable,
  i_InternalPerformanceCounter,
  i_ViewPortState,
  i_ImageResamplerConfig,
  u_TiledLayerWithThreadBase;

type
  TMapMainLayerNew = class(TTiledLayerWithThreadBase)
  private
    FErrorLogger: ITileErrorLogger;
    FPostProcessingConfig: IBitmapPostProcessingConfig;
    FConfig: IMainMapLayerConfig;
    FMainMap: IMapTypeChangeable;
    FLayesList: IMapTypeListChangeable;

    FUsePrevZoomAtMap: Boolean;
    FUsePrevZoomAtLayer: Boolean;
    procedure OnMainMapChange;
    procedure OnLayerListChange;
    procedure OnConfigChange;
  protected
    function CreateLayerProvider(
      const ALayerConverter: ILocalCoordConverter
    ): IBitmapLayerProvider; override;
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AViewPortState: IViewPortState;
      const AResamplerConfig: IImageResamplerConfig;
      const AConverterFactory: ILocalCoordConverterFactorySimpe;
      const AMainMap: IMapTypeChangeable;
      const ALayesList: IMapTypeListChangeable;
      const APostProcessingConfig: IBitmapPostProcessingConfig;
      const AConfig: IMainMapLayerConfig;
      const AErrorLogger: ITileErrorLogger;
      const ATimerNoifier: INotifier
    );
  end;

implementation

uses
  i_TileMatrix,
  u_TileMatrixFactory,
  u_ListenerByEvent,
  u_MapTypeListChangeableActiveBitmapLayers,
  u_BitmapLayerProviderForViewMaps;

{ TMapMainLayerNew }

constructor TMapMainLayerNew.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const AResamplerConfig: IImageResamplerConfig;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  const AMainMap: IMapTypeChangeable;
  const ALayesList: IMapTypeListChangeable;
  const APostProcessingConfig: IBitmapPostProcessingConfig;
  const AConfig: IMainMapLayerConfig;
  const AErrorLogger: ITileErrorLogger;
  const ATimerNoifier: INotifier
);
var
  VTileMatrixFactory: ITileMatrixFactory;
begin
  VTileMatrixFactory :=
    TTileMatrixFactory.Create(
      AResamplerConfig,
      AConverterFactory
    );
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AViewPortState.Position,
    AViewPortState.View,
    VTileMatrixFactory,
    AResamplerConfig,
    AConverterFactory,
    ATimerNoifier,
    False,
    AConfig.ThreadConfig
  );
  FMainMap := AMainMap;
  FLayesList := ALayesList;
  FErrorLogger := AErrorLogger;
  FPostProcessingConfig := APostProcessingConfig;
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
    FConfig.GetChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FPostProcessingConfig.GetChangeNotifier
  );
end;

function TMapMainLayerNew.CreateLayerProvider(
  const ALayerConverter: ILocalCoordConverter
): IBitmapLayerProvider;
var
  VMainMap: IMapType;
  VUsePrevZoomAtMap, VUsePrevZoomAtLayer: Boolean;
  VPostProcessingConfig: IBitmapPostProcessingConfigStatic;
  VLayersList: IMapTypeListStatic;
begin
  VMainMap := FMainMap.GetStatic;
  VLayersList := FLayesList.List;
  VUsePrevZoomAtMap := FUsePrevZoomAtMap;
  VUsePrevZoomAtLayer := FUsePrevZoomAtLayer;
  VPostProcessingConfig := FPostProcessingConfig.GetStatic;

  Result :=
    TBitmapLayerProviderForViewMaps.Create(
      VMainMap,
      VLayersList,
      VUsePrevZoomAtMap,
      VUsePrevZoomAtLayer,
      True,
      VPostProcessingConfig,
      FErrorLogger
    );
end;

procedure TMapMainLayerNew.OnConfigChange;
begin
  ViewUpdateLock;
  try
    FConfig.LockRead;
    try
      FUsePrevZoomAtMap := FConfig.UsePrevZoomAtMap;
      FUsePrevZoomAtLayer := FConfig.UsePrevZoomAtLayer;
    finally
      FConfig.UnlockRead;
    end;
    SetNeedUpdateLayerProvider;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapMainLayerNew.OnLayerListChange;
begin
  ViewUpdateLock;
  try
    SetNeedUpdateLayerProvider;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapMainLayerNew.OnMainMapChange;
begin
  ViewUpdateLock;
  try
    SetNeedUpdateLayerProvider;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapMainLayerNew.StartThreads;
begin
  OnConfigChange;
  inherited;
end;

end.
