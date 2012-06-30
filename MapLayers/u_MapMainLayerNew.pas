unit u_MapMainLayerNew;

interface

uses
  Types,
  SysUtils,
  GR32_Image,
  i_JclNotify,
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
    FMapsConfig: IMainMapsConfig;
    FConfig: IMainMapLayerConfig;

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
  public
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppClosingNotifier: IJclNotifier;
      AParentMap: TImage32;
      const AViewPortState: IViewPortState;
      const AResamplerConfig: IImageResamplerConfig;
      const AConverterFactory: ILocalCoordConverterFactorySimpe;
      const AMapsConfig: IMainMapsConfig;
      const APostProcessingConfig: IBitmapPostProcessingConfig;
      const AConfig: IMainMapLayerConfig;
      const AErrorLogger: ITileErrorLogger;
      const ATimerNoifier: IJclNotifier
    );
  end;

implementation

uses
  ActiveX,
  t_GeoTypes,
  i_TileRectUpdateNotifier,
  i_CoordConverter,
  u_NotifyEventListener,
  u_MapTypeListStatic,
  u_MapTypeListChangeableActiveBitmapLayers,
  u_BitmapLayerProviderForViewMaps,
  u_Synchronizer;

{ TMapMainLayerNew }

constructor TMapMainLayerNew.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppClosingNotifier: IJclNotifier;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const AResamplerConfig: IImageResamplerConfig;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  const AMapsConfig: IMainMapsConfig;
  const APostProcessingConfig: IBitmapPostProcessingConfig;
  const AConfig: IMainMapLayerConfig;
  const AErrorLogger: ITileErrorLogger;
  const ATimerNoifier: IJclNotifier
);
begin
  inherited Create(
    APerfList,
    AAppClosingNotifier,
    AParentMap,
    AViewPortState,
    AResamplerConfig,
    AConverterFactory,
    ATimerNoifier,
    False,
    AConfig.ThreadConfig
  );
  FMapsConfig := AMapsConfig;
  FErrorLogger := AErrorLogger;
  FPostProcessingConfig := APostProcessingConfig;
  FConfig := AConfig;

  FLayesList := TMapTypeListChangeableActiveBitmapLayers.Create(FMapsConfig.GetActiveBitmapLayersSet);

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnMainMapChange),
    FMapsConfig.GetActiveMap.GetChangeNotifier
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
  VMainMap := FMapsConfig.GetSelectedMapType;
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
