unit u_MapLayerFillingMapNew;

interface

uses
  GR32_Image,
  i_Notifier,
  i_NotifierOperation,
  i_InternalPerformanceCounter,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_BitmapLayerProvider,
  i_ImageResamplerConfig,
  i_ViewPortState,
  i_FillingMapLayerConfig,
  u_TiledLayerWithThreadBase;

type
  TMapLayerFillingMapNew = class(TTiledLayerWithThreadBase)
  private
    FConfig: IFillingMapLayerConfig;
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
      const ATimerNoifier: INotifier;
      const AConfig: IFillingMapLayerConfig
    );
  end;

implementation

uses
  i_TileMatrix,
  u_TileMatrixFactory,
  u_ListenerByEvent,
  u_BitmapLayerProviderFillingMap;

{ TMapLayerFillingMapNew }

constructor TMapLayerFillingMapNew.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const AResamplerConfig: IImageResamplerConfig;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  const ATimerNoifier: INotifier;
  const AConfig: IFillingMapLayerConfig
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
    AViewPortState,
    VTileMatrixFactory,
    AResamplerConfig,
    AConverterFactory,
    ATimerNoifier,
    True,
    AConfig.ThreadConfig
  );
  FConfig := AConfig;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(OnConfigChange),
    FConfig.GetChangeNotifier
  );
end;

function TMapLayerFillingMapNew.CreateLayerProvider(
  const ALayerConverter: ILocalCoordConverter
): IBitmapLayerProvider;
var
  VConfig: IFillingMapLayerConfigStatic;
begin
  Result := nil;
  VConfig := FConfig.GetStatic;
  if VConfig.Visible then begin
    Result :=
      TBitmapLayerProviderFillingMap.Create(
        VConfig.SourceMap,
        VConfig.GetActualZoom(ALayerConverter),
        VConfig.Colorer
      );
  end;
end;

procedure TMapLayerFillingMapNew.OnConfigChange;
begin
  ViewUpdateLock;
  try
    Visible := FConfig.GetStatic.Visible;
    SetNeedUpdateLayerProvider;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerFillingMapNew.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
