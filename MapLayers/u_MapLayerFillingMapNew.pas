unit u_MapLayerFillingMapNew;

interface

uses
  GR32_Image,
  i_Notifier, 
  i_InternalPerformanceCounter,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_BitmapLayerProvider,
  i_ImageResamplerConfig,
  i_ViewPortState,
  i_TileMatrix,
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
    function CreteTileMatrix(
      const ASource: ITileMatrix;
      const ANewConverter: ILocalCoordConverter
    ): ITileMatrix; override;
  public
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppClosingNotifier: INotifier;
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
  u_NotifyEventListener,
  u_BitmapLayerProviderFillingMap;

{ TMapLayerFillingMapNew }

constructor TMapLayerFillingMapNew.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppClosingNotifier: INotifier;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const AResamplerConfig: IImageResamplerConfig;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  const ATimerNoifier: INotifier;
  const AConfig: IFillingMapLayerConfig
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
  VConfig := FConfig.GetStatic;

  Result :=
    TBitmapLayerProviderFillingMap.Create(
      VConfig.SourceMap,
      VConfig.GetActualZoom(ALayerConverter),
      VConfig.Colorer
    );
end;

function TMapLayerFillingMapNew.CreteTileMatrix(
  const ASource: ITileMatrix;
  const ANewConverter: ILocalCoordConverter
): ITileMatrix;
var
  VConfig: IFillingMapLayerConfigStatic;
begin
  Result := nil;
  VConfig := FConfig.GetStatic;
  if VConfig.Visible then begin
    Result := inherited CreteTileMatrix(ASource, ANewConverter);
  end;
end;

procedure TMapLayerFillingMapNew.OnConfigChange;
begin
  ViewUpdateLock;
  try
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


