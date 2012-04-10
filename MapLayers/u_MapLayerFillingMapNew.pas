unit u_MapLayerFillingMapNew;

interface

uses
  GR32_Image,
  i_JclNotify,
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
    function CreateLayerProvider(ALayerConverter: ILocalCoordConverter): IBitmapLayerProvider; override;
    function CreteTileMatrix(ASource: ITileMatrix; ANewConverter: ILocalCoordConverter): ITileMatrix; override;
  public
    procedure StartThreads; override;
  public
    constructor Create(
      APerfList: IInternalPerformanceCounterList;
      AAppClosingNotifier: IJclNotifier;
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      AResamplerConfig: IImageResamplerConfig;
      AConverterFactory: ILocalCoordConverterFactorySimpe;
      ATimerNoifier: IJclNotifier;
      AConfig: IFillingMapLayerConfig
    );
  end;

implementation

uses
  u_NotifyEventListener,
  u_BitmapLayerProviderFillingMap;

{ TMapLayerFillingMapNew }

constructor TMapLayerFillingMapNew.Create(
  APerfList: IInternalPerformanceCounterList; AAppClosingNotifier: IJclNotifier;
  AParentMap: TImage32; AViewPortState: IViewPortState;
  AResamplerConfig: IImageResamplerConfig;
  AConverterFactory: ILocalCoordConverterFactorySimpe;
  ATimerNoifier: IJclNotifier; AConfig: IFillingMapLayerConfig);
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
  ALayerConverter: ILocalCoordConverter
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

function TMapLayerFillingMapNew.CreteTileMatrix(ASource: ITileMatrix;
  ANewConverter: ILocalCoordConverter): ITileMatrix;
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
