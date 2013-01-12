unit u_MapLayerFillingMap;

interface

uses
  GR32_Image,
  i_NotifierTime,
  i_NotifierOperation,
  i_InternalPerformanceCounter,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_BitmapLayerProvider,
  i_Bitmap32StaticFactory,
  i_ImageResamplerConfig,
  i_ViewPortState,
  i_FillingMapLayerConfig,
  u_TiledLayerWithThreadBase;

type
  TMapLayerFillingMap = class(TTiledLayerWithThreadBase)
  private
    FConfig: IFillingMapLayerConfig;
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
      const AViewPortState: IViewPortState;
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
  u_TileMatrixFactory,
  u_ListenerByEvent,
  u_BitmapLayerProviderFillingMap;

{ TMapLayerFillingMapNew }

constructor TMapLayerFillingMap.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const ATileMatrixDraftResamplerConfig: IImageResamplerConfig;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  const ATimerNoifier: INotifierTime;
  const ABitmapFactory: IBitmap32StaticFactory;
  const AConfig: IFillingMapLayerConfig
);
var
  VTileMatrixFactory: ITileMatrixFactory;
begin
  VTileMatrixFactory :=
    TTileMatrixFactory.Create(
      ATileMatrixDraftResamplerConfig,
      ABitmapFactory,
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

function TMapLayerFillingMap.CreateLayerProvider(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
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

procedure TMapLayerFillingMap.OnConfigChange;
begin
  ViewUpdateLock;
  try
    Visible := FConfig.GetStatic.Visible;
    SetNeedUpdateLayerProvider;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerFillingMap.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
