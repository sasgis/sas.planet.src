unit u_MapLayerGridsNew;

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
  i_ValueToStringConverter,
  i_ViewPortState,
  i_MapLayerGridsConfig,
  u_TiledLayerWithThreadBase;

type
  TMapLayerGridsNew = class(TTiledLayerWithThreadBase)
  private
    FConfig: IMapLayerGridsConfig;
    FValueToStringConverterConfig: IValueToStringConverterConfig;
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
      const AValueToStringConverterConfig: IValueToStringConverterConfig;
      const AConfig: IMapLayerGridsConfig
    );
  end;

implementation

uses
  GR32,
  i_TileMatrix,
  u_TileMatrixFactory,
  u_ListenerByEvent,
  u_BitmapLayerProviderComplex,
  u_BitmapLayerProviderGridGenshtab,
  u_BitmapLayerProviderGridTiles;

{ TMapLayerGridsNew }

constructor TMapLayerGridsNew.Create(
  const APerfList: IInternalPerformanceCounterList; const AAppStartedNotifier,
  AAppClosingNotifier: INotifierOneOperation; AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const AResamplerConfig: IImageResamplerConfig;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  const ATimerNoifier: INotifier;
  const AValueToStringConverterConfig: IValueToStringConverterConfig;
  const AConfig: IMapLayerGridsConfig);
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
    True,
    AConfig.ThreadConfig
  );
  FConfig := AConfig;
  FValueToStringConverterConfig := AValueToStringConverterConfig;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(OnConfigChange),
    FConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(OnConfigChange),
    FValueToStringConverterConfig.ChangeNotifier
  );
end;

function TMapLayerGridsNew.CreateLayerProvider(
  const ALayerConverter: ILocalCoordConverter): IBitmapLayerProvider;
var
  VVisible: Boolean;
  VColor: TColor32;
  VUseRelativeZoom: Boolean;
  VZoom: Integer;
  VShowText: Boolean;
  VShowLines: Boolean;
  VScale: Integer;
  VProvider: IBitmapLayerProvider;
begin
  Result := nil;
  FConfig.TileGrid.LockRead;
  try
    VVisible := FConfig.TileGrid.Visible;
    VColor := FConfig.TileGrid.GridColor;
    VUseRelativeZoom := FConfig.TileGrid.UseRelativeZoom;
    VZoom := FConfig.TileGrid.Zoom;
    VShowText := FConfig.TileGrid.ShowText;
    VShowLines := True;
  finally
    FConfig.TileGrid.UnlockRead;
  end;
  if VVisible then begin
    Result :=
      TBitmapLayerProviderGridTiles.Create(
        VColor,
        VUseRelativeZoom,
        VZoom,
        VShowText,
        VShowLines
      );
  end;
  FConfig.GenShtabGrid.LockRead;
  try
    VVisible := FConfig.GenShtabGrid.Visible;
    VColor := FConfig.GenShtabGrid.GridColor;
    VScale := FConfig.GenShtabGrid.Scale;
    VShowText := FConfig.GenShtabGrid.ShowText;
    VShowLines := True;
  finally
    FConfig.GenShtabGrid.UnlockRead;
  end;
  if VVisible then begin
    VProvider :=
      TBitmapLayerProviderGridGenshtab.Create(
        VColor,
        VScale,
        VShowText,
        VShowLines
      );

    if Result <> nil then begin
      Result := TBitmapLayerProviderComplex.Create(Result, VProvider);
    end else begin
      Result := VProvider;
    end;
  end;
end;

procedure TMapLayerGridsNew.OnConfigChange;
begin
  ViewUpdateLock;
  try
    Visible := FConfig.TileGrid.Visible or FConfig.GenShtabGrid.Visible;
    SetNeedUpdateLayerProvider;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerGridsNew.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
