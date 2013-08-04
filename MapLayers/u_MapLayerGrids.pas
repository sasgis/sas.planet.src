unit u_MapLayerGrids;

interface

uses
  GR32_Image,
  i_Notifier,
  i_NotifierTime,
  i_NotifierOperation,
  i_InternalPerformanceCounter,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_LocalCoordConverterFactorySimpe,
  i_BitmapLayerProvider,
  i_ImageResamplerConfig,
  i_ValueToStringConverter,
  i_Bitmap32StaticFactory,
  i_MapLayerGridsConfig,
  u_TiledLayerWithThreadBase;

type
  TMapLayerGrids = class(TTiledLayerWithThreadBase)
  private
    FBitmapFactory: IBitmap32StaticFactory;
    FConfig: IMapLayerGridsConfig;
    FValueToStringConverterConfig: IValueToStringConverterConfig;
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
      const ATimerNoifier: INotifierTime;
      const ABitmapFactory: IBitmap32StaticFactory;
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
  u_BitmapLayerProviderGridDegree,
  u_BitmapLayerProviderGridTiles;

{ TMapLayerGridsNew }

constructor TMapLayerGrids.Create(
  const APerfList: IInternalPerformanceCounterList; const AAppStartedNotifier,
  AAppClosingNotifier: INotifierOneOperation; AParentMap: TImage32;
  const APosition: ILocalCoordConverterChangeable;
  const AView: ILocalCoordConverterChangeable;
  const ATileMatrixDraftResamplerConfig: IImageResamplerConfig;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  const ATimerNoifier: INotifierTime;
  const ABitmapFactory: IBitmap32StaticFactory;
  const AValueToStringConverterConfig: IValueToStringConverterConfig;
  const AConfig: IMapLayerGridsConfig);
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
    APosition,
    AView,
    VTileMatrixFactory,
    ATimerNoifier,
    False,
    AConfig.ThreadConfig
  );
  FBitmapFactory := ABitmapFactory;
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

function TMapLayerGrids.CreateLayerProvider(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALayerConverter: ILocalCoordConverter): IBitmapLayerProvider;
var
  VVisible: Boolean;
  VColor: TColor32;
  VUseRelativeZoom: Boolean;
  VZoom: Integer;
  VShowText: Boolean;
  VShowLines: Boolean;
  VScale: Integer;
  VScaleDegree: Double;
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
        FBitmapFactory,
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
        FBitmapFactory,
        VColor,
        VScale,
        VShowText,
        VShowLines
      );

    if Result <> nil then begin
      Result := TBitmapLayerProviderComplex.Create(FBitmapFactory, Result, VProvider);
    end else begin
      Result := VProvider;
    end;
  end;
  FConfig.DegreeGrid.LockRead;
  try
    VVisible := FConfig.DegreeGrid.Visible;
    VColor := FConfig.DegreeGrid.GridColor;
    VScaleDegree := FConfig.DegreeGrid.Scale;
    VShowText := FConfig.DegreeGrid.ShowText;
    VShowLines := True;
  finally
    FConfig.DegreeGrid.UnlockRead;
  end;
  if VVisible then begin
    VProvider :=
      TBitmapLayerProviderGridDegree.Create(
        FBitmapFactory,
        VColor,
        VScaleDegree,
        VShowText,
        VShowLines,
        FValueToStringConverterConfig.GetStatic
      );
    if Result <> nil then begin
      Result := TBitmapLayerProviderComplex.Create(FBitmapFactory, Result, VProvider);
    end else begin
      Result := VProvider;
    end;
  end;
end;

procedure TMapLayerGrids.OnConfigChange;
begin
  ViewUpdateLock;
  try
    Visible := FConfig.TileGrid.Visible or FConfig.GenShtabGrid.Visible or FConfig.DegreeGrid.Visible;
    SetNeedUpdateLayerProvider;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerGrids.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
