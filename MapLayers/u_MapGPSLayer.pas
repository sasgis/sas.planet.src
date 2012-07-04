unit u_MapGPSLayer;

interface

uses
  GR32,
  GR32_Image,
  i_Notifier,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_InternalPerformanceCounter,
  i_NotifierOperation,
  i_LayerBitmapClearStrategy,
  i_ImageResamplerConfig,
  i_GPSRecorder,
  i_SimpleFlag,
  i_MapLayerGPSTrackConfig,
  i_ViewPortState,
  u_MapLayerWithThreadDraw;

type
  TMapGPSLayer = class(TMapLayerTiledWithThreadDraw)
  private
    FConfig: IMapLayerGPSTrackConfig;
    FGPSRecorder: IGPSRecorder;

    FGetTrackCounter: IInternalPerformanceCounter;
    FGpsPosChangeFlag: ISimpleFlag;
    procedure OnConfigChange;
    procedure OnGPSRecorderChange;
    procedure OnTimer;
  protected
    procedure DrawBitmap(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation
    ); override;
  public
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AViewPortState: IViewPortState;
      const AResamplerConfig: IImageResamplerConfig;
      const AConverterFactory: ILocalCoordConverterFactorySimpe;
      const AClearStrategyFactory: ILayerBitmapClearStrategyFactory;
      const ATimerNoifier: INotifier;
      const AConfig: IMapLayerGPSTrackConfig;
      const AGPSRecorder: IGPSRecorder
    );
  end;

implementation

uses
  GR32_Resamplers,
  i_CoordConverter,
  i_Bitmap32Static,
  i_BitmapLayerProvider,
  i_TileIterator,
  u_ListenerByEvent,
  u_SimpleFlagWithInterlock,
  u_BitmapLayerProviderByTrackPath,
  u_TileIteratorSpiralByRect;

{ TMapGPSLayer }

constructor TMapGPSLayer.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const AResamplerConfig: IImageResamplerConfig;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  const AClearStrategyFactory: ILayerBitmapClearStrategyFactory;
  const ATimerNoifier: INotifier;
  const AConfig: IMapLayerGPSTrackConfig;
  const AGPSRecorder: IGPSRecorder
);
begin
  inherited Create(
    APerfList,
    AAppClosingNotifier,
    AParentMap,
    AViewPortState,
    AResamplerConfig,
    AConverterFactory,
    AClearStrategyFactory,
    ATimerNoifier,
    AConfig.ThreadConfig
  );
  FConfig := AConfig;
  FGPSRecorder := AGPSRecorder;

  FGetTrackCounter := PerfList.CreateAndAddNewCounter('GetTrack');
  FGpsPosChangeFlag := TSimpleFlagWithInterlock.Create;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnTimer),
    ATimerNoifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnGPSRecorderChange),
    FGPSRecorder.GetChangeNotifier
  );
end;

procedure TMapGPSLayer.DrawBitmap(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation
);
var
  VTrackColorer: ITrackColorerStatic;
  VPointsCount: Integer;
  VLineWidth: Double;
  VBitmapConverter: ILocalCoordConverter;

  VTileIterator: ITileIterator;
  VGeoConvert: ICoordConverter;

  VZoom: Byte;
  { Прямоугольник пикселей растра в координатах основного конвертера }
  VBitmapOnMapPixelRect: TRect;
  { Прямоугольник тайлов текущего зума, покрывающий растр, в кооординатах
    основного конвертера }
  VTileSourceRect: TRect;
  { Текущий тайл в кооординатах основного конвертера }
  VTile: TPoint;
  { Прямоугольник пикслов текущего тайла в кооординатах основного конвертера }
  VCurrTilePixelRect: TRect;
  { Прямоугольник пикселов в которые будет скопирован текущий тайл }
  VCurrTileOnBitmapRect: TRect;
  VCounterContext: TInternalPerformanceCounterContext;
  VEnum: IEnumGPSTrackPoint;
  VProvider: IBitmapLayerProvider;
  VBitmapTile: IBitmap32Static;
  VTileConverter: ILocalCoordConverter;
begin
  inherited;
  FConfig.LockRead;
  try
    VPointsCount := FConfig.LastPointCount;
    VLineWidth := FConfig.LineWidth;
    VTrackColorer := FConfig.TrackColorerConfig.GetStatic;
  finally
    FConfig.UnlockRead
  end;

  if (VPointsCount > 1) then begin
    VBitmapConverter := LayerCoordConverter;
    VCounterContext := FGetTrackCounter.StartOperation;
    try
      VEnum := FGPSRecorder.LastPoints(VPointsCount);
      VProvider :=
        TBitmapLayerProviderByTrackPath.Create(
          VPointsCount,
          VLineWidth,
          VTrackColorer,
          VBitmapConverter.ProjectionInfo,
          VEnum
        );
    finally
      FGetTrackCounter.FinishOperation(VCounterContext);
    end;
    if not ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      VGeoConvert := VBitmapConverter.GetGeoConverter;
      VZoom := VBitmapConverter.GetZoom;

      VBitmapOnMapPixelRect := VBitmapConverter.GetRectInMapPixel;
      VGeoConvert.CheckPixelRect(VBitmapOnMapPixelRect, VZoom);

      VTileSourceRect := VGeoConvert.PixelRect2TileRect(VBitmapOnMapPixelRect, VZoom);
      VTileIterator := TTileIteratorSpiralByRect.Create(VTileSourceRect);
      while VTileIterator.Next(VTile) do begin
        if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
          break;
        end;
        VTileConverter := ConverterFactory.CreateForTile(VTile, VZoom, VGeoConvert);
        VCurrTilePixelRect := VTileConverter.GetRectInMapPixel;
        VCurrTileOnBitmapRect := VBitmapConverter.MapRect2LocalRect(VCurrTilePixelRect);

        VBitmapTile :=
          VProvider.GetBitmapRect(
            AOperationID,
            ACancelNotifier,
            VTileConverter
          );
        Layer.Bitmap.Lock;
        try
          if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
            break;
          end;
          if VBitmapTile <> nil then begin
            BlockTransfer(
              Layer.Bitmap,
              VCurrTileOnBitmapRect.Left,
              VCurrTileOnBitmapRect.Top,
              Layer.Bitmap.ClipRect,
              VBitmapTile.Bitmap,
              VBitmapTile.Bitmap.BoundsRect,
              dmOpaque
            );
          end else begin
            Layer.Bitmap.FillRectS(
              VCurrTileOnBitmapRect.Left,
              VCurrTileOnBitmapRect.Top,
              VCurrTileOnBitmapRect.Right,
              VCurrTileOnBitmapRect.Bottom,
              0
            );
          end;
          SetBitmapChanged;
        finally
          Layer.Bitmap.UnLock;
        end;
      end;
    end;
  end;
end;

procedure TMapGPSLayer.OnConfigChange;
begin
  ViewUpdateLock;
  try
    SetNeedRedraw;
    SetVisible(FConfig.Visible);
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapGPSLayer.OnGPSRecorderChange;
begin
  FGpsPosChangeFlag.SetFlag;
end;

procedure TMapGPSLayer.OnTimer;
begin
  if FGpsPosChangeFlag.CheckFlagAndReset then begin
    ViewUpdateLock;
    try
      SetNeedRedraw;
    finally
      ViewUpdateUnlock;
    end;
  end;
end;

procedure TMapGPSLayer.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
