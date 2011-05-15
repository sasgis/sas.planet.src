unit u_MapLayerWithThreadDraw;

interface

uses
  Windows,
  Classes,
  GR32,
  GR32_Image,
  i_JclNotify,
  t_CommonTypes,
  i_BackgroundTask,
  i_ImageResamplerConfig,
  i_LayerBitmapClearStrategy,
  i_CoordConverter,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_ViewPortState,
  u_MapLayerBasic;

type
  TMapLayerWithThreadDraw = class(TMapLayerBasic)
  private
    FDrawTask: IBackgroundTask;
    FUpdateCounter: Integer;
    procedure OnDrawBitmap(AIsStop: TIsCancelChecker);
    procedure OnTimer(Sender: TObject);
  protected
    procedure DrawBitmap(AIsStop: TIsCancelChecker); virtual; abstract;
    procedure SetBitmapChanged;
    property DrawTask: IBackgroundTask read FDrawTask;
  protected
    procedure SetNeedRedraw; override;
    procedure SetNeedUpdateLayerSize; override;
    procedure DoRedraw; override;
  public
    constructor Create(
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      ATimerNoifier: IJclNotifier;
      APriority: TThreadPriority
    );
    destructor Destroy; override;
    procedure StartThreads; override;
    procedure SendTerminateToThreads; override;
  end;

  TMapLayerTiledWithThreadDraw = class(TMapLayerWithThreadDraw)
  private
    FClearStrategy: ILayerBitmapClearStrategy;
    FClearStrategyFactory: ILayerBitmapClearStrategyFactory;
    FConverterFactory: ILocalCoordConverterFactorySimpe;
  protected
    procedure SetLayerCoordConverter(AValue: ILocalCoordConverter); override;
    procedure ClearLayerBitmap; override;
    function CreateConverterForTileImage(AGeoConverter: ICoordConverter; ATile: TPoint; AZoom: Byte): ILocalCoordConverter;
  public
    constructor Create(
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      AResamplerConfig: IImageResamplerConfig;
      ATimerNoifier: IJclNotifier;
      APriority: TThreadPriority
    );
  end;

implementation

uses
  Types,
  SysUtils,
  u_NotifyEventListener,
  u_BackgroundTaskLayerDrawBase,
  u_LocalCoordConverterFactorySimpe,
  u_LayerBitmapClearStrategyFactory,
  u_GeoFun;

{ TMapLayerWithThreadDraw }

constructor TMapLayerWithThreadDraw.Create(
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  ATimerNoifier: IJclNotifier;
  APriority: TThreadPriority
);
begin
  inherited Create(AParentMap, AViewPortState);
  Layer.Bitmap.BeginUpdate;
  FDrawTask := TBackgroundTaskLayerDrawBase.Create(OnDrawBitmap, APriority);
  FUpdateCounter := 0;

  LinksList.Add(
    TNotifyEventListener.Create(Self.OnTimer),
    ATimerNoifier
  );
end;

destructor TMapLayerWithThreadDraw.Destroy;
begin
  FDrawTask := nil;
  inherited;
end;

procedure TMapLayerWithThreadDraw.DoRedraw;
begin
  FDrawTask.StopExecute;
  inherited;
  if Visible then begin
    FDrawTask.StartExecute;
  end;
end;

procedure TMapLayerWithThreadDraw.OnDrawBitmap(AIsStop: TIsCancelChecker);
begin
  DrawBitmap(AIsStop);
end;

procedure TMapLayerWithThreadDraw.OnTimer(Sender: TObject);
begin
  if InterlockedExchange(FUpdateCounter, 0) > 0 then begin
    Layer.Changed;
  end;
end;

procedure TMapLayerWithThreadDraw.SendTerminateToThreads;
begin
  inherited;
  FDrawTask.Terminate;
end;

procedure TMapLayerWithThreadDraw.SetBitmapChanged;
begin
  InterlockedIncrement(FUpdateCounter);
end;

procedure TMapLayerWithThreadDraw.SetNeedRedraw;
begin
  FDrawTask.StopExecute;
  inherited;
end;

procedure TMapLayerWithThreadDraw.SetNeedUpdateLayerSize;
begin
  FDrawTask.StopExecute;
  inherited;
end;

procedure TMapLayerWithThreadDraw.StartThreads;
begin
  inherited;
  FDrawTask.Start;
  if Visible then begin
    FDrawTask.StartExecute;
  end;
end;

{ TMapLayerTiledWithThreadDraw }

constructor TMapLayerTiledWithThreadDraw.Create(AParentMap: TImage32;
  AViewPortState: IViewPortState; AResamplerConfig: IImageResamplerConfig;
  ATimerNoifier: IJclNotifier; APriority: TThreadPriority);
begin
  inherited Create(AParentMap, AViewPortState, ATimerNoifier, APriority);
  FClearStrategyFactory := TLayerBitmapClearStrategyFactory.Create(AResamplerConfig);
  FConverterFactory := TLocalCoordConverterFactorySimpe.Create;
end;

function TMapLayerTiledWithThreadDraw.CreateConverterForTileImage(
  AGeoConverter: ICoordConverter; ATile: TPoint; AZoom: Byte): ILocalCoordConverter;
var
  VTileRect: TRect;
  VBitmapTileRect: TRect;
begin
  VTileRect := AGeoConverter.TilePos2PixelRect(ATile, AZoom);
  VBitmapTileRect.Left := 0;
  VBitmapTileRect.Top := 0;
  VBitmapTileRect.Right := VTileRect.Right - VTileRect.Left;
  VBitmapTileRect.Bottom := VTileRect.Bottom - VTileRect.Top;
  Result := FConverterFactory.CreateConverter(VBitmapTileRect, AZoom, AGeoConverter, DoublePoint(1, 1), DoublePoint(VTileRect.TopLeft));
end;

procedure TMapLayerTiledWithThreadDraw.ClearLayerBitmap;
begin
  if Visible then begin
    Layer.Bitmap.Lock;
    try
      if FClearStrategy <> nil then begin
        FClearStrategy.Clear(Layer.Bitmap);
        FClearStrategy := nil;
      end;
    finally
      Layer.Bitmap.UnLock;
    end;
  end;
end;

procedure TMapLayerTiledWithThreadDraw.SetLayerCoordConverter(
  AValue: ILocalCoordConverter);
begin
  Layer.Bitmap.Lock;
  try
    if Visible then begin
      FClearStrategy := FClearStrategyFactory.GetStrategy(LayerCoordConverter, AValue, Layer.Bitmap, FClearStrategy);
    end else begin
      FClearStrategy := nil;
    end;
  finally
    Layer.Bitmap.Unlock;
  end;
  inherited;
end;

end.
