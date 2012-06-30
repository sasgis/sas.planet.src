unit u_MapLayerWithThreadDraw;

interface

uses
  Windows,
  GR32,
  GR32_Image,
  i_JclNotify,
  i_ThreadConfig,
  i_BackgroundTask,
  i_OperationNotifier,
  i_ImageResamplerConfig,
  i_LayerBitmapClearStrategy,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_InternalPerformanceCounter,
  i_ViewPortState,
  u_MapLayerBasic;

type
  TMapLayerWithThreadDraw = class(TMapLayerBasic)
  private
    FDrawTask: IBackgroundTask;
    FUpdateCounter: Integer;
    FBgDrawCounter: IInternalPerformanceCounter;
    FDelicateRedrawCounter: Integer;
    procedure OnDrawBitmap(
      AOperationID: Integer;
      const ACancelNotifier: IOperationNotifier
    );
    procedure OnTimer;
  protected
    procedure DrawBitmap(
      AOperationID: Integer;
      const ACancelNotifier: IOperationNotifier
    ); virtual; abstract;
    procedure SetBitmapChanged;
    property DrawTask: IBackgroundTask read FDrawTask;
  protected
    procedure DelicateRedraw;
    procedure SetNeedRedraw; override;
    procedure SetNeedUpdateLayerSize; override;
    procedure DoRedraw; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppClosingNotifier: IJclNotifier;
      AParentMap: TImage32;
      const AViewPortState: IViewPortState;
      const AResamplerConfig: IImageResamplerConfig;
      const AConverterFactory: ILocalCoordConverterFactorySimpe;
      const ATimerNoifier: IJclNotifier;
      const AThreadConfig: IThreadConfig
    );
    destructor Destroy; override;
    procedure StartThreads; override;
    procedure SendTerminateToThreads; override;
  end;

  TMapLayerTiledWithThreadDraw = class(TMapLayerWithThreadDraw)
  private
    FClearStrategy: ILayerBitmapClearStrategy;
    FClearStrategyFactory: ILayerBitmapClearStrategyFactory;
  protected
    procedure SetLayerCoordConverter(const AValue: ILocalCoordConverter); override;
    procedure ClearLayerBitmap; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppClosingNotifier: IJclNotifier;
      AParentMap: TImage32;
      const AViewPortState: IViewPortState;
      const AResamplerConfig: IImageResamplerConfig;
      const AConverterFactory: ILocalCoordConverterFactorySimpe;
      const AClearStrategyFactory: ILayerBitmapClearStrategyFactory;
      const ATimerNoifier: IJclNotifier;
      const AThreadConfig: IThreadConfig
    );
  end;

implementation

uses
  u_NotifyEventListener,
  u_BackgroundTask;

{ TMapLayerWithThreadDraw }

constructor TMapLayerWithThreadDraw.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppClosingNotifier: IJclNotifier;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const AResamplerConfig: IImageResamplerConfig;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  const ATimerNoifier: IJclNotifier;
  const AThreadConfig: IThreadConfig
);
begin
  inherited Create(APerfList, AParentMap, AViewPortState, AResamplerConfig, AConverterFactory);
  FBgDrawCounter := PerfList.CreateAndAddNewCounter('BgDraw');
  Layer.Bitmap.BeginUpdate;
  FDrawTask := TBackgroundTask.Create(AAppClosingNotifier, OnDrawBitmap, AThreadConfig);
  FUpdateCounter := 0;
  FDelicateRedrawCounter := 0;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnTimer),
    ATimerNoifier
  );
end;

destructor TMapLayerWithThreadDraw.Destroy;
begin
  FDrawTask := nil;
  FBgDrawCounter := nil;
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

procedure TMapLayerWithThreadDraw.OnDrawBitmap(
  AOperationID: Integer;
  const ACancelNotifier: IOperationNotifier
);
var
  VCounterContext: TInternalPerformanceCounterContext;
  VDelicateRedrawCounter: Integer;
begin
  InterlockedExchange(FDelicateRedrawCounter, 0);
  VDelicateRedrawCounter := 1;
  while VDelicateRedrawCounter > 0 do begin
    VCounterContext := FBgDrawCounter.StartOperation;
    try
      DrawBitmap(AOperationID, ACancelNotifier);
    finally
      if Assigned(FBgDrawCounter) then begin
        FBgDrawCounter.FinishOperation(VCounterContext);
      end;
    end;

    VDelicateRedrawCounter := InterlockedExchange(FDelicateRedrawCounter, 0);
  end;
end;

procedure TMapLayerWithThreadDraw.OnTimer;
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

procedure TMapLayerWithThreadDraw.DelicateRedraw;
begin
  InterlockedIncrement(FDelicateRedrawCounter);
  FDrawTask.StartExecute;
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

constructor TMapLayerTiledWithThreadDraw.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppClosingNotifier: IJclNotifier;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const AResamplerConfig: IImageResamplerConfig;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  const AClearStrategyFactory: ILayerBitmapClearStrategyFactory;
  const ATimerNoifier: IJclNotifier;
  const AThreadConfig: IThreadConfig
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
    AThreadConfig
  );
  FClearStrategyFactory := AClearStrategyFactory;
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
  const AValue: ILocalCoordConverter
);
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
