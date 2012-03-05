unit u_MapLayerWithThreadDraw;

interface

uses
  Windows,
  Classes,
  GR32,
  GR32_Image,
  i_JclNotify,
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
    procedure OnDrawBitmap(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier
    );
    procedure OnTimer;
  protected
    procedure DrawBitmap(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier
    ); virtual; abstract;
    procedure SetBitmapChanged;
    property DrawTask: IBackgroundTask read FDrawTask;
  protected
    procedure SetNeedRedraw; override;
    procedure SetNeedUpdateLayerSize; override;
    procedure DoRedraw; override;
  public
    constructor Create(
      APerfList: IInternalPerformanceCounterList;
      AAppClosingNotifier: IJclNotifier;
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      AResamplerConfig: IImageResamplerConfig;
      AConverterFactory: ILocalCoordConverterFactorySimpe;
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
  protected
    procedure SetLayerCoordConverter(AValue: ILocalCoordConverter); override;
    procedure ClearLayerBitmap; override;
  public
    constructor Create(
      APerfList: IInternalPerformanceCounterList;
      AAppClosingNotifier: IJclNotifier;
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      AResamplerConfig: IImageResamplerConfig;
      AConverterFactory: ILocalCoordConverterFactorySimpe;
      AClearStrategyFactory: ILayerBitmapClearStrategyFactory;
      ATimerNoifier: IJclNotifier;
      APriority: TThreadPriority
    );
  end;

implementation

uses
  u_NotifyEventListener,
  u_BackgroundTaskLayerDrawBase;

{ TMapLayerWithThreadDraw }

constructor TMapLayerWithThreadDraw.Create(
  APerfList: IInternalPerformanceCounterList;
  AAppClosingNotifier: IJclNotifier;
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  AResamplerConfig: IImageResamplerConfig;
  AConverterFactory: ILocalCoordConverterFactorySimpe;
  ATimerNoifier: IJclNotifier;
  APriority: TThreadPriority
);
begin
  inherited Create(APerfList, AParentMap, AViewPortState, AResamplerConfig, AConverterFactory);
  FBgDrawCounter := PerfList.CreateAndAddNewCounter('BgDraw');
  Layer.Bitmap.BeginUpdate;
  FDrawTask := TBackgroundTaskLayerDrawBase.Create(AAppClosingNotifier, OnDrawBitmap, APriority);
  FUpdateCounter := 0;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnTimer),
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

procedure TMapLayerWithThreadDraw.OnDrawBitmap(
  AOperationID: Integer;
  ACancelNotifier: IOperationNotifier
);
var
  VCounterContext: TInternalPerformanceCounterContext;
begin
  VCounterContext := FBgDrawCounter.StartOperation;
  try
    DrawBitmap(AOperationID, ACancelNotifier);
  finally
    FBgDrawCounter.FinishOperation(VCounterContext);
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
  APerfList: IInternalPerformanceCounterList;
  AAppClosingNotifier: IJclNotifier;
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  AResamplerConfig: IImageResamplerConfig;
  AConverterFactory: ILocalCoordConverterFactorySimpe;
  AClearStrategyFactory: ILayerBitmapClearStrategyFactory;
  ATimerNoifier: IJclNotifier;
  APriority: TThreadPriority
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
    APriority
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
