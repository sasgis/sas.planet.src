unit u_MapLayerWithThreadDraw;

interface

uses
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
  i_SimpleFlag,
  i_ViewPortState,
  u_MapLayerBasic;

type
  TMapLayerWithThreadDraw = class(TMapLayerBasic)
  private
    FDrawTask: IBackgroundTask;
    FUpdateViewFlag: ISimpleFlag;
    FBgDrawCounter: IInternalPerformanceCounter;
    FDelicateRedrawFlag: ISimpleFlag;
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
      const AAppClosingNotifier: INotifier;
      AParentMap: TImage32;
      const AViewPortState: IViewPortState;
      const AResamplerConfig: IImageResamplerConfig;
      const AConverterFactory: ILocalCoordConverterFactorySimpe;
      const ATimerNoifier: INotifier;
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
      const AAppClosingNotifier: INotifier;
      AParentMap: TImage32;
      const AViewPortState: IViewPortState;
      const AResamplerConfig: IImageResamplerConfig;
      const AConverterFactory: ILocalCoordConverterFactorySimpe;
      const AClearStrategyFactory: ILayerBitmapClearStrategyFactory;
      const ATimerNoifier: INotifier;
      const AThreadConfig: IThreadConfig
    );
  end;

implementation

uses
  u_NotifyEventListener,
  u_SimpleFlagWithInterlock,
  u_BackgroundTask;

{ TMapLayerWithThreadDraw }

constructor TMapLayerWithThreadDraw.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppClosingNotifier: INotifier;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const AResamplerConfig: IImageResamplerConfig;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  const ATimerNoifier: INotifier;
  const AThreadConfig: IThreadConfig
);
begin
  inherited Create(APerfList, AParentMap, AViewPortState, AResamplerConfig, AConverterFactory);
  FBgDrawCounter := PerfList.CreateAndAddNewCounter('BgDraw');
  Layer.Bitmap.BeginUpdate;
  FDrawTask := TBackgroundTask.Create(AAppClosingNotifier, OnDrawBitmap, AThreadConfig);
  FUpdateViewFlag := TSimpleFlagWithInterlock.Create;
  FDelicateRedrawFlag := TSimpleFlagWithInterlock.Create;

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
  VNeedRedraw: Boolean;
begin
  FDelicateRedrawFlag.CheckFlagAndReset;
  VNeedRedraw := True;
  while VNeedRedraw do begin
    VCounterContext := FBgDrawCounter.StartOperation;
    try
      DrawBitmap(AOperationID, ACancelNotifier);
    finally
      if Assigned(FBgDrawCounter) then begin
        FBgDrawCounter.FinishOperation(VCounterContext);
      end;
    end;

    VNeedRedraw := FDelicateRedrawFlag.CheckFlagAndReset;
  end;
end;

procedure TMapLayerWithThreadDraw.OnTimer;
begin
  if FUpdateViewFlag.CheckFlagAndReset then begin
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
  FUpdateViewFlag.SetFlag;
end;

procedure TMapLayerWithThreadDraw.DelicateRedraw;
begin
  FDelicateRedrawFlag.SetFlag;
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
  const AAppClosingNotifier: INotifier;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const AResamplerConfig: IImageResamplerConfig;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  const AClearStrategyFactory: ILayerBitmapClearStrategyFactory;
  const ATimerNoifier: INotifier;
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

