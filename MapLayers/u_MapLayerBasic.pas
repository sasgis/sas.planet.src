unit u_MapLayerBasic;

interface

uses
  GR32,
  GR32_Layers,
  GR32_Image,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_ViewPortState,
  i_InternalPerformanceCounter,
  u_WindowLayerWithPos;

type
  TMapLayerBasicNoBitmap = class(TWindowLayerBasic)
  private
    FOnPaintCounter: IInternalPerformanceCounter;
    procedure OnPaintLayer(
      Sender: TObject;
      Buffer: TBitmap32
    );
  protected
    procedure PaintLayer(
      ABuffer: TBitmap32;
      const ALocalConverter: ILocalCoordConverter
    ); virtual; abstract;
  protected
    procedure DoRedraw; override;
    procedure SetViewCoordConverter(const AValue: ILocalCoordConverter); override;
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AViewPortState: IViewPortState
    );
  end;

implementation

{ TMapLayerBasicNoBitmap }

constructor TMapLayerBasicNoBitmap.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    TCustomLayer.Create(AParentMap.Layers),
    AViewPortState,
    True
  );
  FOnPaintCounter := PerfList.CreateAndAddNewCounter('OnPaint');
end;

procedure TMapLayerBasicNoBitmap.DoRedraw;
begin
  inherited;
  Layer.Changed;
end;

procedure TMapLayerBasicNoBitmap.OnPaintLayer(
  Sender: TObject;
  Buffer: TBitmap32
);
var
  VLocalConverter: ILocalCoordConverter;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  VLocalConverter := ViewCoordConverter;
  if VLocalConverter <> nil then begin
    VCounterContext := FOnPaintCounter.StartOperation;
    try
      PaintLayer(Buffer, VLocalConverter);
    finally
      FOnPaintCounter.FinishOperation(VCounterContext);
    end;
  end;
end;

procedure TMapLayerBasicNoBitmap.SetViewCoordConverter(
  const AValue: ILocalCoordConverter
);
var
  VLocalConverter: ILocalCoordConverter;
begin
  VLocalConverter := ViewCoordConverter;
  if (VLocalConverter = nil) or (not VLocalConverter.GetIsSameConverter(AValue)) then begin
    SetNeedRedraw;
  end;
  inherited;
end;

procedure TMapLayerBasicNoBitmap.StartThreads;
begin
  inherited;
  Layer.OnPaint := OnPaintLayer;
end;

end.
