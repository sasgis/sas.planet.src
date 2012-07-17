unit u_FullMapMouseCursorLayer;

interface

uses
  GR32,
  GR32_Image,
  i_Notifier,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_InternalPerformanceCounter,
  i_ViewPortState,
  i_FullMapMouseCursorLayerConfig,
  i_MainFormState,
  i_MouseState,
  u_MapLayerBasic;

type
  TFullMapMouseCursorLayer = class(TMapLayerBasicNoBitmap)
  private
    FConfig: IFullMapMouseCursorLayerConfig;
    FMainFormState: IMainFormState;
    FMouseState: IMouseState;

    FLastPos: TPoint;
    procedure OnConfigChange;
    procedure OnTimerEvent;
  protected
    procedure PaintLayer(
      ABuffer: TBitmap32;
      const ALocalConverter: ILocalCoordConverter
    ); override;
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AViewPortState: IViewPortState;
      const AMainFormState: IMainFormState;
      const ATimerNoifier: INotifier;
      const AMouseState: IMouseState;
      const AConfig: IFullMapMouseCursorLayerConfig
    );
  end;

implementation

uses
  u_ListenerByEvent;

{ TFullMapMouseCursorLayer }

constructor TFullMapMouseCursorLayer.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const AMainFormState: IMainFormState;
  const ATimerNoifier: INotifier;
  const AMouseState: IMouseState;
  const AConfig: IFullMapMouseCursorLayerConfig
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AViewPortState
  );
  FConfig := AConfig;
  FMainFormState := AMainFormState;
  FMouseState := AMouseState;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FMainFormState.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnTimerEvent),
    ATimerNoifier
  );
end;

procedure TFullMapMouseCursorLayer.OnConfigChange;
begin
  ViewUpdateLock;
  try
    if FConfig.Enabled and ((FMainFormState.State <> ao_movemap) or (FConfig.ShowAlways)) then begin
      Show;
    end else begin
      Hide;
    end;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TFullMapMouseCursorLayer.OnTimerEvent;
var
  VPos: TPoint;
begin
  if Visible then begin
    VPos := FMouseState.CurentPos;
    if (VPos.X <> FLastPos.X) or (VPos.Y <> FLastPos.Y) then begin
      Layer.Changed;
    end;
  end;
end;

procedure TFullMapMouseCursorLayer.PaintLayer(ABuffer: TBitmap32;
  const ALocalConverter: ILocalCoordConverter
);
var
  VPos: TPoint;
  VColor: TColor32;
begin
  inherited;
  VPos := FMouseState.CurentPos;
  VColor := FConfig.LineColor;
  ABuffer.VertLineS(VPos.X, 0, ABuffer.Height, VColor);
  ABuffer.HorzLineS(0, VPos.Y, ABuffer.Width, VColor);
  FLastPos := VPos;
end;

procedure TFullMapMouseCursorLayer.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.


