unit u_FullMapMouseCursorLayer;

interface

uses
  GR32,
  GR32_Image,
  i_Notifier, 
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
    FColor: TColor32;
    procedure OnMainFormStateChange;
    procedure OnConfigChange;
    procedure OnTimerEvent;
  protected
    procedure PaintLayer(
      ABuffer: TBitmap32;
      const ALocalConverter: ILocalCoordConverter
    ); override;
  public
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
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
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const AMainFormState: IMainFormState;
  const ATimerNoifier: INotifier;
  const AMouseState: IMouseState;
  const AConfig: IFullMapMouseCursorLayerConfig
);
begin
  inherited Create(APerfList, AParentMap, AViewPortState);
  FConfig := AConfig;
  FMainFormState := AMainFormState;
  FMouseState := AMouseState;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnMainFormStateChange),
    FMainFormState.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnTimerEvent),
    ATimerNoifier
  );
  Layer.Visible := True;
end;

procedure TFullMapMouseCursorLayer.OnConfigChange;
begin
  FColor := FConfig.LineColor;
  if FConfig.Enabled then begin
    if (FMainFormState.State <> ao_movemap) or (FConfig.ShowAlways) then begin
      Redraw;
    end;
  end;
end;

procedure TFullMapMouseCursorLayer.OnMainFormStateChange;
begin
  if FConfig.Enabled then begin
    if (FMainFormState.State <> ao_movemap) or (FConfig.ShowAlways) then begin
      Redraw;
    end;
  end;
end;

procedure TFullMapMouseCursorLayer.OnTimerEvent;
var
  VPos: TPoint;
begin
  if FConfig.Enabled then begin
    if (FMainFormState.State <> ao_movemap) or (FConfig.ShowAlways) then begin
      VPos := FMouseState.CurentPos;
      if (VPos.X <> FLastPos.X) or (VPos.Y <> FLastPos.Y) then begin
        Layer.Changed;
      end;
    end;
  end;
end;

procedure TFullMapMouseCursorLayer.PaintLayer(ABuffer: TBitmap32;
  const ALocalConverter: ILocalCoordConverter
);
var
  VPos: TPoint;
begin
  inherited;
  if FConfig.Enabled then begin
    if (FMainFormState.State <> ao_movemap) or (FConfig.ShowAlways) then begin
      VPos := FMouseState.CurentPos;
      ABuffer.VertLineS(VPos.X, 0, ABuffer.Height, FColor);
      ABuffer.HorzLineS(0, VPos.Y, ABuffer.Width, FColor);
      FLastPos := VPos;
    end;
  end;
end;

procedure TFullMapMouseCursorLayer.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.


