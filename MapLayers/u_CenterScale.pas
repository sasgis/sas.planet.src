unit u_CenterScale;

interface

uses
  GR32,
  GR32_Image,
  GR32_Layers,
  t_GeoTypes,
  i_NotifierOperation,
  i_MarkerDrawable,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_InternalPerformanceCounter,
  i_CenterScaleConfig,
  u_WindowLayerBasic;

type
  TLayerCenterScale = class(TWindowLayerAbstract)
  private
    FConfig: ICenterScaleConfig;
    FMarkerChangeable: IMarkerDrawableChangeable;
    FPosition: ILocalCoordConverterChangeable;

    FLayer: TCustomLayer;

    FFixedPoint: TDoublePoint;

    procedure OnConfigChange;
    procedure OnPosChange;
    procedure OnPaintLayer(
      Sender: TObject;
      Buffer: TBitmap32
    );
  protected
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const APosition: ILocalCoordConverterChangeable;
      const AMarkerChangeable: IMarkerDrawableChangeable;
      const AConfig: ICenterScaleConfig
    );
  end;

implementation

uses
  u_ListenerByEvent,
  u_GeoFun;

{ TLayerCenterScale }

constructor TLayerCenterScale.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const APosition: ILocalCoordConverterChangeable;
  const AMarkerChangeable: IMarkerDrawableChangeable;
  const AConfig: ICenterScaleConfig
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier
  );
  FConfig := AConfig;
  FPosition := APosition;
  FMarkerChangeable := AMarkerChangeable;

  FLayer := TBitmapLayer.Create(AParentMap.Layers);
  FLayer.MouseEvents := false;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnPosChange),
    FPosition.ChangeNotifier
  );
end;

procedure TLayerCenterScale.OnConfigChange;
var
  VVisible: Boolean;
begin
  VVisible := FConfig.Visible;
  if VVisible then begin
    FLayer.Visible := True;
    FLayer.Changed;
  end else begin
    FLayer.Visible := False;
  end;
end;

procedure TLayerCenterScale.OnPaintLayer(Sender: TObject; Buffer: TBitmap32);
var
  VMarker: IMarkerDrawable;
begin
  VMarker := FMarkerChangeable.GetStatic;
  if VMarker <> nil then begin
    VMarker.DrawToBitmap(Buffer, FFixedPoint);
  end;
end;

procedure TLayerCenterScale.OnPosChange;
var
  VNewFixedPoint: TDoublePoint;
begin
  if FLayer.Visible then begin
    VNewFixedPoint := RectCenter(FPosition.GetStatic.GetLocalRect);
    if not DoublePointsEqual(VNewFixedPoint, FFixedPoint) then begin
      FFixedPoint := VNewFixedPoint;
      FLayer.Changed;
    end;
  end;
end;

procedure TLayerCenterScale.StartThreads;
begin
  inherited;
  FLayer.OnPaint := Self.OnPaintLayer;
  OnConfigChange;
end;

end.
