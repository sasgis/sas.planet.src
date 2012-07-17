unit u_MapLayerGoto;

interface

uses
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_Notifier,
  i_NotifierOperation,
  i_InternalPerformanceCounter,
  i_MarkerDrawable,
  i_ViewPortState,
  i_MapViewGoto,
  i_LocalCoordConverter,
  i_GotoLayerConfig,
  u_MapLayerBasic;

type
  TGotoLayer = class(TMapLayerBasicNoBitmap)
  private
    FConfig: IGotoLayerConfig;
    FMapGoto: IMapViewGoto;
    FMarkerChangeable: IMarkerDrawableChangeable;

    procedure OnTimer;
    procedure OnConfigChange;
    function GetIsVisible: Boolean;
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
      const ATimerNoifier: INotifier;
      const AViewPortState: IViewPortState;
      const AMarkerChangeable: IMarkerDrawableChangeable;
      const AMapGoto: IMapViewGoto;
      const AConfig: IGotoLayerConfig
    );
  end;

implementation

uses
  Math,
  SysUtils,
  i_Listener,
  i_CoordConverter,
  u_ListenerByEvent,
  u_GeoFun;

{ TGotoLayer }

constructor TGotoLayer.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const ATimerNoifier: INotifier;
  const AViewPortState: IViewPortState;
  const AMarkerChangeable: IMarkerDrawableChangeable;
  const AMapGoto: IMapViewGoto;
  const AConfig: IGotoLayerConfig
);
var
  VListener: IListener;
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AViewPortState
  );
  FConfig := AConfig;
  FMarkerChangeable := AMarkerChangeable;
  FMapGoto := AMapGoto;

  VListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  LinksList.Add(
    VListener,
    FConfig.GetChangeNotifier
  );

  LinksList.Add(
    VListener,
    FMarkerChangeable.ChangeNotifier
  );

  LinksList.Add(
    VListener,
    FMapGoto.GetChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnTimer),
    ATimerNoifier
  );
end;

function TGotoLayer.GetIsVisible: Boolean;
var
  VCurrTime: TDateTime;
  VGotoTime: TDateTime;
  VTimeDelta: Double;
  VGotoPos: IGotoPosStatic;
  VGotoLonLat: TDoublePoint;
  VShowTimeDelta: TDateTime;
begin
  VGotoPos := FMapGoto.LastGotoPos;
  Result := False;
  if VGotoPos <> nil then begin
    VGotoTime := VGotoPos.GotoTime;
    if not IsNan(VGotoTime) then begin
      VGotoLonLat := VGotoPos.LonLat;
      if not PointIsEmpty(VGotoLonLat) then begin
        VCurrTime := Now;
        if (VGotoTime <= VCurrTime) then begin
          VShowTimeDelta := (FConfig.ShowTickCount / 1000) / 60 / 60 / 24;
          VTimeDelta := VCurrTime - VGotoTime;
          if (VTimeDelta < VShowTimeDelta) then begin
            Result := True;
          end;
        end;
      end;
    end;
  end;
end;

procedure TGotoLayer.OnConfigChange;
begin
  ViewUpdateLock;
  try
    SetVisible(GetIsVisible);
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TGotoLayer.OnTimer;
begin
  if Visible then begin
    if not GetIsVisible then begin
      Hide;
    end;
  end;
end;

procedure TGotoLayer.PaintLayer(
  ABuffer: TBitmap32;
  const ALocalConverter: ILocalCoordConverter
);
var
  VConverter: ICoordConverter;
  VGotoPos: IGotoPosStatic;
  VMarker: IMarkerDrawable;
  VGotoLonLat: TDoublePoint;
  VFixedOnView: TDoublePoint;
begin
  inherited;
  VGotoPos := FMapGoto.LastGotoPos;

  if VGotoPos = nil then begin
    Exit;
  end;

  VGotoLonLat := VGotoPos.LonLat;
  if not PointIsEmpty(VGotoLonLat) then begin
    VConverter := ALocalConverter.GetGeoConverter;
    VMarker := FMarkerChangeable.GetStatic;
    VFixedOnView := ALocalConverter.LonLat2LocalPixelFloat(VGotoLonLat);
    VMarker.DrawToBitmap(ABuffer, VFixedOnView);
  end;
end;

procedure TGotoLayer.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
