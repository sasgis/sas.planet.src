unit u_MapLayerGoto;

interface

uses
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_Notifier,
  i_NotifierTime,
  i_NotifierOperation,
  i_LocalCoordConverterChangeable,
  i_InternalPerformanceCounter,
  i_MarkerDrawable,
  i_MapViewGoto,
  i_LocalCoordConverter,
  i_GotoLayerConfig,
  u_WindowLayerWithPos;

type
  TGotoLayer = class(TWindowLayerBasicBase)
  private
    FLocalConverter: ILocalCoordConverterChangeable;
    FConfig: IGotoLayerConfig;
    FMapGoto: IMapViewGoto;
    FMarkerChangeable: IMarkerDrawableChangeable;

    procedure OnTimer;
    procedure OnConfigChange;
    procedure OnPosChange;
    function GetIsVisible: Boolean;
  protected
    procedure PaintLayer(ABuffer: TBitmap32); override;
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const ATimerNoifier: INotifierTime;
      const ALocalConverter: ILocalCoordConverterChangeable;
      const AMarkerChangeable: IMarkerDrawableChangeable;
      const AMapGoto: IMapViewGoto;
      const AConfig: IGotoLayerConfig
    );
  end;

implementation

uses
  Math,
  SysUtils,
  GR32_Layers,
  i_Listener,
  i_CoordConverter,
  u_ListenerTime,
  u_ListenerByEvent,
  u_GeoFunc;

{ TGotoLayer }

constructor TGotoLayer.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const ATimerNoifier: INotifierTime;
  const ALocalConverter: ILocalCoordConverterChangeable;
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
    TCustomLayer.Create(AParentMap.Layers)
  );
  FLocalConverter := ALocalConverter;
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
    TListenerTimeCheck.Create(Self.OnTimer, 1000),
    ATimerNoifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnPosChange),
    FLocalConverter.ChangeNotifier
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
    Visible := GetIsVisible;
    SetNeedFullRepaintLayer;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TGotoLayer.OnPosChange;
begin
  ViewUpdateLock;
  try
    SetNeedFullRepaintLayer;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TGotoLayer.OnTimer;
begin
  if Visible then begin
    ViewUpdateLock;
    try
      Visible := GetIsVisible;
    finally
      ViewUpdateLock;
    end;
  end;
end;

procedure TGotoLayer.PaintLayer(
  ABuffer: TBitmap32
);
var
  VLocalConverter: ILocalCoordConverter;
  VConverter: ICoordConverter;
  VGotoPos: IGotoPosStatic;
  VMarker: IMarkerDrawable;
  VGotoLonLat: TDoublePoint;
  VFixedOnView: TDoublePoint;
begin
  inherited;
  VLocalConverter := FLocalConverter.GetStatic;

  VGotoPos := FMapGoto.LastGotoPos;

  if VGotoPos = nil then begin
    Exit;
  end;

  VGotoLonLat := VGotoPos.LonLat;
  if not PointIsEmpty(VGotoLonLat) then begin
    VConverter := VLocalConverter.GetGeoConverter;
    VMarker := FMarkerChangeable.GetStatic;
    VFixedOnView := VLocalConverter.LonLat2LocalPixelFloat(VGotoLonLat);
    VMarker.DrawToBitmap(ABuffer, VFixedOnView);
  end;
end;

procedure TGotoLayer.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
