unit u_MapLayerGoto;

interface

uses
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_Notifier,
  i_NotifierOperation,
  i_InternalPerformanceCounter,
  i_BitmapMarker,
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


    FMarkerProvider: IBitmapMarkerProviderChangeable;
    FMarkerProviderStatic: IBitmapMarkerProvider;
    FMarker: IBitmapMarker;

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
      const AMarkerProvider: IBitmapMarkerProviderChangeable;
      const AMapGoto: IMapViewGoto;
      const AConfig: IGotoLayerConfig
    );
  end;

implementation

uses
  Types,
  Math,
  SysUtils,
  GR32_Resamplers,
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
  const AMarkerProvider: IBitmapMarkerProviderChangeable;
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
  FMarkerProvider := AMarkerProvider;
  FMapGoto := AMapGoto;

  VListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  LinksList.Add(
    VListener,
    FConfig.GetChangeNotifier
  );

  LinksList.Add(
    VListener,
    FMarkerProvider.GetChangeNotifier
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
var
  VMarker: IBitmapMarker;
begin
  FMarkerProviderStatic := FMarkerProvider.GetStatic;
  VMarker := FMarkerProviderStatic.GetMarker;
  FMarker := VMarker;
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
  VMarker: IBitmapMarker;
  VGotoLonLat: TDoublePoint;
  VFixedOnView: TDoublePoint;
  VTargetPoint: TPoint;
  VTargetPointFloat: TDoublePoint;
begin
  inherited;
  VGotoPos := FMapGoto.LastGotoPos;

  if VGotoPos = nil then begin
    Exit;
  end;

  VGotoLonLat := VGotoPos.LonLat;
  if not PointIsEmpty(VGotoLonLat) then begin
    VConverter := ALocalConverter.GetGeoConverter;
    VMarker := FMarker;
    VFixedOnView := ALocalConverter.LonLat2LocalPixelFloat(VGotoLonLat);
    VTargetPointFloat :=
      DoublePoint(
        VFixedOnView.X - VMarker.AnchorPoint.X,
        VFixedOnView.Y - VMarker.AnchorPoint.Y
      );
    VTargetPoint := PointFromDoublePoint(VTargetPointFloat, prToTopLeft);
    if PtInRect(ALocalConverter.GetLocalRect, VTargetPoint) then begin
      BlockTransfer(
        ABuffer,
        VTargetPoint.X, VTargetPoint.Y,
        ABuffer.ClipRect,
        VMarker.Bitmap,
        VMarker.Bitmap.BoundsRect,
        dmBlend,
        cmBlend
      );
    end;
  end;
end;

procedure TGotoLayer.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
