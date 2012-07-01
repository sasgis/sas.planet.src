unit u_MapLayerGoto;

interface

uses
  GR32,
  GR32_Image,
  t_GeoTypes,
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

    FGotoPos: IGotoPosStatic;
    FShowTimeDelta: TDateTime;

    procedure OnConfigChange;
  protected
    function GetVisibleForNewPos(
      const ANewVisualCoordConverter: ILocalCoordConverter
    ): Boolean; override;
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
  i_JclNotify,
  i_CoordConverter,
  u_NotifyEventListener,
  u_GeoFun;

{ TGotoLayer }

constructor TGotoLayer.Create(
  const APerfList: IInternalPerformanceCounterList;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const AMarkerProvider: IBitmapMarkerProviderChangeable;
  const AMapGoto: IMapViewGoto;
  const AConfig: IGotoLayerConfig
);
var
  VListener: IJclListener;
begin
  inherited Create(APerfList, AParentMap, AViewPortState);
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
end;

function TGotoLayer.GetVisibleForNewPos(
  const ANewVisualCoordConverter: ILocalCoordConverter
): Boolean;
var
  VCurrTime: TDateTime;
  VGotoTime: TDateTime;
  VTimeDelta: Double;
  VGotoPos: IGotoPosStatic;
  VGotoLonLat: TDoublePoint;
  VFixedOnView: TDoublePoint;
begin
  VGotoPos := FGotoPos;
  Result := False;
  if VGotoPos <> nil then begin
    VGotoTime := VGotoPos.GotoTime;
    if not IsNan(VGotoTime) then begin
      VGotoLonLat := VGotoPos.LonLat;
      if not PointIsEmpty(VGotoLonLat) then begin
        VCurrTime := Now;
        if (VGotoTime <= VCurrTime) then begin
          VTimeDelta := VCurrTime - VGotoTime;
          if (VTimeDelta < FShowTimeDelta) then begin
            VFixedOnView := ANewVisualCoordConverter.LonLat2LocalPixelFloat(VGotoLonLat);
            if PixelPointInRect(VFixedOnView, DoubleRect(ANewVisualCoordConverter.GetLocalRect)) then begin
              Result := True;
            end;
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
  FGotoPos := FMapGoto.LastGotoPos;
  FMarkerProviderStatic := FMarkerProvider.GetStatic;
  VMarker := FMarkerProviderStatic.GetMarker;
  FMarker := VMarker;
  ViewUpdateLock;
  try
    FShowTimeDelta := (FConfig.ShowTickCount / 1000) / 60 / 60 / 24;
    SetVisible(GetVisibleForNewPos(LayerCoordConverter));
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
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
  VGotoPos := FGotoPos;
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
