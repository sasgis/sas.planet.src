unit u_MapLayerNavToMark;

interface

uses
  Types,
  GR32,
  GR32_Image,
  i_Notifier,
  t_GeoTypes,
  i_LocalCoordConverter,
  i_InternalPerformanceCounter,
  i_NavigationToPoint,
  i_MapLayerNavToPointMarkerConfig,
  i_BitmapMarker,
  i_ViewPortState,
  u_MapLayerBasic;

type
  TNavToMarkLayer = class(TMapLayerBasicNoBitmap)
  private
    FConfig: IMapLayerNavToPointMarkerConfig;
    FNavToPoint: INavigationToPoint;
    FArrowMarkerProvider: IBitmapMarkerProviderChangeable;
    FArrowMarkerProviderStatic: IBitmapMarkerProvider;
    FReachedMarkerProvider: IBitmapMarkerProviderChangeable;
    FReachedMarkerProviderStatic: IBitmapMarkerProvider;
    FReachedMarker: IBitmapMarker;

    FMarkPoint: TDoublePoint;
    procedure OnNavToPointChange;
    procedure OnConfigChange;
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
      const ANavToPoint: INavigationToPoint;
      const AArrowMarkerProvider: IBitmapMarkerProviderChangeable;
      const AReachedMarkerProvider: IBitmapMarkerProviderChangeable;
      const AConfig: IMapLayerNavToPointMarkerConfig
    );
  end;

implementation

uses
  SysUtils,
  Math,
  GR32_Resamplers,
  i_CoordConverter,
  u_GeoFun,
  u_ListenerByEvent;

{ TNavToMarkLayer }

constructor TNavToMarkLayer.Create(
  const APerfList: IInternalPerformanceCounterList;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const ANavToPoint: INavigationToPoint;
  const AArrowMarkerProvider: IBitmapMarkerProviderChangeable;
  const AReachedMarkerProvider: IBitmapMarkerProviderChangeable;
  const AConfig: IMapLayerNavToPointMarkerConfig
);
begin
  inherited Create(APerfList, AParentMap, AViewPortState);
  FNavToPoint := ANavToPoint;
  FArrowMarkerProvider := AArrowMarkerProvider;
  FReachedMarkerProvider := AReachedMarkerProvider;
  FConfig := AConfig;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FArrowMarkerProvider.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FReachedMarkerProvider.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnNavToPointChange),
    FNavToPoint.GetChangeNotifier
  );
end;

procedure TNavToMarkLayer.OnConfigChange;
begin
  ViewUpdateLock;
  try
    FArrowMarkerProviderStatic := FArrowMarkerProvider.GetStatic;
    FReachedMarkerProviderStatic := FReachedMarkerProvider.GetStatic;
    FReachedMarker := FReachedMarkerProviderStatic.GetMarker;
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TNavToMarkLayer.OnNavToPointChange;
begin
  ViewUpdateLock;
  try
    SetNeedRedraw;
    FMarkPoint := FNavToPoint.LonLat;
    SetVisible(FNavToPoint.IsActive);
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TNavToMarkLayer.PaintLayer(
  ABuffer: TBitmap32;
  const ALocalConverter: ILocalCoordConverter
);
var
  VMarkMapPos: TDoublePoint;
  VScreenCenterMapPos: TDoublePoint;
  VDelta: TDoublePoint;
  VDeltaNormed: TDoublePoint;
  VZoom: Byte;
  VConverter: ICoordConverter;
  VCrossDist: Double;
  VDistInPixel: Double;
  VAngle: Double;
  VTargetPointFloat: TDoublePoint;
  VTargetPoint: TPoint;
  VFixedOnView: TDoublePoint;
  VMarker: IBitmapMarker;
  VMarkerProvider: IBitmapMarkerProvider;
  VMarkerWithDirectionProvider: IBitmapMarkerWithDirectionProvider;
begin
  VConverter := ALocalConverter.GetGeoConverter;
  VZoom := ALocalConverter.GetZoom;
  VScreenCenterMapPos := ALocalConverter.GetCenterMapPixelFloat;
  VMarkMapPos := VConverter.LonLat2PixelPosFloat(FMarkPoint, VZoom);
  VDelta.X := VMarkMapPos.X - VScreenCenterMapPos.X;
  VDelta.Y := VMarkMapPos.Y - VScreenCenterMapPos.Y;
  VDistInPixel := Sqrt(Sqr(VDelta.X) + Sqr(VDelta.Y));
  VCrossDist := FConfig.CrossDistInPixels;
  if VDistInPixel < VCrossDist then begin
    VFixedOnView := ALocalConverter.LonLat2LocalPixelFloat(FMarkPoint);
    VMarker := FReachedMarker;
  end else begin
    VDeltaNormed.X := VDelta.X / VDistInPixel * VCrossDist;
    VDeltaNormed.Y := VDelta.Y / VDistInPixel * VCrossDist;
    VMarkMapPos.X := VScreenCenterMapPos.X + VDeltaNormed.X;
    VMarkMapPos.Y := VScreenCenterMapPos.Y + VDeltaNormed.Y;
    VFixedOnView := ALocalConverter.MapPixelFloat2LocalPixelFloat(VMarkMapPos);
    VMarkerProvider := FArrowMarkerProviderStatic;
    if Supports(VMarkerProvider, IBitmapMarkerWithDirectionProvider, VMarkerWithDirectionProvider) then begin
      VAngle := ArcSin(VDelta.X / VDistInPixel) / Pi * 180;
      if VDelta.Y > 0 then begin
        VAngle := 180 - VAngle;
      end;
      VMarker := VMarkerWithDirectionProvider.GetMarkerWithRotation(VAngle);
    end else begin
      VMarker := VMarkerProvider.GetMarker;
    end;
  end;
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

procedure TNavToMarkLayer.StartThreads;
begin
  inherited;
  OnNavToPointChange;
  OnConfigChange;
end;

end.
