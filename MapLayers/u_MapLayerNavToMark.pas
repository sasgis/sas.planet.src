unit u_MapLayerNavToMark;

interface

uses
  Windows,
  Types,
  GR32,
  GR32_Transforms,
  GR32_Image,
  i_JclNotify,
  t_GeoTypes,
  i_LocalCoordConverter,
  i_NavigationToPoint,
  i_MapLayerNavToPointMarkerConfig,
  i_BitmapMarker,
  i_ViewPortState,
  u_MapLayerBasic;

type
  TNavToMarkLayer = class(TMapLayerBasicNoBitmap)
  private
    FConfig: IMapLayerNavToPointMarkerConfig;
    FTransform: TAffineTransformation;
    FNavToPoint:  INavigationToPoint;
    FArrowMarkerProvider: IBitmapMarkerProviderChangeable;
    FArrowMarkerProviderStatic: IBitmapMarkerProvider;
    FReachedMarkerProvider: IBitmapMarkerProviderChangeable;
    FReachedMarkerProviderStatic: IBitmapMarkerProvider;
    FReachedMarker: IBitmapMarker;

    FMarker: TCustomBitmap32;
    FFixedOnBitmap: TDoublePoint;
    FMarkPoint: TDoublePoint;
    procedure OnNavToPointChange(Sender: TObject);
    procedure OnConfigChange(Sender: TObject);
  protected
    procedure PaintLayer(ABuffer: TBitmap32; ALocalConverter: ILocalCoordConverter); override;
  public
    procedure StartThreads; override;
  public
    constructor Create(
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      ANavToPoint: INavigationToPoint;
      AArrowMarkerProvider: IBitmapMarkerProviderChangeable;
      AReachedMarkerProvider: IBitmapMarkerProviderChangeable;
      AConfig: IMapLayerNavToPointMarkerConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  Math,
  i_CoordConverter,
  u_GeoFun,
  u_NotifyEventListener;

{ TNavToMarkLayer }

constructor TNavToMarkLayer.Create(
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  ANavToPoint: INavigationToPoint;
  AArrowMarkerProvider: IBitmapMarkerProviderChangeable;
  AReachedMarkerProvider: IBitmapMarkerProviderChangeable;
  AConfig: IMapLayerNavToPointMarkerConfig
);
begin
  inherited Create(AParentMap, AViewPortState);
  FNavToPoint := ANavToPoint;
  FArrowMarkerProvider := AArrowMarkerProvider;
  FReachedMarkerProvider := AReachedMarkerProvider;
  FConfig := AConfig;

  FMarker := TCustomBitmap32.Create;
  FMarker.DrawMode := dmBlend;
  FMarker.CombineMode := cmBlend;

  FTransform := TAffineTransformation.Create;
  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FArrowMarkerProvider.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FReachedMarkerProvider.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyEventListener.Create(Self.OnNavToPointChange),
    FNavToPoint.GetChangeNotifier
  );
end;

destructor TNavToMarkLayer.Destroy;
begin
  FreeAndNil(FTransform);
  FreeAndNil(FMarker);
  inherited;
end;

procedure TNavToMarkLayer.OnConfigChange(Sender: TObject);
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
  ViewUpdate;
end;

procedure TNavToMarkLayer.OnNavToPointChange(Sender: TObject);
begin
  ViewUpdateLock;
  try
    SetNeedRedraw;
    FMarkPoint := FNavToPoint.LonLat;
    SetVisible(FNavToPoint.IsActive);
  finally
    ViewUpdateUnlock;
  end;
  ViewUpdate;
end;

procedure TNavToMarkLayer.PaintLayer(ABuffer: TBitmap32;
  ALocalConverter: ILocalCoordConverter);
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
  VVisualConverter: ILocalCoordConverter;
  VTargetPoint: TDoublePoint;
  VFixedOnView: TDoublePoint;
  VMarker: IBitmapMarker;
begin
  VVisualConverter := ViewCoordConverter;
  VConverter := VVisualConverter.GetGeoConverter;
  VZoom := VVisualConverter.GetZoom;
  VScreenCenterMapPos := VVisualConverter.GetCenterMapPixelFloat;
  VMarkMapPos := VConverter.LonLat2PixelPosFloat(FMarkPoint, VZoom);
  VDelta.X := VMarkMapPos.X - VScreenCenterMapPos.X;
  VDelta.Y := VMarkMapPos.Y - VScreenCenterMapPos.Y;
  VDistInPixel := Sqrt(Sqr(VDelta.X) + Sqr(VDelta.Y));
  VCrossDist := FConfig.CrossDistInPixels;
  if VDistInPixel < VCrossDist then begin
    VFixedOnView :=  VVisualConverter.LonLat2LocalPixelFloat(FMarkPoint);
    VMarker := FReachedMarker;
  end else begin
    VDeltaNormed.X := VDelta.X / VDistInPixel * VCrossDist;
    VDeltaNormed.Y := VDelta.Y / VDistInPixel * VCrossDist;
    VMarkMapPos.X := VScreenCenterMapPos.X + VDeltaNormed.X;
    VMarkMapPos.Y := VScreenCenterMapPos.Y + VDeltaNormed.Y;
    VFixedOnView := VVisualConverter.MapPixelFloat2LocalPixelFloat(VMarkMapPos);
    VAngle := ArcSin(VDelta.X/VDistInPixel) / Pi * 180;
    if VDelta.Y > 0 then begin
      VAngle := 180 - VAngle;
    end;
    VMarker := FArrowMarkerProviderStatic.GetMarkerWithRotation(VAngle);
  end;
  FMarker.Lock;
  try
    VTargetPoint.X := VFixedOnView.X - FFixedOnBitmap.X - (VMarker.BitmapSize.X div 2);
    VTargetPoint.Y := VFixedOnView.Y - FFixedOnBitmap.Y - (VMarker.BitmapSize.Y div 2);
    if PixelPointInRect(VTargetPoint, DoubleRect(ALocalConverter.GetLocalRect)) then begin
      ABuffer.Draw(Trunc(VTargetPoint.X), Trunc(VTargetPoint.Y), VMarker.Bitmap);
    end;
  finally
    FMarker.Unlock;
  end;
end;

procedure TNavToMarkLayer.StartThreads;
begin
  inherited;
  OnNavToPointChange(nil);
  OnConfigChange(nil);
end;

end.
