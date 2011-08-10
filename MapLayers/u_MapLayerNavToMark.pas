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
    FArrowMarkerProvider: IBitmapMarkerProvider;
    FReachedMarkerProvider: IBitmapMarkerProvider;
    FArrowMarker: IBitmapMarker;
    FReachedMarker: IBitmapMarker;

    FMarker: TCustomBitmap32;
    FPreparedMarker: IBitmapMarker;
    FPreparedAngle: Double;
    FFixedOnBitmap: TDoublePoint;
    FMarkPoint: TDoublePoint;
    procedure OnNavToPointChange(Sender: TObject);
    procedure OnConfigChange(Sender: TObject);
    procedure PrepareMarker(AMarker: IBitmapMarker; AAngle: Double);
  protected
    procedure PaintLayer(ABuffer: TBitmap32; ALocalConverter: ILocalCoordConverter); override;
  public
    procedure StartThreads; override;
  public
    constructor Create(
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      ANavToPoint: INavigationToPoint;
      AArrowMarkerProvider: IBitmapMarkerProvider;
      AReachedMarkerProvider: IBitmapMarkerProvider;
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
  AArrowMarkerProvider: IBitmapMarkerProvider;
  AReachedMarkerProvider: IBitmapMarkerProvider;
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
    FArrowMarker := FArrowMarkerProvider.GetMarker;
    FReachedMarker := FReachedMarkerProvider.GetMarker;
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
    VAngle := 0;
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
    VMarker := FArrowMarker;
  end;
  FMarker.Lock;
  try
    PrepareMarker(VMarker, VAngle);
    VTargetPoint.X := VFixedOnView.X - FFixedOnBitmap.X;
    VTargetPoint.Y := VFixedOnView.Y - FFixedOnBitmap.Y;
    if PixelPointInRect(VTargetPoint, DoubleRect(ALocalConverter.GetLocalRect)) then begin
      ABuffer.Draw(Trunc(VTargetPoint.X), Trunc(VTargetPoint.Y), FMarker);
    end;
  finally
    FMarker.Unlock;
  end;
end;

procedure TNavToMarkLayer.PrepareMarker(AMarker: IBitmapMarker; AAngle: Double);
var
  VSizeSource: TPoint;
  VSizeSourceFloat: TFloatPoint;
  VSizeTarget: TPoint;
  VDiag: Integer;
  VFixedOnBitmap: TFloatPoint;
begin
  if AMarker <> nil then begin
    if (AMarker <> FPreparedMarker) or (Abs(AAngle - FPreparedAngle) > 1) then begin
      if AMarker.UseDirection then begin
        VSizeSource := AMarker.BitmapSize;
        VSizeSourceFloat := FloatPoint(VSizeSource.X, VSizeSource.Y);
        VDiag := Trunc(Hypot(VSizeSourceFloat.X, VSizeSourceFloat.Y));
        VSizeTarget.X := VDiag;
        VSizeTarget.Y := VDiag;
        FMarker.Lock;
        try
          FTransform.SrcRect := FloatRect(0, 0, VSizeSource.X, VSizeSource.Y);
          FTransform.Clear;
          FTransform.Translate(-VSizeSource.X / 2, -VSizeSource.Y / 2);
          FTransform.Rotate(0, 0, AMarker.DefaultDirection - AAngle);
          FTransform.Translate(VSizeTarget.X / 2, VSizeTarget.Y / 2);

          FMarker.SetSize(VSizeTarget.X, VSizeTarget.Y);
          FMarker.Clear(0);
          Transform(FMarker, AMarker.Bitmap, FTransform);
          VFixedOnBitmap := FTransform.Transform(FloatPoint(AMarker.AnchorPoint.X, AMarker.AnchorPoint.Y));
          FFixedOnBitmap := DoublePoint(VFixedOnBitmap.X, VFixedOnBitmap.Y);
        finally
          FMarker.Unlock;
        end;
      end else begin
        VSizeTarget := AMarker.BitmapSize;
        FMarker.Lock;
        try
          FMarker.SetSize(VSizeTarget.X, VSizeTarget.Y);
          FFixedOnBitmap := AMarker.AnchorPoint;
          AMarker.Bitmap.DrawTo(FMarker);
        finally
          FMarker.Unlock;
        end;
      end;
      FPreparedMarker := AMarker;
      FPreparedAngle := AAngle;
    end;
  end else begin
    FMarker.Lock;
    try
      FMarker.SetSize(0, 0);
    finally
      FMarker.Unlock;
    end;
  end;
end;

procedure TNavToMarkLayer.StartThreads;
begin
  inherited;
  OnNavToPointChange(nil);
  OnConfigChange(nil);
end;

end.
