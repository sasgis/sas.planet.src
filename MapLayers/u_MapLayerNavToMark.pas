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
  i_ViewPortState,
  u_MapLayerBasic;

type
  TNavToMarkLayer = class(TMapLayerFixedWithBitmap)
  private
    FConfig: IMapLayerNavToPointMarkerConfig;
    FTransform: TAffineTransformation;
    FNavToPoint:  INavigationToPoint;
    FMarkPoint: TDoublePoint;
    FDistInPixel: Double;
    FAngle: Double;
    procedure OnNavToPointChange(Sender: TObject);
    procedure OnConfigChange(Sender: TObject);
  protected
    procedure DoRedraw; override;
    procedure DoPosChange(ANewVisualCoordConverter: ILocalCoordConverter); override;
    function GetLayerSizeForView(ANewVisualCoordConverter: ILocalCoordConverter): TPoint; override;
  public
    procedure StartThreads; override;
  public
    constructor Create(
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      ANavToPoint: INavigationToPoint;
      AConfig: IMapLayerNavToPointMarkerConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  Math,
  i_CoordConverter,
  u_NotifyEventListener;

{ TNavToMarkLayer }

constructor TNavToMarkLayer.Create(
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  ANavToPoint: INavigationToPoint;
  AConfig: IMapLayerNavToPointMarkerConfig
);
begin
  inherited Create(AParentMap, AViewPortState);
  FNavToPoint := ANavToPoint;
  FConfig := AConfig;
  FTransform := TAffineTransformation.Create;
  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyEventListener.Create(Self.OnNavToPointChange),
    FNavToPoint.GetChangeNotifier
  );
  OnNavToPointChange(nil);
  OnConfigChange(nil);
end;

destructor TNavToMarkLayer.Destroy;
begin
  FreeAndNil(FTransform);
  inherited;
end;

procedure TNavToMarkLayer.DoPosChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
var
  VMarkMapPos: TDoublePoint;
  VScreenCenterMapPos: TDoublePoint;
  VDelta: TDoublePoint;
  VDeltaNormed: TDoublePoint;
  VZoom: Byte;
  VConverter: ICoordConverter;
  VCrossDist: Double;
begin
  inherited;
  VConverter := ANewVisualCoordConverter.GetGeoConverter;
  VZoom := ANewVisualCoordConverter.GetZoom;
  VScreenCenterMapPos := ANewVisualCoordConverter.GetCenterMapPixelFloat;
  VMarkMapPos := VConverter.LonLat2PixelPosFloat(FMarkPoint, VZoom);
  VDelta.X := VMarkMapPos.X - VScreenCenterMapPos.X;
  VDelta.Y := VMarkMapPos.Y - VScreenCenterMapPos.Y;
  FDistInPixel := Sqrt(Sqr(VDelta.X) + Sqr(VDelta.Y));
  VCrossDist := FConfig.CrossDistInPixels;
  if FDistInPixel < VCrossDist then begin
    FFixedLonLat := FMarkPoint;
    FAngle := 0;
  end else begin
    VDeltaNormed.X := VDelta.X / FDistInPixel * VCrossDist;
    VDeltaNormed.Y := VDelta.Y / FDistInPixel * VCrossDist;
    VMarkMapPos.X := VScreenCenterMapPos.X + VDeltaNormed.X;
    VMarkMapPos.Y := VScreenCenterMapPos.Y + VDeltaNormed.Y;
    FFixedLonLat := VConverter.PixelPosFloat2LonLat(VMarkMapPos, VZoom);
    FAngle := ArcSin(VDelta.X/FDistInPixel) / Pi * 180;
    if VDelta.Y > 0 then begin
      FAngle := 180 - FAngle;
    end;
  end;
  SetNeedRedraw;
end;

procedure TNavToMarkLayer.DoRedraw;
var
  VSize: TPoint;
  VMarker: TCustomBitmap32;
begin
  inherited;
  VSize := Point(FLayer.Bitmap.Width, FLayer.Bitmap.Height);
  if FDistInPixel > FConfig.CrossDistInPixels then begin
    FTransform.SrcRect := FloatRect(0, 0, VSize.X, VSize.Y);
    FTransform.Clear;
    FConfig.LockRead;
    try
      VMarker := FConfig.GetMarkerArrow;
      FTransform.Rotate(VSize.X / 2, VSize.Y / 2, -FAngle);
      FLayer.Bitmap.Lock;
      try
        FLayer.Bitmap.Clear(0);
        Transform(FLayer.Bitmap, VMarker, FTransform);
      finally
        FLayer.Bitmap.Unlock;
      end;
    finally
      FConfig.UnlockRead;
    end;
  end else begin
    FLayer.Bitmap.Lock;
    try
      FConfig.LockRead;
      try
        VMarker := FConfig.GetMarkerCross;
        FLayer.Bitmap.Clear(0);
        VMarker.DrawTo(
          FLayer.Bitmap,
          trunc(VSize.X / 2 - VMarker.Width / 2),
          trunc(VSize.Y / 2 - VMarker.Height / 2)
        );
      finally
        FConfig.UnlockRead;
      end;
    finally
      FLayer.Bitmap.Unlock;
    end;
  end;
end;

function TNavToMarkLayer.GetLayerSizeForView(
  ANewVisualCoordConverter: ILocalCoordConverter): TPoint;
var
  VMarker: TCustomBitmap32;
begin
  FConfig.LockRead;
  try
    VMarker := FConfig.GetMarkerArrow;
    Result := Point(VMarker.Width, VMarker.Height);
    VMarker := FConfig.GetMarkerCross;
    if Result.X < VMarker.Width then begin
      Result.X := VMarker.Width;
    end;
    if Result.Y < VMarker.Height then begin
      Result.Y := VMarker.Height;
    end;
  finally
    FConfig.UnlockRead;
  end;
  FFixedOnBitmap.X := Result.X / 2;
  FFixedOnBitmap.Y := Result.Y / 2;
end;

procedure TNavToMarkLayer.OnConfigChange(Sender: TObject);
begin
  ViewUpdateLock;
  try
    SetNeedUpdateLayerSize;
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

procedure TNavToMarkLayer.StartThreads;
begin
  inherited;
  OnConfigChange(nil);
end;

end.
