unit u_MapLayerGPSMarker;

interface

uses
  Windows,
  Types,
  GR32,
  GR32_Transforms,
  GR32_Image,
  i_JclNotify,
  u_MapViewPortState,
  u_MapLayerBasic;


type
  TMapLayerGPSMarker = class(TMapLayerFixedWithBitmap)
  private
    FTransform: TAffineTransformation;
    FGPSDisconntectListener: IJclListener;
    FGPSReceiveListener: IJclListener;
    FMarkerMoved: TCustomBitmap32;
    FMarkerMovedSize: Integer;
    FMarkerMovedColor: TColor32;
    FMarkerStoped: TCustomBitmap32;
    FMarkerStopedColor: TColor32;
    FMarkerStopedSize: Integer;
    FAngle: Double;
    FSpeed: Double;
    FMinMoveSpeed: Double;
    procedure PrepareMarker;
    procedure GPSReceiverReceive(Sender: TObject);
    procedure GPSReceiverDisconnect(Sender: TObject);
  protected
    procedure DoRedraw; override;
  public
    procedure StartThreads; override;
    procedure SendTerminateToThreads; override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  GR32_Polygons,
  i_GPS,
  u_GlobalState,
  u_NotifyEventListener;

{ TMapLayerGPSMarker }

constructor TMapLayerGPSMarker.Create(AParentMap: TImage32;
  AViewPortState: TMapViewPortState);
var
  VSize: TPoint;
begin
  inherited;
  FTransform := TAffineTransformation.Create;

  FMarkerMovedSize := 25;
  FMarkerMovedColor := SetAlpha(Color32(GState.GPSpar.GPS_ArrowColor), 150);

  FMarkerStopedSize := FMarkerMovedSize div 3;
  FMarkerStopedColor := SetAlpha(Color32(GState.GPSpar.GPS_ArrowColor), 200);
  FMinMoveSpeed := 1;

  FMarkerMoved := TCustomBitmap32.Create;
  FMarkerMoved.DrawMode:=dmBlend;
  FMarkerMoved.CombineMode:=cmMerge;

  FMarkerStoped := TCustomBitmap32.Create;
  FMarkerStoped.DrawMode:=dmBlend;
  FMarkerStoped.CombineMode:=cmMerge;

  PrepareMarker;
  VSize := Point(FMarkerMoved.Width, FMarkerMoved.Height);
  FLayer.Bitmap.SetSize(VSize.X, VSize.Y);
  FLayer.Bitmap.Clear(0);
  DoUpdateLayerSize(VSize);
  FGPSDisconntectListener := TNotifyEventListenerSync.Create(Self.GPSReceiverDisconnect);
  FGPSReceiveListener := TNotifyEventListenerSync.Create(Self.GPSReceiverReceive);
end;

destructor TMapLayerGPSMarker.Destroy;
begin
  FreeAndNil(FTransform);
  FreeAndNil(FMarkerMoved);
  FreeAndNil(FMarkerStoped);
  FGPSDisconntectListener := nil;
  FGPSReceiveListener := nil;
  inherited;
end;

procedure TMapLayerGPSMarker.DoRedraw;
var
  VSize: TPoint;
begin
  inherited;
  VSize := LayerSize;
  if FSpeed > FMinMoveSpeed then begin
    FTransform.SrcRect := FloatRect(0, 0, VSize.X, VSize.Y);
    FTransform.Clear;

    FTransform.Translate(-VSize.X / 2, -VSize.Y / 2);
    FTransform.Rotate(0, 0, -FAngle);
    FTransform.Translate(VSize.X / 2, VSize.Y / 2);
    FLayer.Bitmap.Lock;
    try
      FLayer.Bitmap.Clear(0);
      Transform(FLayer.Bitmap, FMarkerMoved, FTransform);
    finally
      FLayer.Bitmap.Unlock;
    end;
  end else begin
    FLayer.Bitmap.Lock;
    try
      FMarkerStoped.DrawTo(FLayer.Bitmap);
    finally
      FLayer.Bitmap.Unlock;
    end;
  end;
end;

procedure TMapLayerGPSMarker.GPSReceiverDisconnect(Sender: TObject);
begin
  Hide;
end;

procedure TMapLayerGPSMarker.GPSReceiverReceive(Sender: TObject);
var
  VGPSPosition: IGPSPosition;
begin
  VGPSPosition := GState.GPSpar.GPSModule.Position;
  if VGPSPosition.IsFix = 0 then begin
    Hide;
  end else begin
    FFixedLonLat := VGPSPosition.Position;
    FAngle := VGPSPosition.Heading;
    FSpeed := VGPSPosition.Speed_KMH;
    Redraw;
    UpdateLayerLocation(GetMapLayerLocationRect);
    Show;
  end;
end;

procedure TMapLayerGPSMarker.PrepareMarker;
var
  VSize: TPoint;
  VPolygon: TPolygon32;
  VPointHalfSize: Double;
  VMarkRect: TRect;
begin
  VSize := Point(FMarkerMovedSize * 2, FMarkerMovedSize * 2);

  FFixedOnBitmap.X := VSize.X / 2;
  FFixedOnBitmap.Y := VSize.Y / 2;

  FMarkerMoved.SetSize(VSize.Y, VSize.Y);
  FMarkerMoved.Clear(0);
  VPolygon := TPolygon32.Create;
  try
    VPolygon.Antialiased := true;
    VPolygon.AntialiasMode := am32times;
    VPolygon.Add(FixedPoint(FFixedOnBitmap.X, FFixedOnBitmap.Y - FMarkerMovedSize));
    VPolygon.Add(FixedPoint(FFixedOnBitmap.X - FMarkerMovedSize / 3, FFixedOnBitmap.Y));
    VPolygon.Add(FixedPoint(FFixedOnBitmap.X + FMarkerMovedSize / 3, FFixedOnBitmap.Y));
    VPolygon.DrawFill(FMarkerMoved, FMarkerMovedColor);
  finally
    FreeAndNil(VPolygon);
  end;

  VPointHalfSize := FMarkerStopedSize / 2;
  FMarkerStoped.SetSize(VSize.Y, VSize.Y);
  FMarkerStoped.Clear(0);
  VMarkRect := Bounds(
    Trunc(FFixedOnBitmap.X - VPointHalfSize),
    Trunc(FFixedOnBitmap.y - VPointHalfSize),
    FMarkerStopedSize,
    FMarkerStopedSize
  );
  FMarkerStoped.FillRectS(VMarkRect, FMarkerStopedColor);
end;

procedure TMapLayerGPSMarker.SendTerminateToThreads;
begin
  inherited;
  GState.GPSpar.GPSModule.DisconnectNotifier.Remove(FGPSDisconntectListener);
  GState.GPSpar.GPSModule.DataReciveNotifier.Remove(FGPSReceiveListener);
end;

procedure TMapLayerGPSMarker.StartThreads;
begin
  inherited;
  GState.GPSpar.GPSModule.DisconnectNotifier.Add(FGPSDisconntectListener);
  GState.GPSpar.GPSModule.DataReciveNotifier.Add(FGPSReceiveListener);
end;

end.
