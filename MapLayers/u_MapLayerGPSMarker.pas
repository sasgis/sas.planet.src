unit u_MapLayerGPSMarker;

interface

uses
  Windows,
  Types,
  GR32,
  GR32_Image,
  i_JclNotify,
  t_GeoTypes,
  u_MapViewPortState,
  u_MapLayerBasic;


type
  TMapLayerGPSMarker = class(TMapLayerFixedWithBitmap)
  private
    FGPSDisconntectListener: IJclListener;
    FGPSReceiveListener: IJclListener;
    procedure GPSReceiverReceive(Sender: TObject);
    procedure GPSReceiverDisconnect(Sender: TObject);
  protected
    FMarker: TCustomBitmap32;
    FMarkerSize: Integer;
    FMarkerColor: TColor32;
    FMarkerAngle: Double;
    procedure PrepareMarker;
  protected
    procedure DoRedraw; override;
    procedure StartThreads; override;
    procedure SendTerminateToThreads; override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  GR32_Transforms,
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
  FMarkerSize := 25;
  FMarkerColor := SetAlpha(Color32(GState.GPSpar.GPS_ArrowColor), 150);
  FMarker := TCustomBitmap32.Create;
  FMarker.DrawMode:=dmBlend;
  FMarker.CombineMode:=cmMerge;
  PrepareMarker;
  VSize := Point(FMarker.Width, FMarker.Height);
  FLayer.Bitmap.SetSize(VSize.X, VSize.Y);
  FLayer.Bitmap.Clear(0);
  DoUpdateLayerSize(VSize);
  FGPSDisconntectListener := TNotifyEventListenerSync.Create(Self.GPSReceiverDisconnect);
  FGPSReceiveListener := TNotifyEventListenerSync.Create(Self.GPSReceiverReceive);
end;

destructor TMapLayerGPSMarker.Destroy;
begin
  FreeAndNil(FMarker);
  FGPSDisconntectListener := nil;
  FGPSReceiveListener := nil;
  inherited;
end;

procedure TMapLayerGPSMarker.DoRedraw;
begin
  inherited;
  FMarker.DrawTo(FLayer.Bitmap);
end;

procedure TMapLayerGPSMarker.GPSReceiverDisconnect(Sender: TObject);
begin
  Hide;
end;

procedure TMapLayerGPSMarker.GPSReceiverReceive(Sender: TObject);
var
  VGPSPosition: IGPSPosition;
begin
  VGPSPosition := GState.GPSpar.GPSModele.Position;
  if VGPSPosition.IsFix = 0 then begin
    Hide;
  end else begin
    FFixedLonLat := VGPSPosition.Position;
    FMarkerAngle := VGPSPosition.Heading;
    Redraw;
    Show;
  end;
end;

procedure TMapLayerGPSMarker.PrepareMarker;
var
  VSize: TPoint;
  VPolygon: TPolygon32;
begin
  VSize := Point(FMarkerSize * 2, FMarkerSize * 2);

  FFixedOnBitmap.X := VSize.X / 2;
  FFixedOnBitmap.Y := VSize.Y / 2 - FMarkerSize / 2;

  FMarker.SetSize(VSize.Y, VSize.Y);
  FMarker.Clear(0);
  VPolygon := TPolygon32.Create;
  try
    VPolygon.Antialiased := true;
    VPolygon.AntialiasMode := am32times;
    VPolygon.Add(FixedPoint(FFixedOnBitmap.X, FFixedOnBitmap.Y));
    VPolygon.Add(FixedPoint(FFixedOnBitmap.X - FMarkerSize / 5, FFixedOnBitmap.Y + FMarkerSize / 2));
    VPolygon.Add(FixedPoint(FFixedOnBitmap.X + FMarkerSize / 5, FFixedOnBitmap.Y + FMarkerSize / 2));
    VPolygon.DrawFill(FMarker, FMarkerColor);
  finally
    FreeAndNil(VPolygon);
  end;
end;

procedure TMapLayerGPSMarker.SendTerminateToThreads;
begin
  inherited;
  GState.GPSpar.GPSModele.DisconnectNotifier.Remove(FGPSDisconntectListener);
  GState.GPSpar.GPSModele.DataReciveNotifier.Remove(FGPSReceiveListener);
end;

procedure TMapLayerGPSMarker.StartThreads;
begin
  inherited;
  GState.GPSpar.GPSModele.DisconnectNotifier.Add(FGPSDisconntectListener);
  GState.GPSpar.GPSModele.DataReciveNotifier.Add(FGPSReceiveListener);
end;

end.
