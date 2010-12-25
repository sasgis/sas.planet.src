unit u_MapLayerNavToMark;

interface

uses
  Windows,
  Types,
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_ILocalCoordConverter,
  u_MapViewPortState,
  u_MapLayerBasic;

type
  TNavToMarkLayer = class(TMapLayerFixedWithBitmap)
  protected
    FMarkPoint: TDoublePoint;
    FId: integer;
    FCrossDist: Double;
    FArrowBitmap: TCustomBitmap32;
    FCrossBitmap: TCustomBitmap32;
    FCurrDist: Double;
    FCurrAngle: Double;
    procedure PrepareMarker;
  protected
    procedure DoRedraw; override;
    procedure DoPosChange(ANewVisualCoordConverter: ILocalCoordConverter); override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    destructor Destroy; override;
    procedure StartNav(APoint: TDoublePoint; Aid: integer);
    function GetMarkLonLat: TDoublePoint;
    property ID: Integer read FId;
  end;

implementation

uses
  Graphics,
  SysUtils,
  Math,
  GR32_Transforms,
  GR32_Polygons,
  u_GlobalState,
  Ugeofun,
  u_WindowLayerBasic;

{ TNavToMarkLayer }

constructor TNavToMarkLayer.Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
var
  VSize: TPoint;
  Polygon: TPolygon32;
  VMarkSize: integer;
begin
  inherited;
  FArrowBitmap := TCustomBitmap32.Create;
  FArrowBitmap.DrawMode:=dmBlend;
  FArrowBitmap.CombineMode:=cmMerge;
  FCrossBitmap := TCustomBitmap32.Create;
  FCrossBitmap.DrawMode:=dmBlend;
  FCrossBitmap.CombineMode:=cmMerge;
  PrepareMarker;
  FCrossDist := 100;
  VSize := Point(FArrowBitmap.Width, FArrowBitmap.Height);
  FLayer.Bitmap.SetSize(VSize.X, VSize.Y);
  FLayer.Bitmap.Clear(clBlack32);
  DoUpdateLayerSize(VSize);
end;

destructor TNavToMarkLayer.Destroy;
begin
  FreeAndNil(FArrowBitmap);
  FreeAndNil(FCrossBitmap);
  inherited;
end;

procedure TNavToMarkLayer.DoPosChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
var
  VMarkMapPos: TDoublePoint;
  VScreenCenterMapPos: TDoublePoint;
  VDelta: TDoublePoint;
begin
  VScreenCenterMapPos := ANewVisualCoordConverter.GetCenterMapPixelFloat;
  VMarkMapPos := ANewVisualCoordConverter.LonLat2LocalPixelFloat(FMarkPoint);
  VDelta.X := VScreenCenterMapPos.X - VMarkMapPos.X;
  VDelta.Y := VScreenCenterMapPos.Y - VMarkMapPos.Y;
  FCurrDist := Sqrt(Sqr(VDelta.X) + Sqr(VDelta.Y));
  if FCurrDist < FCrossDist then begin
    FFixedLonLat := FMarkPoint;
  end else begin
    VDelta.X := VDelta.X / FCurrDist * FCrossDist;
    VDelta.Y := VDelta.Y / FCurrDist * FCrossDist;
    VMarkMapPos.X := VScreenCenterMapPos.X - VDelta.X;
    VMarkMapPos.Y := VScreenCenterMapPos.Y - VDelta.Y;
    FFixedLonLat := ANewVisualCoordConverter.GetGeoConverter.pixe
  end;
  inherited;
end;

procedure TNavToMarkLayer.DoRedraw;
begin
  inherited;

end;

function TNavToMarkLayer.GetMarkLonLat: TDoublePoint;
begin
  Result := FMarkPoint;
end;

procedure TNavToMarkLayer.PrepareMarker;
var
  VSize: TPoint;
  Polygon: TPolygon32;
  VMarkSize: integer;
  VColor: TColor32;
  VRect: TRect;
begin
  VMarkSize := 20;
  VSize := Point(VMarkSize * 2, VMarkSize * 2);
  FFixedOnBitmap := DoublePoint(VMarkSize, VMarkSize);
  VColor := SetAlpha(Color32(GState.GPSpar.GPS_ArrowColor), 150);
  FArrowBitmap.SetSize(VSize.Y, VSize.Y);
  FArrowBitmap.Clear(0);
  Polygon := TPolygon32.Create;
  try
    Polygon.Antialiased := true;
    Polygon.AntialiasMode := am32times;
    Polygon.Add(FixedPoint(FFixedOnBitmap.X, FFixedOnBitmap.Y));
    Polygon.Add(FixedPoint(FFixedOnBitmap.X - VMarkSize / 5, FFixedOnBitmap.Y + VMarkSize));
    Polygon.Add(FixedPoint(FFixedOnBitmap.X + VMarkSize / 5, FFixedOnBitmap.Y + VMarkSize));
    Polygon.DrawFill(FArrowBitmap, VColor)
  finally
    FreeAndNil(Polygon);
  end;
  FCrossBitmap.SetSize(VSize.Y, VSize.Y);
  FCrossBitmap.Clear(0);
  VRect.Left := Trunc(FFixedOnBitmap.X - VMarkSize /10);
  VRect.Top := Trunc(FFixedOnBitmap.Y - VMarkSize /2);
  VRect.Right := Trunc(FFixedOnBitmap.X + VMarkSize /10);
  VRect.Bottom := VRect.Top + VMarkSize;
  FCrossBitmap.FillRectS(VRect, VColor);
  VRect.Left := Trunc(FFixedOnBitmap.X - VMarkSize /2);
  VRect.Top := Trunc(FFixedOnBitmap.Y - VMarkSize /10);
  VRect.Right := VRect.Left + VMarkSize;
  VRect.Bottom := Trunc(FFixedOnBitmap.Y + VMarkSize /10);
  FCrossBitmap.FillRectS(VRect, VColor);
end;

//function TNavToMarkLayer.GetScreenCenterInBitmapPixels: TPoint;
//var
//  VMarkPoint: TDoublePoint;
//  VSize: TPoint;
//  D: Extended;
//begin
//  VSize := GetBitmapSizeInPixel;
//  Result.X := VSize.X div 2;
//  Result.Y := VSize.Y div 2;
//  VMarkPoint := FGeoConvert.LonLat2PixelPosFloat(FMarkPoint, FZoom);
//  D := Sqrt(Sqr(VMarkPoint.X - FScreenCenterPos.X) + Sqr(VMarkPoint.Y - FScreenCenterPos.Y));
//  if D < GState.GPSpar.GPS_ArrowSize * 2 then begin
//    Result.X := Result.X + Trunc(FScreenCenterPos.X - VMarkPoint.X);
//    Result.Y := Result.Y + Trunc(FScreenCenterPos.Y - VMarkPoint.Y);
//  end else if D > GState.GPSpar.GPS_ArrowSize * 14 then begin
//    Result.X := Result.X + Trunc(GState.GPSpar.GPS_ArrowSize * 7 / D * (FScreenCenterPos.X - VMarkPoint.X));
//    Result.Y := Result.Y + Trunc(GState.GPSpar.GPS_ArrowSize * 7 / D * (FScreenCenterPos.Y - VMarkPoint.Y));
//  end else begin
//    Result.X := Result.X + Trunc((FScreenCenterPos.X - VMarkPoint.X) / 2);
//    Result.Y := Result.Y + Trunc((FScreenCenterPos.Y - VMarkPoint.Y) / 2);
//  end;
//end;

procedure TNavToMarkLayer.StartNav(APoint: TDoublePoint; Aid: integer);
begin
  FMarkPoint := APoint;
  FId := Aid;
  Redraw;
  Show;
end;

end.
