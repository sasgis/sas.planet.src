unit u_MapLayerNavToMark;

interface

uses
  Windows,
  GR32,
  GR32_Image,
  t_GeoTypes,
  u_MapLayerBasic;

type
  TNavToMarkLayer =  class(TMapLayerBasic)
  protected
    FMarkPoint: TExtendedPoint;
    FId: integer;
    procedure DoRedraw; override;
    function GetBitmapSizeInPixel: TPoint; override;
    function GetScreenCenterInBitmapPixels: TPoint; override;
  public
    constructor Create(AParentMap: TImage32; ACenter: TPoint);
    procedure StartNav(APoint: TExtendedPoint; Aid: integer);
    function GetDistToMark: Double;
    property ID: Integer read FId;
  end;

implementation

uses
  Graphics,
  SysUtils,
  GR32_Polygons,
  u_GlobalState,
  u_WindowLayerBasic;

{ TNavToMarkLayer }

constructor TNavToMarkLayer.Create(AParentMap: TImage32; ACenter: TPoint);
begin
  inherited Create(AParentMap, ACenter);
end;

procedure TNavToMarkLayer.DoRedraw;
var
  D: Double;
  dl: integer;
  VMarkPoint: TPoint;
  Polygon: TPolygon32;
begin
  inherited;
  FLayer.Bitmap.Clear(clBlack);
  VMarkPoint := FGeoConvert.LonLat2PixelPos(FMarkPoint, FZoom);
  D := Sqrt(Sqr(VMarkPoint.X-FScreenCenterPos.X)+Sqr(VMarkPoint.Y-FScreenCenterPos.Y));
  dl:=GState.GPS_ArrowSize;
  if D > dl * 2 then begin
    Polygon := TPolygon32.Create;
    try
      Polygon.Antialiased := true;
      Polygon.AntialiasMode:=am4times;
      Polygon.Add(FixedPoint(dl div 2, dl));
      Polygon.Add(FixedPoint(dl, dl div 2));
      Polygon.Add(FixedPoint(dl * 3 div 2, dl));
      Polygon.Add(FixedPoint(dl, dl * 3 div 2));
      Polygon.DrawFill(FLayer.Bitmap, SetAlpha(Color32(GState.GPS_ArrowColor), 150))
    finally
      FreeAndNil(Polygon);
    end;
  end else begin
     FLayer.Bitmap.VertLine(dl, dl div 2, 3 * dl div 2,SetAlpha(Color32(GState.GPS_ArrowColor), 150));
     FLayer.Bitmap.HorzLine(dl div 2, dl, 3 * dl div 2,SetAlpha(Color32(GState.GPS_ArrowColor), 150));
  end;
end;

function TNavToMarkLayer.GetBitmapSizeInPixel: TPoint;
begin
  Result := Point(GState.GPS_ArrowSize * 2, GState.GPS_ArrowSize * 2);
end;

function TNavToMarkLayer.GetDistToMark: Double;
var
  VPoint: TExtendedPoint;
begin
  if Visible then begin
    VPoint := FGeoConvert.PixelPos2LonLat(FScreenCenterPos, FZoom);
    Result := FGeoConvert.CalcDist(VPoint, FMarkPoint);
  end else begin
    Result := 0;
  end;
end;

function TNavToMarkLayer.GetScreenCenterInBitmapPixels: TPoint;
var
  VMarkPoint: TPoint;
  VSize: TPoint;
  D: Double;
begin
  VSize := GetBitmapSizeInPixel;
  Result.X := VSize.X div 2;
  Result.Y := VSize.Y div 2;
  VMarkPoint := FGeoConvert.LonLat2PixelPos(FMarkPoint, FZoom);
  D := Sqrt(Sqr(VMarkPoint.X-FScreenCenterPos.X)+Sqr(VMarkPoint.Y-FScreenCenterPos.Y));
  if D < GState.GPS_ArrowSize * 2 then begin
    Result.X := Result.X + (FScreenCenterPos.X - VMarkPoint.X);
    Result.Y := Result.Y + (FScreenCenterPos.Y - VMarkPoint.Y);
  end else if D > GState.GPS_ArrowSize * 14 then begin
    Result.X := Result.X + Trunc(GState.GPS_ArrowSize * 7 / D * (FScreenCenterPos.X - VMarkPoint.X));
    Result.Y := Result.Y + Trunc(GState.GPS_ArrowSize * 7 / D * (FScreenCenterPos.Y - VMarkPoint.Y));
  end else begin
    Result.X := Result.X + (FScreenCenterPos.X - VMarkPoint.X) div 2;
    Result.Y := Result.Y + (FScreenCenterPos.Y - VMarkPoint.Y) div 2;
  end;
end;

procedure TNavToMarkLayer.StartNav(APoint: TExtendedPoint; Aid: integer);
begin
  FMarkPoint := APoint;
  FId := Aid;
  Visible := True;
  Resize;
  Redraw;
end;

end.
