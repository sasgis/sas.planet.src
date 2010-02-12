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
    FArrowBitmap: TBitmap32;
    procedure DoRedraw; override;
    function GetBitmapSizeInPixel: TPoint; override;
    function GetScreenCenterInBitmapPixels: TPoint; override;
  public
    constructor Create(AParentMap: TImage32; ACenter: TPoint);
    destructor Destroy; override;
    procedure StartNav(APoint: TExtendedPoint; Aid: integer);
    function GetDistToMark: Double;
    property ID: Integer read FId;
  end;

implementation

uses
  Graphics,
  SysUtils,
  GR32_Transforms,
  GR32_Polygons,
  u_GlobalState,
  u_WindowLayerBasic, Math;

{ TNavToMarkLayer }

constructor TNavToMarkLayer.Create(AParentMap: TImage32; ACenter: TPoint);
var
  VSize: TPoint;
  Polygon: TPolygon32;
  dl: integer;
begin
  inherited Create(AParentMap, ACenter);
  FArrowBitmap := TBitmap32.Create;
  VSize := GetBitmapSizeInPixel;
  FArrowBitmap.SetSize(VSize.X, VSize.Y);
  dl:=GState.GPS_ArrowSize;
  FArrowBitmap.Clear(clBlack);
  Polygon := TPolygon32.Create;
  try
    Polygon.Antialiased := true;
    Polygon.AntialiasMode:=am4times;
    Polygon.Add(FixedPoint(dl, dl div 3));
    Polygon.Add(FixedPoint(dl - dl div 5, dl + dl div 3));
    Polygon.Add(FixedPoint(dl + dl div 5, dl + dl div 3));
    Polygon.DrawFill(FArrowBitmap, SetAlpha(Color32(GState.GPS_ArrowColor), 150))
  finally
    FreeAndNil(Polygon);
  end;
end;

destructor TNavToMarkLayer.Destroy;
begin
  FreeAndNil(FArrowBitmap);
  inherited;
end;

procedure TNavToMarkLayer.DoRedraw;
var
  D: Double;
  dl: integer;
  VMarkPoint: TPoint;
  T: TAffineTransformation;
  Alpha: Double;
  VSize: TPoint;
begin
  inherited;
  FLayer.Bitmap.BeginUpdate;
  try
    FLayer.Bitmap.Clear(clBlack);
    VMarkPoint := FGeoConvert.LonLat2PixelPos(FMarkPoint, FZoom);
    D := Sqrt(Sqr(VMarkPoint.X-FScreenCenterPos.X)+Sqr(VMarkPoint.Y-FScreenCenterPos.Y));
    dl:=GState.GPS_ArrowSize;
    if D > dl * 2 then begin
      VSize := GetBitmapSizeInPixel;
      Alpha := ArcSin(Abs(VMarkPoint.X-FScreenCenterPos.X)/D);
      Alpha := Alpha / Pi * 180;
      if VMarkPoint.X-FScreenCenterPos.X < 0 then begin
        if VMarkPoint.Y-FScreenCenterPos.Y < 0 then begin
          Alpha := Alpha;
        end else begin
          Alpha := 180 - Alpha;
        end;
      end else begin
        if VMarkPoint.Y-FScreenCenterPos.Y < 0 then begin
          Alpha := 360 - Alpha;
        end else begin
          Alpha := Alpha + 180;
        end;
      end;
      T := TAffineTransformation.Create;
      try
        T.SrcRect := FloatRect(0, 0, VSize.X, VSize.Y);
        T.Clear;

        T.Translate(-VSize.X / 2, -VSize.Y / 2);
        T.Rotate(0, 0, Alpha);
        T.Translate(VSize.X / 2, VSize.Y / 2);
        Transform(FLayer.Bitmap, FArrowBitmap, T);
      finally
        FreeAndNil(T);
      end;
    end else begin
       FLayer.Bitmap.VertLine(dl, dl div 2, 3 * dl div 2,SetAlpha(Color32(GState.GPS_ArrowColor), 150));
       FLayer.Bitmap.HorzLine(dl div 2, dl, 3 * dl div 2,SetAlpha(Color32(GState.GPS_ArrowColor), 150));
    end;
  finally
    FLayer.Bitmap.EndUpdate;
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
