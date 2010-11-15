unit u_MapLayerNavToMark;

interface

uses
  Windows,
  Types,
  GR32,
  GR32_Image,
  t_GeoTypes,
  u_MapViewPortState,
  u_MapLayerBasic;

type
  TNavToMarkLayer = class(TMapLayerBasic)
  protected
    FMarkPoint: TDoublePoint;
    FId: integer;
    FArrowBitmap: TCustomBitmap32;
    procedure DoRedraw; override;
    function GetBitmapSizeInPixel: TPoint; override;
    function GetScreenCenterInBitmapPixels: TPoint; override;
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
  u_WindowLayerBasic;

{ TNavToMarkLayer }

constructor TNavToMarkLayer.Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
var
  VSize: TPoint;
  Polygon: TPolygon32;
  dl: integer;
begin
  inherited;
  FArrowBitmap := TCustomBitmap32.Create;
  FArrowBitmap.DrawMode:=dmBlend;
  FArrowBitmap.CombineMode:=cmMerge;
  VSize := GetBitmapSizeInPixel;
  FArrowBitmap.SetSize(VSize.X, VSize.Y);
  dl := GState.GPSpar.GPS_ArrowSize;
  FArrowBitmap.Clear(clBlack);
  Polygon := TPolygon32.Create;
  try
    Polygon.Antialiased := true;
    Polygon.AntialiasMode := am32times;
    Polygon.Add(FixedPoint(dl, dl div 3));
    Polygon.Add(FixedPoint(dl - dl div 5, dl + dl div 3));
    Polygon.Add(FixedPoint(dl + dl div 5, dl + dl div 3));
    Polygon.DrawFill(FArrowBitmap, SetAlpha(Color32(GState.GPSpar.GPS_ArrowColor), 150))
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
  VMarkPoint: TDoublePoint;
  T: TAffineTransformation;
  Alpha: Double;
  VSize: TPoint;
begin
  inherited;
  FLayer.Bitmap.BeginUpdate;
  try
    FLayer.Bitmap.Clear(clBlack);
    VMarkPoint := FGeoConvert.LonLat2PixelPosFloat(FMarkPoint, FZoom);
    D := Sqrt(Sqr(VMarkPoint.X - FScreenCenterPos.X) + Sqr(VMarkPoint.Y - FScreenCenterPos.Y));
    dl := GState.GPSpar.GPS_ArrowSize;
    if D > dl * 2 then begin
      VSize := GetBitmapSizeInPixel;
      Alpha := ArcSin(Abs(VMarkPoint.X - FScreenCenterPos.X) / D);
      Alpha := Alpha / Pi * 180;
      if VMarkPoint.X - FScreenCenterPos.X < 0 then begin
        if VMarkPoint.Y - FScreenCenterPos.Y < 0 then begin
          Alpha := Alpha;
        end else begin
          Alpha := 180 - Alpha;
        end;
      end else begin
        if VMarkPoint.Y - FScreenCenterPos.Y < 0 then begin
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
      FLayer.Bitmap.VertLine(dl, dl div 2, 3 * dl div 2, SetAlpha(Color32(GState.GPSpar.GPS_ArrowColor), 150));
      FLayer.Bitmap.HorzLine(dl div 2, dl, 3 * dl div 2, SetAlpha(Color32(GState.GPSpar.GPS_ArrowColor), 150));
    end;
  finally
    FLayer.Bitmap.EndUpdate;
  end;

end;

function TNavToMarkLayer.GetBitmapSizeInPixel: TPoint;
begin
  Result := Point(GState.GPSpar.GPS_ArrowSize * 2, GState.GPSpar.GPS_ArrowSize * 2);
end;

function TNavToMarkLayer.GetMarkLonLat: TDoublePoint;
begin
  Result := FMarkPoint;
end;

function TNavToMarkLayer.GetScreenCenterInBitmapPixels: TPoint;
var
  VMarkPoint: TDoublePoint;
  VSize: TPoint;
  D: Extended;
begin
  VSize := GetBitmapSizeInPixel;
  Result.X := VSize.X div 2;
  Result.Y := VSize.Y div 2;
  VMarkPoint := FGeoConvert.LonLat2PixelPosFloat(FMarkPoint, FZoom);
  D := Sqrt(Sqr(VMarkPoint.X - FScreenCenterPos.X) + Sqr(VMarkPoint.Y - FScreenCenterPos.Y));
  if D < GState.GPSpar.GPS_ArrowSize * 2 then begin
    Result.X := Result.X + Trunc(FScreenCenterPos.X - VMarkPoint.X);
    Result.Y := Result.Y + Trunc(FScreenCenterPos.Y - VMarkPoint.Y);
  end else if D > GState.GPSpar.GPS_ArrowSize * 14 then begin
    Result.X := Result.X + Trunc(GState.GPSpar.GPS_ArrowSize * 7 / D * (FScreenCenterPos.X - VMarkPoint.X));
    Result.Y := Result.Y + Trunc(GState.GPSpar.GPS_ArrowSize * 7 / D * (FScreenCenterPos.Y - VMarkPoint.Y));
  end else begin
    Result.X := Result.X + Trunc((FScreenCenterPos.X - VMarkPoint.X) / 2);
    Result.Y := Result.Y + Trunc((FScreenCenterPos.Y - VMarkPoint.Y) / 2);
  end;
end;

procedure TNavToMarkLayer.StartNav(APoint: TDoublePoint; Aid: integer);
begin
  FMarkPoint := APoint;
  FId := Aid;
  Visible := True;
  Resize;
  Redraw;
end;

end.
