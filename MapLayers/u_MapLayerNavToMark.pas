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
  TNavToMarkLayer = class(TMapLayerFixedWithBitmap)
  protected
    FMarkSize: Integer;
    FMarkPoint: TDoublePoint;
    FId: integer;
    FArrowBitmap: TCustomBitmap32;
    function GetBitmapSizeInPixel: TPoint; override;
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
  FMarkSize: integer;
begin
  inherited;
  FArrowBitmap := TCustomBitmap32.Create;
  FArrowBitmap.DrawMode:=dmBlend;
  FArrowBitmap.CombineMode:=cmMerge;
  FMarkSize := 20;
  FArrowBitmap.SetSize(FMarkSize * 2, FMarkSize * 2);
  FArrowBitmap.Clear(clBlack);
  Polygon := TPolygon32.Create;
  try
    Polygon.Antialiased := true;
    Polygon.AntialiasMode := am32times;
    Polygon.Add(FixedPoint(FMarkSize, FMarkSize div 3));
    Polygon.Add(FixedPoint(FMarkSize - FMarkSize div 5, FMarkSize + FMarkSize div 3));
    Polygon.Add(FixedPoint(FMarkSize + FMarkSize div 5, FMarkSize + FMarkSize div 3));
    Polygon.DrawFill(FArrowBitmap, SetAlpha(Color32(GState.GPSpar.GPS_ArrowColor), 150))
  finally
    FreeAndNil(Polygon);
  end;
  FLayer.Bitmap.SetSize(FMarkSize * 2, FMarkSize * 2);
  FLayer.Bitmap.Clear(clBlack32);
end;

destructor TNavToMarkLayer.Destroy;
begin
  FreeAndNil(FArrowBitmap);
  inherited;
end;

function TNavToMarkLayer.GetBitmapSizeInPixel: TPoint;
begin
  Result := Point(FMarkSize * 2, FMarkSize * 2);
end;

function TNavToMarkLayer.GetMarkLonLat: TDoublePoint;
begin
  Result := FMarkPoint;
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
  Visible := True;
end;

end.
