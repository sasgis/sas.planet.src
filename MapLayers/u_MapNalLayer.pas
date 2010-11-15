unit u_MapNalLayer;

interface

uses
  Windows,
  GR32,
  GR32_Image,
  GR32_Polygons,
  t_GeoTypes,
  u_MapViewPortState,
  u_MapLayerBasic;

type
  TMapNalDrawType = (mndtNothing, mndtSelectRect, mndtSelectPoly, mndtCalcLen, mndtNewPath, mndtNewPoly);

  TMapNalLayer = class(TMapLayerBasic)
  private
    FDrawType: TMapNalDrawType;
    FPath: TDoublePointArray;
    FSelectedLonLat: TDoubleRect;
    FPolyActivePointIndex: integer;
    FLenShow: Boolean;

    FPolyPointColor: TColor32;
    FPolyActivePointColor: TColor32;
    FPolyFirstPointColor: TColor32;
    FPolyLineColor: TColor32;
    FPolyFillColor: TColor32;
    FPolyLineWidth: Integer;
    procedure DoDrawSelectionRect;
    procedure DoDrawSelectionPoly;
    procedure DoDrawCalcLine;
    procedure DoDrawNewPath(AIsPoly: Boolean);
    procedure PreparePolygon(pathll: TExtendedPointArray; polygon: TPolygon32);
  protected
    procedure DoRedraw; override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    destructor Destroy; override;
    procedure DrawNothing;
    procedure DrawSelectionRect(ASelectedLonLat: TDoubleRect);
    procedure DrawReg(ASelectedLonLatPoly: TDoublePointArray);
    procedure DrawLineCalc(APathLonLat: TDoublePointArray; ALenShow: Boolean; AActiveIndex: Integer);
    procedure DrawNewPath(APathLonLat: TDoublePointArray; AIsPoly: boolean; AActiveIndex: Integer);
  end;

implementation

uses
  Types,
  Graphics,
  SysUtils,
  Ugeofun,
  u_GeoToStr,
  UResStrings,
  u_GlobalState,
  u_WindowLayerBasic;

const
  CRectSize = 1 shl 14;

{ TMapNalLayer }

constructor TMapNalLayer.Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
begin
  inherited;
  FPolyPointColor := SetAlpha(clYellow32, 150);
  FPolyActivePointColor := SetAlpha(ClRed32, 255);
  FPolyFirstPointColor := SetAlpha(ClGreen32, 255);
  FPolyLineColor := SetAlpha(ClRed32, 150);
  FPolyFillColor := SetAlpha(ClWhite32, 50);
  FPolyLineWidth := 3;
end;

destructor TMapNalLayer.Destroy;
begin
  FPath := nil;
  inherited;
end;

procedure TMapNalLayer.DoDrawCalcLine;
var
  i, j, textW: integer;
  k1: TDoublePoint;
  k2: TDoublePoint;
  len: real;
  text: string;
  polygon: TPolygon32;
  VBitmapSize: TPoint;
  VPointsOnBitmap: TDoublePointArray;
  VPointsCount: Integer;
begin
  try
    polygon := TPolygon32.Create;
    polygon.Antialiased := true;
    polygon.AntialiasMode := am4times;
    polygon.Closed := false;
    PreparePolygon(FPath,polygon);
    with Polygon.Outline do try
       with Grow(Fixed(FPolyLineWidth / 2), 0.5) do try
         FillMode := pfWinding;
         DrawFill(FLayer.Bitmap, SetAlpha(ClRed32, 150));
       finally
         free;
       end;
    finally
      free;
    end;
  finally
    polygon.Free;
  end;

  VBitmapSize := GetBitmapSizeInPixel;
  FLayer.Bitmap.Font.Name := 'Tahoma';
  VPointsCount := Length(FPath);
  if VPointsCount > 0 then begin
    SetLength(VPointsOnBitmap, VPointsCount);
    try
      for i := 0 to VPointsCount - 1 do begin
        VPointsOnBitmap[i] := MapPixel2BitmapPixel(FGeoConvert.LonLat2PixelPosFloat(FPath[i], FZoom));
      end;
      for i := 0 to VPointsCount - 2 do begin
        k2 := VPointsOnBitmap[i + 1];
        if not ((k2.x > 0) and (k2.y > 0)) and ((k2.x < VBitmapSize.X) and (k2.y < VBitmapSize.Y)) then begin
          continue;
        end;
        FLayer.Bitmap.FrameRectS(
          Trunc(k2.x - 3),
          Trunc(k2.y - 3),
          Trunc(k2.X + 3),
          Trunc(k2.Y + 3),
          SetAlpha(ClRed32, 150)
        );
        FLayer.Bitmap.FillRectS(
          Trunc(k2.x - 2),
          Trunc(k2.y - 2),
          Trunc(k2.X + 2),
          Trunc(k2.y + 2),
          SetAlpha(ClWhite32, 150)
        );
        if i = VPointsCount - 2 then begin
          len := 0;
          for j := 0 to i do begin
            len := len + FGeoConvert.CalcDist(FPath[j], FPath[j + 1]);
          end;
          text := SAS_STR_Whole + ': ' + DistToStrWithUnits(len, GState.num_format);
          FLayer.Bitmap.Font.Size := 9;
          textW := FLayer.Bitmap.TextWidth(text) + 11;
          FLayer.Bitmap.FillRectS(
            Trunc(k2.x + 12),
            Trunc(k2.y),
            Trunc(k2.X + textW),
            Trunc(k2.y + 15),
            SetAlpha(ClWhite32, 110)
          );
          FLayer.Bitmap.RenderText(
            Trunc(k2.X + 15),
            Trunc(k2.y),
            text,
            3,
            clBlack32
          );
        end else begin
          if FLenShow then begin
            text := DistToStrWithUnits(FGeoConvert.CalcDist(FPath[i], FPath[i + 1]), GState.num_format);
            FLayer.Bitmap.Font.Size := 7;
            textW := FLayer.Bitmap.TextWidth(text) + 11;
            FLayer.Bitmap.FillRectS(
              Trunc(k2.x + 5),
              Trunc(k2.y + 5),
              Trunc(k2.X + textW),
              Trunc(k2.y + 16),
              SetAlpha(ClWhite32, 110)
            );
            FLayer.Bitmap.RenderText(
              Trunc(k2.X + 8),
              Trunc(k2.y + 5),
              text,
              0,
              clBlack32
            );
          end;
        end;
      end;
      k1 := VPointsOnBitmap[0];
      if ((k1.x > 0) and (k1.y > 0)) and ((k1.x < VBitmapSize.X) and (k1.y < VBitmapSize.Y)) then begin
        k1 := DoublePoint(k1.x - 3, k1.y - 3);
        FLayer.Bitmap.FillRectS(bounds(Round(k1.x), Round(k1.y), 6, 6), SetAlpha(ClGreen32, 255));
      end;
      k1 := VPointsOnBitmap[FPolyActivePointIndex];
      if ((k1.x > 0) and (k1.y > 0)) and ((k1.x < VBitmapSize.X) and (k1.y < VBitmapSize.Y)) then begin
        k1 := DoublePoint(k1.x - 3, k1.y - 3);
        FLayer.Bitmap.FillRectS(bounds(Round(k1.x), Round(k1.y), 6, 6), SetAlpha(ClRed32, 255));
      end;
    finally
      VPointsOnBitmap := nil;
    end;
  end;
end;

procedure TMapNalLayer.DoDrawNewPath(AIsPoly: Boolean);
var
  i: integer;
  k1: TExtendedPoint;
  polygon: TPolygon32;
  VBitmapSize: TPoint;
  VPointsOnBitmap: TDoublePointArray;
  VPointsCount: Integer;
begin
  try
    polygon := TPolygon32.Create;
    polygon.Antialiased := true;
    polygon.AntialiasMode := am4times;
    polygon.Closed := AIsPoly;
    PreparePolygon(FPath,polygon);
    if AIsPoly then begin
      Polygon.DrawFill(FLayer.Bitmap, FPolyFillColor);
    end;
    with Polygon.Outline do try
       with Grow(Fixed(FPolyLineWidth / 2), 0.5) do try
         FillMode := pfWinding;
         DrawFill(FLayer.Bitmap, FPolyLineColor);
       finally
         free;
       end;
    finally
      free;
    end;
  finally
    polygon.Free;
  end;

  VBitmapSize := GetBitmapSizeInPixel;
  VPointsCount := Length(FPath);
  if VPointsCount > 0 then begin
    SetLength(VPointsOnBitmap, VPointsCount);
    try
      for i := 0 to VPointsCount - 1 do begin
        VPointsOnBitmap[i] := MapPixel2BitmapPixel(FGeoConvert.LonLat2PixelPosFloat(FPath[i], FZoom));
      end;
      for i := 1 to VPointsCount - 1 do begin
        k1 := VPointsOnBitmap[i];
        if ((k1.x > 0) and (k1.y > 0)) and ((k1.x < VBitmapSize.X) and (k1.y < VBitmapSize.Y)) then begin
          k1 := DoublePoint(k1.x - 4, k1.y - 4);
          FLayer.Bitmap.FillRectS(bounds(Round(k1.X), Round(k1.y), 8, 8), FPolyPointColor);
        end;
      end;
      k1 := VPointsOnBitmap[0];
      if ((k1.x > 0) and (k1.y > 0)) and ((k1.x < VBitmapSize.X) and (k1.y < VBitmapSize.Y)) then begin
        k1 := DoublePoint(k1.x - 4, k1.y - 4);
        FLayer.Bitmap.FillRectS(bounds(Round(k1.X), Round(k1.y), 8, 8), FPolyFirstPointColor);
      end;
      k1 := VPointsOnBitmap[FPolyActivePointIndex];
      if ((k1.x > 0) and (k1.y > 0)) and ((k1.x < VBitmapSize.X) and (k1.y < VBitmapSize.Y)) then begin
        k1 := DoublePoint(k1.x - 4, k1.y - 4);
        FLayer.Bitmap.FillRectS(bounds(Round(k1.X), Round(k1.y), 8, 8), FPolyActivePointColor);
      end;
    finally
      VPointsOnBitmap := nil;
    end;
  end;
end;

procedure TMapNalLayer.DoDrawSelectionPoly;
var
  i: integer;
  k1: TDoublePoint;
  Polygon: TPolygon32;
  VBitmapSize: TPoint;
  VPointsOnBitmap: TDoublePointArray;
  VPointsCount: Integer;
begin
  try
    polygon := TPolygon32.Create;
    polygon.Antialiased := true;
    polygon.AntialiasMode := am4times;
    polygon.Closed := true;
    PreparePolygon(FPath,polygon);
    Polygon.DrawFill(FLayer.Bitmap, SetAlpha(clWhite32, 40));
    with Polygon.Outline do try
       with Grow(Fixed(FPolyLineWidth / 2), 0.5) do try
         FillMode := pfWinding;
         DrawFill(FLayer.Bitmap, SetAlpha(clBlue32, 180));
       finally
         free;
       end;
    finally
      free;
    end;
  finally
    polygon.Free;
  end;

  VBitmapSize := GetBitmapSizeInPixel;
  VPointsCount := Length(FPath);
  if VPointsCount > 0 then begin
    SetLength(VPointsOnBitmap, VPointsCount);
    try
      for i := 0 to VPointsCount - 1 do begin
        VPointsOnBitmap[i] := MapPixel2BitmapPixel(FGeoConvert.LonLat2PixelPosFloat(FPath[i], FZoom));
      end;
      k1 := VPointsOnBitmap[0];
      if ((k1.x > 0) and (k1.y > 0)) and ((k1.x < VBitmapSize.X) and (k1.y < VBitmapSize.Y)) then begin
        k1 := DoublePoint(k1.x - 3, k1.y - 3);
        FLayer.Bitmap.FillRectS(bounds(Round(k1.X), Round(k1.Y), 6, 6), SetAlpha(ClGreen32, 255));
      end;
      if VPointsCount > 1 then begin
        k1 := VPointsOnBitmap[VPointsCount - 1];
        if ((k1.x > 0) and (k1.y > 0)) and ((k1.x < VBitmapSize.X) and (k1.y < VBitmapSize.Y)) then begin
          k1 := DoublePoint(k1.x - 3, k1.y - 3);
          FLayer.Bitmap.FillRectS(bounds(Round(k1.X), Round(k1.Y), 6, 6), SetAlpha(ClRed32, 255));
        end;
      end;
    finally
      VPointsOnBitmap := nil;
    end;
  end;
end;

procedure TMapNalLayer.DoDrawSelectionRect;
var
  kz, jj: integer;
  xy1, xy2: TPoint;
  VSelectedPixels: TRect;
  VZoomDelta: Byte;
  VColor: TColor32;
  VSelectedRelative: TDoubleRect;
  VSelectedTiles: TRect;
begin
  VSelectedPixels := FGeoConvert.LonLatRect2PixelRect(FSelectedLonLat, FZoom);

  xy1 := MapPixel2BitmapPixel(VSelectedPixels.TopLeft);
  xy1.x := xy1.x;
  xy1.y := xy1.y;
  xy2 := MapPixel2BitmapPixel(VSelectedPixels.BottomRight);
  xy2.x := xy2.x;
  xy2.y := xy2.y;

  FLayer.Bitmap.FillRectS(xy1.x, xy1.y, xy2.x, xy2.y, SetAlpha(clWhite32, 20));
  FLayer.Bitmap.FrameRectS(xy1.x, xy1.y, xy2.x, xy2.y, SetAlpha(clBlue32, 150));
  FLayer.Bitmap.FrameRectS(xy1.x - 1, xy1.y - 1, xy2.x + 1, xy2.y + 1, SetAlpha(clBlue32, 150));

  VSelectedRelative := FGeoConvert.PixelRect2RelativeRect(VSelectedPixels, FZoom);

  jj := FZoom;
  VZoomDelta := 0;
  while (VZoomDelta < 3) and (jj < 24) do begin
    VSelectedTiles := FGeoConvert.RelativeRect2TileRect(VSelectedRelative, jj);
    VSelectedPixels := FGeoConvert.RelativeRect2PixelRect(
      FGeoConvert.TileRect2RelativeRect(VSelectedTiles, jj), FZoom
    );

    xy1 := MapPixel2BitmapPixel(VSelectedPixels.TopLeft);
    xy2 := MapPixel2BitmapPixel(VSelectedPixels.BottomRight);

    kz := 256 shr VZoomDelta;
    VColor := SetAlpha(RGB(kz - 1, kz - 1, kz - 1), 255);

    FLayer.Bitmap.FrameRectS(
      xy1.X - (VZoomDelta + 1), xy1.Y - (VZoomDelta + 1),
      xy2.X + (VZoomDelta + 1), xy2.Y + (VZoomDelta + 1),
      VColor
    );

    FLayer.Bitmap.Font.Size := 11;
    FLayer.Bitmap.RenderText(
      xy2.x - ((xy2.x - xy1.x) div 2) - 42 + VZoomDelta * 26,
      xy2.y - ((xy2.y - xy1.y) div 2) - 6,
      'x' + inttostr(jj + 1), 3, VColor
    );
    Inc(jj);
    Inc(VZoomDelta);
  end;
end;

procedure TMapNalLayer.DoRedraw;
begin
  inherited;
  FLayer.Bitmap.Clear(clBlack);
  case FDrawType of
    mndtNothing:;
    mndtSelectRect: DoDrawSelectionRect;
    mndtSelectPoly: DoDrawSelectionPoly;
    mndtCalcLen: DoDrawCalcLine;
    mndtNewPath: DoDrawNewPath(False);
    mndtNewPoly: DoDrawNewPath(True);
  end;
end;

procedure TMapNalLayer.DrawLineCalc(APathLonLat: TDoublePointArray; ALenShow: Boolean; AActiveIndex: Integer);
begin

  FDrawType := mndtCalcLen;
  FPath := Copy(APathLonLat);

  FPolyActivePointIndex := AActiveIndex;
  FLenShow := ALenShow;
  Redraw;
end;

procedure TMapNalLayer.DrawNewPath(APathLonLat: TDoublePointArray;
  AIsPoly: boolean; AActiveIndex: Integer);
begin
  if AIsPoly then begin
    FDrawType := mndtNewPoly;
  end else begin
    FDrawType := mndtNewPath;
  end;
  FPath := Copy(APathLonLat);
  FPolyActivePointIndex := AActiveIndex;
  if (FPolyActivePointIndex < 0) or (FPolyActivePointIndex >= length(FPath)) then begin
    FPolyActivePointIndex := length(FPath) - 1;
  end;
  Redraw;
end;

procedure TMapNalLayer.DrawNothing;
begin
  FDrawType := mndtNothing;
  FPath := nil;
  Redraw;
end;

procedure TMapNalLayer.DrawReg(ASelectedLonLatPoly: TDoublePointArray);
begin
  FDrawType := mndtSelectPoly;
  FPath := Copy(ASelectedLonLatPoly);
  Redraw;
end;

procedure TMapNalLayer.DrawSelectionRect(ASelectedLonLat: TDoubleRect);
begin
  FDrawType := mndtSelectRect;
  FPath := nil;
  FSelectedLonLat := ASelectedLonLat;
  Redraw;
end;

procedure TMapNalLayer.PreparePolygon(pathll: TExtendedPointArray; polygon: TPolygon32);

function MapPixel2BitmapPixel( Pnt: TExtendedPoint): TExtendedPoint;
var     VRect: TRect;
begin
  VRect.TopLeft := BitmapPixel2MapPixel(Point(0, 0));
  VRect.BottomRight := BitmapPixel2MapPixel(GetBitmapSizeInPixel);
  Result.X := Pnt.X - VRect.Left;
  Result.Y := Pnt.Y - VRect.Top;
end;

var
  i, adp, j, lenpath: integer;
  k1: TextendedPoint;
  k2: TextendedPoint;
  k4: TextendedPoint;
  k3: TextendedPoint;
  kOld: TextendedPoint;
  VLonLat: TExtendedPoint;
  pathllbuf:TExtendedPointArray;
begin
   lenpath:=length(pathll);
   VLonLat := pathll[0];
   FGeoConvert.CheckLonLatPos(VLonLat);
   k1 := FGeoConvert.LonLat2PixelPosFloat(VLonLat, FZoom);
   k1 := MapPixel2BitmapPixel(k1);
   for i := 0 to lenpath-2 do begin
      VLonLat := pathll[i+1];
      FGeoConvert.CheckLonLatPos(VLonLat);
      k2 := FGeoConvert.LonLat2PixelPosFloat(VLonLat, FZoom);
      k2 := MapPixel2BitmapPixel(k2);

      if (k1.X<32766)and(k1.X>-32766)and(k1.Y<32766)and(k1.Y>-32766) then begin
        polygon.Add(FixedPoint(k1.X, k1.Y));
      end;

      if (abs(k1.x)>16383)or(abs(k2.x)>16383)or(abs(k1.y)>16383)or(abs(k2.y)>16383) then begin
        if abs(k2.x - k1.x) > abs(k2.y - k1.y) then begin
          adp := (Trunc(abs(k2.x - k1.x) / 32766) + 1)*3;
        end else begin
          adp := (Trunc(abs(k2.y - k1.y) / 32766) + 1)*3;
        end;
        if adp > 1 then begin
          k3 := extPoint(((k2.X - k1.x) / adp), ((k2.y - k1.y) / adp));
          for j := 1 to adp - 1 do begin
            k4 := extPoint((k1.x + k3.x * j), (k1.Y + k3.y * j));
            if (k4.X<32766)and(k4.X>-32766)and(k4.Y<32766)and(k4.Y>-32766) then begin
              polygon.Add(FixedPoint(k4.X, k4.Y));
            end;
          end;
        end;
      end;

      if (k2.X<32766)and(k2.X>-32766)and(k2.Y<32766)and(k2.Y>-32766) then begin
        polygon.Add(FixedPoint(k2.X, k2.Y));
      end;

      k1:=k2;
  end;
end;

end.
