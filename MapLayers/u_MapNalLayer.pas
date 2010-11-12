unit u_MapNalLayer;

interface

uses
  Windows,
  GR32,
  GR32_Image,
  t_GeoTypes,
  u_MapViewPortState,
  u_MapLayerBasic;

type
  TMapNalDrawType = (mndtNothing, mndtSelectRect, mndtSelectPoly, mndtCalcLen, mndtNewPath, mndtNewPoly);

  TMapNalLayer = class(TMapLayerBasic)
  private
    FDrawType: TMapNalDrawType;
    FPath: TExtendedPointArray;
    FSelectedLonLat: TExtendedRect;
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
  protected
    procedure DoRedraw; override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    destructor Destroy; override;
    procedure DrawNothing;
    procedure DrawSelectionRect(ASelectedLonLat: TExtendedRect);
    procedure DrawReg(ASelectedLonLatPoly: TExtendedPointArray);
    procedure DrawLineCalc(APathLonLat: TExtendedPointArray; ALenShow: Boolean; AActiveIndex: Integer);
    procedure DrawNewPath(APathLonLat: TExtendedPointArray; AIsPoly: boolean; AActiveIndex: Integer);
  end;

implementation

uses
  Types,
  Graphics,
  SysUtils,
  GR32_Polygons,
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
  i, j, textW, adp: integer;
  k1: TExtendedPoint;
  k2: TExtendedPoint;
  k4: TExtendedPoint;
  k3: TExtendedPoint;
  len: real;
  text: string;
  polygon: TPolygon32;
  VBitmapSize: TPoint;
  VPointsOnBitmap: TExtendedPointArray;
  VPointsCount: Integer;
begin
  VBitmapSize := GetBitmapSizeInPixel;
  FLayer.Bitmap.Font.Name := 'Tahoma';
  VPointsCount := Length(FPath);
  if VPointsCount > 0 then begin
    SetLength(VPointsOnBitmap, VPointsCount);
    try
      for i := 0 to VPointsCount - 1 do begin
        VPointsOnBitmap[i] := MapPixel2BitmapPixel(FGeoConvert.LonLat2PixelPosFloat(FPath[i], FZoom));
      end;
      polygon := TPolygon32.Create;
      try
        polygon.Antialiased := true;
        polygon.AntialiasMode := am4times;
        polygon.Closed := false;
        for i := 0 to VPointsCount - 1 do begin
          k1 := VPointsOnBitmap[i];
          if (k1.x < 32767) and (k1.x > -32767) and (k1.y < 32767) and (k1.y > -32767) then begin
            polygon.Add(FixedPoint(k1.X, k1.Y));
          end;
          if i < VPointsCount - 1 then begin
            k2 := VPointsOnBitmap[i+1];
            if (k2.x - k1.x) > (k2.y - k1.y) then begin
              adp := Trunc((k2.x - k1.x) / 32767) + 2;
            end else begin
              adp := Trunc((k2.y - k1.y) / 32767) + 2;
            end;
            k3 := ExtPoint(((k2.X - k1.x) / adp), ((k2.y - k1.y) / adp));
            if adp > 2 then begin
              for j := 1 to adp - 1 do begin
                k4 := ExtPoint(k1.x + k3.x * j, k1.Y + k3.y * j);
                if (k4.x < 32767) and (k4.x > -32767) and (k4.y < 32767) and (k4.y > -32767) then begin
                  polygon.Add(FixedPoint(k4.x, k4.y));
                end;
              end;
            end;
          end;
        end;
        with Polygon.Outline do try
          with Grow(Fixed(2.5 / 2), 0.5) do try
            FillMode := pfWinding;
            DrawFill(FLayer.Bitmap, SetAlpha(ClRed32, 150));
          finally
            Free;
          end;
        finally
          Free;
        end;
      finally
        polygon.Free;
      end;
      for i := 0 to VPointsCount - 2 do begin
        k2 := VPointsOnBitmap[i + 1];
        if not ((k2.x > 0) and (k2.y > 0)) and ((k2.x < VBitmapSize.X) and (k2.y < VBitmapSize.Y)) then begin
          continue;
        end;
        FLayer.Bitmap.FrameRectS(
          Round(k2.x - 3),
          Round(k2.y - 3),
          Round(k2.X + 3),
          Round(k2.Y + 3),
          SetAlpha(ClRed32, 150)
        );
        FLayer.Bitmap.FillRectS(
          Round(k2.x - 2),
          Round(k2.y - 2),
          Round(k2.X + 2),
          Round(k2.y + 2),
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
            Round(k2.x + 12),
            Round(k2.y),
            Round(k2.X + textW),
            Round(k2.y + 15),
            SetAlpha(ClWhite32, 110)
          );
          FLayer.Bitmap.RenderText(
            Round(k2.X + 15),
            Round(k2.y),
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
              Round(k2.x + 5),
              Round(k2.y + 5),
              Round(k2.X + textW),
              Round(k2.y + 16),
              SetAlpha(ClWhite32, 110)
            );
            FLayer.Bitmap.RenderText(
              Round(k2.X + 8),
              Round(k2.y + 5),
              text,
              0,
              clBlack32
            );
          end;
        end;
      end;
      k1 := VPointsOnBitmap[0];
      if ((k1.x > 0) and (k1.y > 0)) and ((k1.x < VBitmapSize.X) and (k1.y < VBitmapSize.Y)) then begin
        k1 := ExtPoint(k1.x - 3, k1.y - 3);
        FLayer.Bitmap.FillRectS(bounds(Round(k1.x), Round(k1.y), 6, 6), SetAlpha(ClGreen32, 255));
      end;
      k1 := VPointsOnBitmap[FPolyActivePointIndex];
      if ((k1.x > 0) and (k1.y > 0)) and ((k1.x < VBitmapSize.X) and (k1.y < VBitmapSize.Y)) then begin
        k1 := ExtPoint(k1.x - 3, k1.y - 3);
        FLayer.Bitmap.FillRectS(bounds(Round(k1.x), Round(k1.y), 6, 6), SetAlpha(ClRed32, 255));
      end;
    finally
      VPointsOnBitmap := nil;
    end;
  end;
end;

procedure TMapNalLayer.DoDrawNewPath(AIsPoly: Boolean);
var
  i, adp, j: integer;
  k1, k2, k4: TPoint;
  k3: TextendedPoint;
  polygon: TPolygon32;
begin
  polygon := TPolygon32.Create;
  try
    polygon.Antialiased := true;
    polygon.AntialiasMode := am4times;
    polygon.Closed := AIsPoly;
    if length(FPath) > 0 then begin
      for i := 0 to length(FPath) - 1 do begin
        k1 := FGeoConvert.LonLat2PixelPos(FPath[i], FZoom);
        k1 := MapPixel2BitmapPixel(k1);
        if (k1.x < 32767) and (k1.x > -32767) and (k1.y < 32767) and (k1.y > -32767) then begin
          polygon.Add(FixedPoint(k1));
        end;
        if i < length(FPath) - 1 then begin
          k2 := FGeoConvert.LonLat2PixelPos(Fpath[i + 1], FZoom);
          k2 := MapPixel2BitmapPixel(k2);
          if (k2.x - k1.x) > (k2.y - k1.y) then begin
            adp := (k2.x - k1.x) div 32767 + 2;
          end else begin
            adp := (k2.y - k1.y) div 32767 + 2;
          end;
          k3 := extPoint(((k2.X - k1.x) / adp), ((k2.y - k1.y) / adp));
          if adp > 2 then begin
            for j := 1 to adp - 1 do begin
              k4 := Point(round(k1.x + k3.x * j), round(k1.Y + k3.y * j));
              if (k4.x < 32767) and (k4.x > -32767) and (k4.y < 32767) and (k4.y > -32767) then begin
                polygon.Add(FixedPoint(k4.x, k4.y));
              end;
            end;
          end;
        end;
      end;
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
      for i := 1 to length(FPath) - 1 do begin
        k1 := FGeoConvert.LonLat2PixelPos(Fpath[i], FZoom);
        k1 := MapPixel2BitmapPixel(k1);
        k1 := Point(k1.x - 4, k1.y - 4);
        FLayer.Bitmap.FillRectS(bounds(k1.X, k1.y, 8, 8), FPolyPointColor);
      end;
    end;
  finally
    polygon.Free;
  end;
  if (length(FPath) > 0) then begin
    k1 := FGeoConvert.LonLat2PixelPos(Fpath[0], FZoom);
    k1 := MapPixel2BitmapPixel(k1);
    k1 := Point(k1.x - 4, k1.y - 4);
    FLayer.Bitmap.FillRectS(bounds(k1.X, k1.y, 8, 8), FPolyFirstPointColor);
    k1 := FGeoConvert.LonLat2PixelPos(Fpath[FPolyActivePointIndex], FZoom);
    k1 := MapPixel2BitmapPixel(k1);
    k1 := Point(k1.x - 4, k1.y - 4);
    FLayer.Bitmap.FillRectS(bounds(k1.X, k1.y, 8, 8), FPolyActivePointColor);
  end;
end;

procedure TMapNalLayer.DoDrawSelectionPoly;
var
  i: integer;
  k1: TPoint;
  Polygon: TPolygon32;
begin
  Polygon := TPolygon32.Create;
  Polygon.Antialiased := true;
  Polygon.AntialiasMode := am32times;
  Polygon.FillMode := pfAlternate;
  for i := 0 to length(FPath) - 1 do begin
    k1 := FGeoConvert.LonLat2PixelPos(FPath[i], FZoom);
    k1 := MapPixel2BitmapPixel(k1);
    Polygon.add(FixedPoint(k1.x, k1.Y));
  end;
  with Polygon.Outline do begin
    FillMode := pfWinding;
    with Grow(Fixed(2 / 2), 0.5) do begin
      DrawFill(FLayer.Bitmap, SetAlpha(clBlue32, 180));
      free;
    end;
    free;
  end;
  Polygon.DrawFill(FLayer.Bitmap, SetAlpha(clWhite32, 40));
  if length(FPath) > 0 then begin
    k1 := FGeoConvert.LonLat2PixelPos(FPath[0], FZoom);
    k1 := MapPixel2BitmapPixel(k1);
    k1 := Point(k1.x - 3, k1.y - 3);
    FLayer.Bitmap.FillRectS(bounds(k1.X, k1.Y, 6, 6), SetAlpha(ClGreen32, 255));
    if length(FPath) > 1 then begin
      k1 := FGeoConvert.LonLat2PixelPos(FPath[length(FPath) - 1], FZoom);
      k1 := MapPixel2BitmapPixel(k1);
      k1 := Point(k1.x - 3, k1.y - 3);
      FLayer.Bitmap.FillRectS(bounds(k1.X, k1.Y, 6, 6), SetAlpha(ClRed32, 255));
    end;
  end;
  Polygon.Free;
end;

procedure TMapNalLayer.DoDrawSelectionRect;
var
  kz, jj: integer;
  xy1, xy2: TPoint;
  VSelectedPixels: TRect;
  VZoomDelta: Byte;
  VColor: TColor32;
  VSelectedRelative: TExtendedRect;
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

procedure TMapNalLayer.DrawLineCalc(APathLonLat: TExtendedPointArray; ALenShow: Boolean; AActiveIndex: Integer);
begin
  FDrawType := mndtCalcLen;
  FPath := Copy(APathLonLat);
  FPolyActivePointIndex := AActiveIndex;
  FLenShow := ALenShow;
  Redraw;
end;

procedure TMapNalLayer.DrawNewPath(APathLonLat: TExtendedPointArray;
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

procedure TMapNalLayer.DrawReg(ASelectedLonLatPoly: TExtendedPointArray);
begin
  FDrawType := mndtSelectPoly;
  FPath := Copy(ASelectedLonLatPoly);
  Redraw;
end;

procedure TMapNalLayer.DrawSelectionRect(ASelectedLonLat: TExtendedRect);
begin
  FDrawType := mndtSelectRect;
  FPath := nil;
  FSelectedLonLat := ASelectedLonLat;
  Redraw;
end;

end.
