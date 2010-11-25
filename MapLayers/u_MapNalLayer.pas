unit u_MapNalLayer;

interface

uses
  Windows,
  GR32,
  GR32_Image,
  GR32_Polygons,
  t_GeoTypes,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  u_MapViewPortState,
  u_ClipPolygonByRect,
  u_MapLayerBasic;

type
  TMapNalDrawType = (mndtNothing, mndtSelectRect, mndtSelectPoly, mndtCalcLen, mndtNewPath, mndtNewPoly);

  TMapNalLayer = class(TMapLayerBasic)
  private
    FBitmapClip: IPolyClip;
    FDrawType: TMapNalDrawType;
    FPath: TExtendedPointArray;
    FSelectedLonLat: TExtendedRect;
    FPolyActivePointIndex: integer;
    FLenShow: Boolean;

    FEditMarkLineColor: TColor32;
    FEditMarkFillColor: TColor32;
    FEditMarkLineWidth: Integer;
    FEditMarkPointColor: TColor32;
    FEditMarkActivePointColor: TColor32;
    FEditMarkFirstPointColor: TColor32;
    FEditMarkPointSize: Integer;

    FCalcLineColor: TColor32;
    FCalcLineWidth: integer;
    FCalcPointFillColor: TColor32;
    FCalcPointRectColor: TColor32;
    FCalcPointFirstColor: TColor32;
    FCalcPointActiveColor: TColor32;
    FCalcPointSize: integer;
    FCalcTextColor: TColor32;
    FCalcTextBGColor: TColor32;

    FSelectionPolyFillColor: TColor32;
    FSelectionPolyLineColor: TColor32;
    FSelectionPolyLineWidth: Integer;
    FSelectionPolyPointFirstColor: TColor32;
    FSelectionPolyPointLastColor: TColor32;
    FSelectionPolyPointSize: Integer;

    FSelectionRectFillColor: TColor32;
    FSelectionRectBorderColor: TColor32;
    FSelectionRectZoomDeltaColor: array [0..2] of TColor32;

    procedure DrawPolyPoint(
      const ABitmapSize: TPoint;
      const APosOnBitmap: TExtendedPoint;
      const ASize: Integer;
      const AFillColor: TColor32;
      const ARectColor: TColor32
    );
    procedure DoDrawSelectionRect;
    procedure DoDrawSelectionPoly;
    procedure DoDrawCalcLine;
    procedure DoDrawNewPath(AIsPoly: Boolean);
  protected
    procedure DoRedraw; override;
    procedure DoResizeBitmap; override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    destructor Destroy; override;
    procedure LoadConfig(AConfigProvider: IConfigDataProvider); override;
    procedure SaveConfig(AConfigProvider: IConfigDataWriteProvider); override;
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
  Ugeofun,
  u_GeoToStr,
  UResStrings,
  u_GlobalState,
  u_WindowLayerBasic;

{ TMapNalLayer }

constructor TMapNalLayer.Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
var
  i: Integer;
  kz: Integer;
begin
  inherited;
  FLayer.Bitmap.Font.Name := 'Tahoma';
  FEditMarkPointColor := SetAlpha(clYellow32, 150);
  FEditMarkActivePointColor := SetAlpha(ClRed32, 255);
  FEditMarkFirstPointColor := SetAlpha(ClGreen32, 255);
  FEditMarkLineColor := SetAlpha(ClRed32, 150);
  FEditMarkFillColor := SetAlpha(ClWhite32, 50);
  FEditMarkLineWidth := 3;
  FEditMarkPointSize := 8;
  FCalcLineColor := SetAlpha(ClRed32, 150);
  FCalcTextColor := clBlack32;
  FCalcTextBGColor := SetAlpha(ClWhite32, 110);
  FCalcPointFillColor := SetAlpha(ClWhite32, 150);
  FCalcPointRectColor := SetAlpha(ClRed32, 150);
  FCalcPointFirstColor := SetAlpha(ClGreen32, 255);
  FCalcPointActiveColor := SetAlpha(ClRed32, 255);
  FCalcLineWidth := 3;
  FCalcPointSize := 6;
  FSelectionPolyFillColor := SetAlpha(clWhite32, 40);
  FSelectionPolyLineColor := SetAlpha(clBlue32, 180);
  FSelectionPolyPointFirstColor := SetAlpha(ClGreen32, 255);
  FSelectionPolyPointLastColor := SetAlpha(ClRed32, 255);
  FSelectionPolyLineWidth := 3;
  FSelectionPolyPointSize := 6;
  FSelectionRectFillColor := SetAlpha(clWhite32, 20);
  FSelectionRectBorderColor := SetAlpha(clBlue32, 150);
  for i := 0 to Length(FSelectionRectZoomDeltaColor) - 1 do begin
    kz := 256 shr i;
    FSelectionRectZoomDeltaColor[i] := SetAlpha(RGB(kz - 1, kz - 1, kz - 1), 255);
  end;
end;

destructor TMapNalLayer.Destroy;
begin
  FPath := nil;
  inherited;
end;

procedure TMapNalLayer.DoDrawCalcLine;
var
  i, j, textW: integer;
  k1: TExtendedPoint;
  len: real;
  text: string;
  polygon: TPolygon32;
  VBitmapSize: TPoint;
  VPointsOnBitmap: TExtendedPointArray;
  VPointsCount: Integer;
  VLonLat: TExtendedPoint;
begin
  VPointsCount := Length(FPath);
  if VPointsCount > 0 then begin
    SetLength(VPointsOnBitmap, VPointsCount);
    for i := 0 to VPointsCount - 1 do begin
      VLonLat := FPath[i];
      FGeoConvert.CheckLonLatPos(VLonLat);
      VPointsOnBitmap[i] := MapPixel2BitmapPixel(FGeoConvert.LonLat2PixelPosFloat(VLonLat, FZoom));
    end;

    polygon := TPolygon32.Create;
    try
      polygon.Antialiased := true;
      polygon.AntialiasMode := am4times;
      polygon.Closed := false;
      PrepareGR32Polygon(VPointsOnBitmap, polygon);
      with Polygon.Outline do try
         with Grow(Fixed(FCalcLineWidth / 2), 0.5) do try
           FillMode := pfWinding;
           DrawFill(FLayer.Bitmap, FCalcLineColor);
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
    try
      for i := 0 to VPointsCount - 2 do begin
        k1 := VPointsOnBitmap[i + 1];
        if ((k1.x > 0) and (k1.y > 0)) and ((k1.x < VBitmapSize.X) and (k1.y < VBitmapSize.Y)) then begin
          if i = VPointsCount - 2 then begin
            len := 0;
            for j := 0 to i do begin
              len := len + FGeoConvert.CalcDist(FPath[j], FPath[j + 1]);
            end;
            text := SAS_STR_Whole + ': ' + DistToStrWithUnits(len, GState.num_format);
            FLayer.Bitmap.Font.Size := 9;
            textW := FLayer.Bitmap.TextWidth(text) + 11;
            FLayer.Bitmap.FillRectS(
              Trunc(k1.x + 12),
              Trunc(k1.y),
              Trunc(k1.X + textW),
              Trunc(k1.y + 15),
              FCalcTextBGColor
            );
            FLayer.Bitmap.RenderText(
              Trunc(k1.X + 15),
              Trunc(k1.y),
              text,
              3,
              FCalcTextColor
            );
          end else begin
            if FLenShow then begin
              text := DistToStrWithUnits(FGeoConvert.CalcDist(FPath[i], FPath[i + 1]), GState.num_format);
              FLayer.Bitmap.Font.Size := 7;
              textW := FLayer.Bitmap.TextWidth(text) + 11;
              FLayer.Bitmap.FillRectS(
                Trunc(k1.x + 5),
                Trunc(k1.y + 5),
                Trunc(k1.X + textW),
                Trunc(k1.y + 16),
                FCalcTextBGColor
              );
              FLayer.Bitmap.RenderText(
                Trunc(k1.X + 8),
                Trunc(k1.y + 5),
                text,
                0,
                FCalcTextColor
              );
            end;
          end;
          DrawPolyPoint(VBitmapSize, k1, FCalcPointSize, FCalcPointFillColor, FCalcPointRectColor);
        end;
      end;
      k1 := VPointsOnBitmap[0];
      DrawPolyPoint(VBitmapSize, k1, FCalcPointSize, FCalcPointFirstColor, FCalcPointFirstColor);
      k1 := VPointsOnBitmap[FPolyActivePointIndex];
      DrawPolyPoint(VBitmapSize, k1, FCalcPointSize, FCalcPointActiveColor, FCalcPointActiveColor);
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
  VPointsOnBitmap: TExtendedPointArray;
  VPointsCount: Integer;
  VPointsOnBitmapPrepared: TExtendedPointArray;
  VPointsProcessedCount: Integer;
  VLonLat: TExtendedPoint;
  VPathFixedPoints: TArrayOfFixedPoint;
begin
  VPointsCount := Length(FPath);
  if VPointsCount > 0 then begin
    SetLength(VPointsOnBitmap, VPointsCount + 1);
    for i := 0 to VPointsCount - 1 do begin
      VLonLat := FPath[i];
      FGeoConvert.CheckLonLatPos(VLonLat);
      VPointsOnBitmap[i] := MapPixel2BitmapPixel(FGeoConvert.LonLat2PixelPosFloat(VLonLat, FZoom));
    end;
    if AIsPoly then begin
      if not compare2EP(VPointsOnBitmap[0], VPointsOnBitmap[VPointsCount - 1]) then begin
        VPointsOnBitmap[VPointsCount] := VPointsOnBitmap[0];
        Inc(VPointsCount);
      end;
    end;
    VPointsProcessedCount := FBitmapClip.Clip(VPointsOnBitmap, VPointsCount, VPointsOnBitmapPrepared);
    if VPointsProcessedCount > 0 then begin
      if VPointsProcessedCount > 1 then begin
        SetLength(VPathFixedPoints, VPointsProcessedCount);
        for i := 0 to VPointsProcessedCount - 1 do begin
          VPathFixedPoints[i] := FixedPoint(VPointsOnBitmapPrepared[i].X, VPointsOnBitmapPrepared[i].Y);
        end;
        polygon := TPolygon32.Create;
        try
          polygon.Antialiased := true;
          polygon.AntialiasMode := am4times;
          polygon.Closed := AIsPoly;
          polygon.AddPoints(VPathFixedPoints[0], VPointsProcessedCount);
          if AIsPoly then begin
            Polygon.DrawFill(FLayer.Bitmap, FEditMarkFillColor);
          end;
          with Polygon.Outline do try
             with Grow(Fixed(FEditMarkLineWidth / 2), 0.5) do try
               FillMode := pfWinding;
               DrawFill(FLayer.Bitmap, FEditMarkLineColor);
             finally
               free;
             end;
          finally
            free;
          end;
        finally
          polygon.Free;
        end;
      end;
      VBitmapSize := GetBitmapSizeInPixel;
      try
        for i := 1 to VPointsProcessedCount - 1 do begin
          k1 := VPointsOnBitmapPrepared[i];
          DrawPolyPoint(VBitmapSize, k1, FEditMarkPointSize, FEditMarkPointColor, FEditMarkPointColor);
        end;
        k1 := VPointsOnBitmapPrepared[0];
        DrawPolyPoint(VBitmapSize, k1, FEditMarkPointSize, FEditMarkFirstPointColor, FEditMarkFirstPointColor);
        k1 := VPointsOnBitmapPrepared[FPolyActivePointIndex];
        DrawPolyPoint(VBitmapSize, k1, FEditMarkPointSize, FEditMarkActivePointColor, FEditMarkActivePointColor);
      finally
        VPointsOnBitmap := nil;
      end;
    end;
  end;
end;

procedure TMapNalLayer.DoDrawSelectionPoly;
var
  i: integer;
  k1: TExtendedPoint;
  Polygon: TPolygon32;
  VBitmapSize: TPoint;
  VPointsOnBitmap: TExtendedPointArray;
  VPointsCount: Integer;
  VLonLat: TExtendedPoint;
begin
  VPointsCount := Length(FPath);
  if VPointsCount > 0 then begin
    SetLength(VPointsOnBitmap, VPointsCount);
    for i := 0 to VPointsCount - 1 do begin
      VLonLat := FPath[i];
      FGeoConvert.CheckLonLatPos(VLonLat);
      VPointsOnBitmap[i] := MapPixel2BitmapPixel(FGeoConvert.LonLat2PixelPosFloat(VLonLat, FZoom));
    end;
    polygon := TPolygon32.Create;
    try
      polygon.Antialiased := true;
      polygon.AntialiasMode := am4times;
      polygon.Closed := true;
      PrepareGR32Polygon(VPointsOnBitmap, polygon);
      Polygon.DrawFill(FLayer.Bitmap, FSelectionPolyFillColor);
      with Polygon.Outline do try
         with Grow(Fixed(FSelectionPolyLineWidth / 2), 0.5) do try
           FillMode := pfWinding;
           DrawFill(FLayer.Bitmap, FSelectionPolyLineColor);
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
    try
      k1 := VPointsOnBitmap[0];
      DrawPolyPoint(VBitmapSize, k1, FSelectionPolyPointSize, FSelectionPolyPointFirstColor, FSelectionPolyPointFirstColor);
      if VPointsCount > 1 then begin
        k1 := VPointsOnBitmap[VPointsCount - 1];
        DrawPolyPoint(VBitmapSize, k1, FSelectionPolyPointSize, FSelectionPolyPointLastColor, FSelectionPolyPointLastColor);
      end;
    finally
      VPointsOnBitmap := nil;
    end;
  end;
end;

procedure TMapNalLayer.DoDrawSelectionRect;
var
  jj: integer;
  xy1, xy2: TPoint;
  VSelectedPixels: TRect;
  VZoomDelta: Byte;
  VColor: TColor32;
  VSelectedRelative: TExtendedRect;
  VSelectedTiles: TRect;
  VMaxZoomDelta: Integer;
begin
  VSelectedPixels := FGeoConvert.LonLatRect2PixelRect(FSelectedLonLat, FZoom);

  xy1 := MapPixel2BitmapPixel(VSelectedPixels.TopLeft);
  xy1.x := xy1.x;
  xy1.y := xy1.y;
  xy2 := MapPixel2BitmapPixel(VSelectedPixels.BottomRight);
  xy2.x := xy2.x;
  xy2.y := xy2.y;

  FLayer.Bitmap.FillRectS(xy1.x, xy1.y, xy2.x, xy2.y, FSelectionRectFillColor);
  FLayer.Bitmap.FrameRectS(xy1.x, xy1.y, xy2.x, xy2.y, FSelectionRectBorderColor);
  FLayer.Bitmap.FrameRectS(xy1.x - 1, xy1.y - 1, xy2.x + 1, xy2.y + 1, FSelectionRectBorderColor);

  VSelectedRelative := FGeoConvert.PixelRect2RelativeRect(VSelectedPixels, FZoom);

  jj := FZoom;
  VZoomDelta := 0;
  VMaxZoomDelta := Length(FSelectionRectZoomDeltaColor) - 1;
  while (VZoomDelta <= VMaxZoomDelta) and (jj < 24) do begin
    VSelectedTiles := FGeoConvert.RelativeRect2TileRect(VSelectedRelative, jj);
    VSelectedPixels := FGeoConvert.RelativeRect2PixelRect(
      FGeoConvert.TileRect2RelativeRect(VSelectedTiles, jj), FZoom
    );

    xy1 := MapPixel2BitmapPixel(VSelectedPixels.TopLeft);
    xy2 := MapPixel2BitmapPixel(VSelectedPixels.BottomRight);

    VColor := FSelectionRectZoomDeltaColor[VZoomDelta];

    FLayer.Bitmap.FrameRectS(
      xy1.X - (VZoomDelta + 1), xy1.Y - (VZoomDelta + 1),
      xy2.X + (VZoomDelta + 1), xy2.Y + (VZoomDelta + 1),
      VColor
    );

    FLayer.Bitmap.Font.Size := 11;
    FLayer.Bitmap.RenderText(
      xy2.x - ((xy2.x - xy1.x) div 2) - 42 + VZoomDelta * 26,
      xy2.y - ((xy2.y - xy1.y) div 2) - 6,
      'z' + inttostr(jj + 1), 3, VColor
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

procedure TMapNalLayer.DoResizeBitmap;
var
  VSize: TPoint;
begin
  inherited;
  VSize := GetBitmapSizeInPixel;
  FBitmapClip := TPolyClipByRect.Create(MakeRect(0, 0, VSize.X, VSize.Y));
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

procedure TMapNalLayer.DrawPolyPoint(const ABitmapSize: TPoint;
  const APosOnBitmap: TExtendedPoint; const ASize: Integer; const AFillColor,
  ARectColor: TColor32);
var
  VHalfSize: Extended;
  VRect: TRect;
  VRectFloat: TExtendedRect;
begin
  VHalfSize := ASize / 2;
  VRectFloat.Left := APosOnBitmap.X - VHalfSize;
  VRectFloat.Top := APosOnBitmap.Y - VHalfSize;
  VRectFloat.Right := VRectFloat.Left + ASize;
  VRectFloat.Bottom := VRectFloat.Top + ASize;
  if
    (VRectFloat.Left > 0) and
    (VRectFloat.Top > 0) and
    (VRectFloat.Right < ABitmapSize.X) and
    (VRectFloat.Bottom < ABitmapSize.Y)
  then begin
    VRect.Left := Trunc(VRectFloat.Left);
    VRect.Top := Trunc(VRectFloat.Top);
    VRect.Right := Trunc(VRectFloat.Right);
    VRect.Bottom := Trunc(VRectFloat.Bottom);
    FLayer.Bitmap.FillRectS(VRect, ARectColor);
    if AFillColor <> ARectColor then begin
      Inc(VRect.Left);
      Inc(VRect.Top);
      Dec(VRect.Right);
      Dec(VRect.Bottom);
      FLayer.Bitmap.FillRectS(VRect, AFillColor);
    end;
  end;
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

function LoadColor32(
  AConfigProvider: IConfigDataProvider;
  AIdent: string;
  ADefault: TColor32
): TColor32;
var
  VColor: TColor;
  VAlfa: Integer;
begin
  Result := ADefault;
  if AConfigProvider <> nil then begin
    VAlfa := AlphaComponent(Result);
    VColor := WinColor(Result);
    VAlfa := AConfigProvider.ReadInteger(AIdent + 'Alfa', VAlfa);
    VColor := AConfigProvider.ReadInteger(AIdent, VColor);
    Result := SetAlpha(Color32(VColor), VAlfa);
  end;
end;

procedure TMapNalLayer.LoadConfig(AConfigProvider: IConfigDataProvider);
var
  VConfigProvider: IConfigDataProvider;
begin
  inherited;
  VConfigProvider := AConfigProvider.GetSubItem('VIEW');
  if VConfigProvider <> nil then begin
    VConfigProvider := VConfigProvider.GetSubItem('EditMark');
    if VConfigProvider <> nil then begin
      FEditMarkLineColor := LoadColor32(VConfigProvider, 'LineColor', FEditMarkLineColor);
      FEditMarkFillColor := LoadColor32(VConfigProvider, 'FillColor', FEditMarkFillColor);
      FEditMarkLineWidth := VConfigProvider.ReadInteger('LineWidth', FEditMarkLineWidth);
      FEditMarkPointColor := LoadColor32(VConfigProvider, 'PointColor', FEditMarkPointColor);
      FEditMarkActivePointColor := LoadColor32(VConfigProvider, 'ActivePointColor', FEditMarkActivePointColor);
      FEditMarkFirstPointColor := LoadColor32(VConfigProvider, 'FirstPointColor', FEditMarkFirstPointColor);
      FEditMarkPointSize := VConfigProvider.ReadInteger('PointSize', FEditMarkPointSize);
    end;
  end;
end;

procedure WriteColor32(
  AConfigProvider: IConfigDataWriteProvider;
  AIdent: string;
  AValue: TColor32
);
begin
  AConfigProvider.WriteInteger(AIdent + 'Alfa', AlphaComponent(AValue));
  AConfigProvider.WriteInteger(AIdent, WinColor(AValue));
end;

procedure TMapNalLayer.SaveConfig(AConfigProvider: IConfigDataWriteProvider);
var
  VConfigProvider: IConfigDataWriteProvider;
begin
  inherited;
  VConfigProvider := AConfigProvider.GetOrCreateSubItem('VIEW');
  VConfigProvider := VConfigProvider.GetOrCreateSubItem('EditMark');

  WriteColor32(VConfigProvider, 'LineColor', FEditMarkLineColor);
  WriteColor32(VConfigProvider, 'FillColor', FEditMarkFillColor);
  VConfigProvider.WriteInteger('LineWidth', FEditMarkLineWidth);
  WriteColor32(VConfigProvider, 'PointColor', FEditMarkPointColor);
  WriteColor32(VConfigProvider, 'ActivePointColor', FEditMarkActivePointColor);
  WriteColor32(VConfigProvider, 'FirstPointColor', FEditMarkFirstPointColor);
  VConfigProvider.WriteInteger('PointSize', FEditMarkPointSize);
end;

end.
