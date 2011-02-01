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
  i_IViewPortState,
  u_ClipPolygonByRect,
  u_MapLayerBasic;

type
  TMapNalDrawType = (mndtNothing, mndtSelectRect, mndtSelectPoly, mndtCalcLen, mndtNewPath, mndtNewPoly);

  TMapNalLayer = class(TMapLayerBasic)
  private
    FBitmapClip: IPolyClip;
    FDrawType: TMapNalDrawType;
    FPath: TDoublePointArray;
    FSelectedLonLat: TDoubleRect;
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
      const APosOnBitmap: TDoublePoint;
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
  public
    constructor Create(AParentMap: TImage32; AViewPortState: IViewPortState);
    destructor Destroy; override;
    procedure LoadConfig(AConfigProvider: IConfigDataProvider); override;
    procedure SaveConfig(AConfigProvider: IConfigDataWriteProvider); override;
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
  i_IDatum,
  i_ICoordConverter,
  i_ILocalCoordConverter,
  i_IValueToStringConverter,
  u_ConfigProviderHelpers,
  UResStrings,
  u_GlobalState;

{ TMapNalLayer }

constructor TMapNalLayer.Create(AParentMap: TImage32; AViewPortState: IViewPortState);
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
  k1: TDoublePoint;
  len: Double;
  text: string;
  polygon: TPolygon32;
  VLonLat: TDoublePoint;
  VBitmapSize: TPoint;
  VPointsOnBitmap: TDoublePointArray;
  VPointsCount: Integer;
  VPointsOnBitmapPrepared: TDoublePointArray;
  VPointsProcessedCount: Integer;
  VPathFixedPoints: TArrayOfFixedPoint;
  VLocalConverter: ILocalCoordConverter;
  VGeoConvert: ICoordConverter;
  VValueConverter: IValueToStringConverter;
  VDatum: IDatum;
begin
  VPointsCount := Length(FPath);
  if VPointsCount > 0 then begin
    VValueConverter := GState.ValueToStringConverterConfig.GetStaticConverter;
    VLocalConverter := BitmapCoordConverter;
    VGeoConvert := VLocalConverter.GetGeoConverter;
    VDatum := VGeoConvert.Datum;
    SetLength(VPointsOnBitmap, VPointsCount);
    for i := 0 to VPointsCount - 1 do begin
      VLonLat := FPath[i];
      VGeoConvert.CheckLonLatPos(VLonLat);
      VPointsOnBitmap[i] := VLocalConverter.LonLat2LocalPixelFloat(VLonLat);
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
          polygon.Closed := false;
          polygon.AddPoints(VPathFixedPoints[0], VPointsProcessedCount);
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
      end;
    end;
    VBitmapSize := VLocalConverter.GetLocalRectSize;
    for i := 0 to VPointsCount - 2 do begin
      k1 := VPointsOnBitmap[i + 1];
      if ((k1.x > 0) and (k1.y > 0)) and ((k1.x < VBitmapSize.X) and (k1.y < VBitmapSize.Y)) then begin
        if i = VPointsCount - 2 then begin
          len := 0;
          for j := 0 to i do begin
            len := len + VDatum.CalcDist(FPath[j], FPath[j + 1]);
          end;
          text := SAS_STR_Whole + ': ' + VValueConverter.DistConvert(len);
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
            text := VValueConverter.DistConvert(VDatum.CalcDist(FPath[i], FPath[i + 1]));
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
  end;
end;

procedure TMapNalLayer.DoDrawNewPath(AIsPoly: Boolean);
var
  i: integer;
  k1: TDoublePoint;
  polygon: TPolygon32;
  VLonLat: TDoublePoint;
  VBitmapSize: TPoint;
  VPointsOnBitmap: TDoublePointArray;
  VPointsCount: Integer;
  VPointsOnBitmapPrepared: TDoublePointArray;
  VPointsProcessedCount: Integer;
  VPathFixedPoints: TArrayOfFixedPoint;
  VLocalConverter: ILocalCoordConverter;
  VGeoConvert: ICoordConverter;
begin
  VPointsCount := Length(FPath);
  if VPointsCount > 0 then begin
    VLocalConverter := BitmapCoordConverter;
    VGeoConvert := VLocalConverter.GetGeoConverter;
    SetLength(VPointsOnBitmap, VPointsCount + 1);
    for i := 0 to VPointsCount - 1 do begin
      VLonLat := FPath[i];
      VGeoConvert.CheckLonLatPos(VLonLat);
      VPointsOnBitmap[i] := VLocalConverter.LonLat2LocalPixelFloat(VLonLat);
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
      VBitmapSize := VLocalConverter.GetLocalRectSize;
      for i := 1 to VPointsProcessedCount - 1 do begin
        k1 := VPointsOnBitmapPrepared[i];
        DrawPolyPoint(VBitmapSize, k1, FEditMarkPointSize, FEditMarkPointColor, FEditMarkPointColor);
      end;
      k1 := VPointsOnBitmap[0];
      DrawPolyPoint(VBitmapSize, k1, FEditMarkPointSize, FEditMarkFirstPointColor, FEditMarkFirstPointColor);
      k1 := VPointsOnBitmap[FPolyActivePointIndex];
      DrawPolyPoint(VBitmapSize, k1, FEditMarkPointSize, FEditMarkActivePointColor, FEditMarkActivePointColor);
    end;
  end;
end;

procedure TMapNalLayer.DoDrawSelectionPoly;
var
  i: integer;
  k1: TDoublePoint;
  Polygon: TPolygon32;
  VLonLat: TDoublePoint;
  VBitmapSize: TPoint;
  VPointsOnBitmap: TDoublePointArray;
  VPointsCount: Integer;
  VPointsOnBitmapPrepared: TDoublePointArray;
  VPointsProcessedCount: Integer;
  VPathFixedPoints: TArrayOfFixedPoint;
  VLocalConverter: ILocalCoordConverter;
  VGeoConvert: ICoordConverter;
begin
  VPointsCount := Length(FPath);
  if VPointsCount > 0 then begin
    VLocalConverter := BitmapCoordConverter;
    VGeoConvert := VLocalConverter.GetGeoConverter;
    SetLength(VPointsOnBitmap, VPointsCount);
    for i := 0 to VPointsCount - 1 do begin
      VLonLat := FPath[i];
      VGeoConvert.CheckLonLatPos(VLonLat);
      VPointsOnBitmap[i] := VLocalConverter.LonLat2LocalPixelFloat(VLonLat);
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
          polygon.Closed := true;
          polygon.AddPoints(VPathFixedPoints[0], VPointsProcessedCount);
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
      end;
      VBitmapSize := VLocalConverter.GetLocalRectSize;
      k1 := VPointsOnBitmap[0];
      DrawPolyPoint(VBitmapSize, k1, FSelectionPolyPointSize, FSelectionPolyPointFirstColor, FSelectionPolyPointFirstColor);
      if VPointsCount > 1 then begin
        k1 := VPointsOnBitmap[VPointsCount - 1];
        DrawPolyPoint(VBitmapSize, k1, FSelectionPolyPointSize, FSelectionPolyPointLastColor, FSelectionPolyPointLastColor);
      end;
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
  VSelectedRelative: TDoubleRect;
  VSelectedTiles: TRect;
  VMaxZoomDelta: Integer;
  VLocalConverter: ILocalCoordConverter;
  VGeoConvert: ICoordConverter;
  VZoom: Byte;
begin
  VLocalConverter := BitmapCoordConverter;
  VGeoConvert := VLocalConverter.GetGeoConverter;
  VZoom := VLocalConverter.GetZoom;
  VSelectedPixels := VGeoConvert.LonLatRect2PixelRect(FSelectedLonLat, VZoom);

  xy1 := VLocalConverter.LonLat2LocalPixel(FSelectedLonLat.TopLeft);
  xy2 := VLocalConverter.LonLat2LocalPixel(FSelectedLonLat.BottomRight);

  FLayer.Bitmap.FillRectS(xy1.x, xy1.y, xy2.x, xy2.y, FSelectionRectFillColor);
  FLayer.Bitmap.FrameRectS(xy1.x, xy1.y, xy2.x, xy2.y, FSelectionRectBorderColor);
  FLayer.Bitmap.FrameRectS(xy1.x - 1, xy1.y - 1, xy2.x + 1, xy2.y + 1, FSelectionRectBorderColor);

  VSelectedRelative := VGeoConvert.PixelRect2RelativeRect(VSelectedPixels, VZoom);

  jj := VZoom;
  VZoomDelta := 0;
  VMaxZoomDelta := Length(FSelectionRectZoomDeltaColor) - 1;
  while (VZoomDelta <= VMaxZoomDelta) and (jj < 24) do begin
    VSelectedTiles := VGeoConvert.RelativeRect2TileRect(VSelectedRelative, jj);
    VSelectedPixels := VGeoConvert.RelativeRect2PixelRect(
      VGeoConvert.TileRect2RelativeRect(VSelectedTiles, jj), VZoom
    );

    xy1 := VLocalConverter.MapPixel2LocalPixel(VSelectedPixels.TopLeft);
    xy2 := VLocalConverter.MapPixel2LocalPixel(VSelectedPixels.BottomRight);

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
  if FDrawType <> mndtNothing then begin
    FBitmapClip := TPolyClipByRect.Create(BitmapCoordConverter.GetLocalRect);
  end;
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
  Visible := True;
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
  Visible := True;
  Redraw;
end;

procedure TMapNalLayer.DrawNothing;
begin
  FDrawType := mndtNothing;
  FPath := nil;
  Visible := False;
  Redraw;
end;

procedure TMapNalLayer.DrawPolyPoint(const ABitmapSize: TPoint;
  const APosOnBitmap: TDoublePoint; const ASize: Integer; const AFillColor,
  ARectColor: TColor32);
var
  VHalfSize: Double;
  VRect: TRect;
  VRectFloat: TDoubleRect;
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

procedure TMapNalLayer.DrawReg(ASelectedLonLatPoly: TDoublePointArray);
begin
  FDrawType := mndtSelectPoly;
  FPath := Copy(ASelectedLonLatPoly);
  Visible := True;
  Redraw;
end;

procedure TMapNalLayer.DrawSelectionRect(ASelectedLonLat: TDoubleRect);
begin
  FDrawType := mndtSelectRect;
  FPath := nil;
  FSelectedLonLat := ASelectedLonLat;
  Visible := True;
  Redraw;
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
