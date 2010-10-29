unit u_MapMarksLayer;

interface

uses
  GR32,
  GR32_Image,
  GR32_Polygons,
  t_GeoTypes,
  u_MapViewPortState,
  u_MapLayerBasic;

type
  TMapMarksLayer = class(TMapLayerBasic)
  protected
    FLLRect: TExtendedRect;
    FTempBmp: TCustomBitmap32;
{$IFDEF USE_VPR}
    FFixedPointArray: TArrayOfFloatPoint;
    procedure PreparePolygonNew(pathll: TExtendedPointArray; poly: Boolean; var ATargetPointsCount: Integer);
    procedure drawPathNew(pathll: TExtendedPointArray; color1, color2: TColor32; linew: integer; poly: boolean);
{$ENDIF}
    procedure PreparePolygon(pathll: TExtendedPointArray; polygon: TPolygon32);
    procedure drawPath(pathll: TExtendedPointArray; color1, color2: TColor32; linew: integer; poly: boolean);
    procedure DrawPoint(ALL: TExtendedPoint; AName: string; APicName: string; AMarkSize, AFontSize: integer; AColor1, AColor2: TColor32);
    procedure DrawMarks;
    procedure DoRedraw; override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    destructor Destroy; override;
  end;

implementation

uses
  Types,
  Graphics,
  Classes,
  SysUtils,
  GR32_Resamplers,
{$IFDEF USE_VPR}
  GR32_PolygonsEx,
  GR32_VPR,
  GR32_VectorUtils,
{$ENDIF}
  t_CommonTypes,
  u_GlobalState,
  i_IBitmapLayerProvider,
  Ugeofun,
  u_MarksSimple,
  u_MarksReadWriteSimple,
  u_WindowLayerBasic;

{ TMapMarksLayer }

constructor TMapMarksLayer.Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
begin
  inherited;
  FLayer.Bitmap.Font.Name := 'Tahoma';
  FLayer.Bitmap.Font.Style := [];
  FTempBmp := TCustomBitmap32.Create;
  FTempBmp.DrawMode := dmBlend;
  FTempBmp.Resampler := TLinearResampler.Create;
end;

procedure TMapMarksLayer.PreparePolygon(pathll: TExtendedPointArray;
  polygon: TPolygon32);
var
  i, adp, j: integer;
  k1: TextendedPoint;
  k2: TextendedPoint;
  k4: TextendedPoint;
  k3: TextendedPoint;
  VLonLat: TExtendedPoint;
begin
  for i := 0 to length(pathll) - 1 do begin
    VLonLat := pathll[i];
    FGeoConvert.CheckLonLatPos(VLonLat);
    k1 := FGeoConvert.LonLat2PixelPosFloat(VLonLat, FZoom);
    k1 := MapPixel2BitmapPixel(k1);
    if (k1.x < 32767) and (k1.x > -32767) and (k1.y < 32767) and (k1.y > -32767) then begin
      polygon.Add(FixedPoint(k1.X, k1.Y));
    end;
    if i < length(pathll) - 1 then begin
      VLonLat := pathll[i + 1];
      FGeoConvert.CheckLonLatPos(VLonLat);
      k2 := FGeoConvert.LonLat2PixelPosFloat(VLonLat, FZoom);
      k2 := MapPixel2BitmapPixel(k2);
      if (k2.x - k1.x) > (k2.y - k1.y) then begin
        adp := Trunc((k2.x - k1.x) / 32767) + 2;
      end else begin
        adp := Trunc((k2.y - k1.y) / 32767) + 2;
      end;
      k3 := extPoint(((k2.X - k1.x) / adp), ((k2.y - k1.y) / adp));
      if adp > 2 then begin
        for j := 1 to adp - 1 do begin
          k4 := extPoint((k1.x + k3.x * j), (k1.Y + k3.y * j));
          if (k4.x < 32767) and (k4.x > -32767) and (k4.y < 32767) and (k4.y > -32767) then begin
            polygon.Add(FixedPoint(k4.x, k4.y));
          end;
        end;
      end;
    end;
  end;
end;

procedure TMapMarksLayer.drawPath(pathll: TExtendedPointArray; color1, color2: TColor32; linew: integer; poly: boolean);
var
  polygon: TPolygon32;
begin
  try
    polygon := TPolygon32.Create;
    try
      polygon.Antialiased := true;
      polygon.AntialiasMode := am4times;
      polygon.Closed := poly;
      if length(pathll) > 0 then begin
        PreparePolygon(pathll, polygon);
        if poly then begin
          Polygon.DrawFill(FLayer.Bitmap, color2);
        end;
        with Polygon.Outline do try
          with Grow(Fixed(linew / 2), 0.5) do try
            FillMode := pfWinding;
            DrawFill(FLayer.Bitmap, color1);
          finally
            free;
          end;
        finally
          free;
        end;
      end;
    finally
      polygon.Free;
    end;
  except
  end;
end;

{$IFDEF USE_VPR}
procedure TMapMarksLayer.PreparePolygonNew(pathll: TExtendedPointArray;
  poly: Boolean; var ATargetPointsCount: Integer);
var
  i, j: integer;
  VLonLat: TExtendedPoint;
  VMapPoint: TExtendedPoint;
  VBitmapPointCurr: TExtendedPoint;
  VBitmapPointPrev: TExtendedPoint;
  VPointsCount: Integer;
  VDist: Extended;
  VArrayLen: integer;
  VDelta: TExtendedPoint;
  VMaxDelta: Extended;
  VSplitCount: Integer;
  VStepDelta: TExtendedPoint;
  VTempPoint: TExtendedPoint;
const
  CRectSize = 1 shl 14;
begin
  VPointsCount := length(pathll);
  VArrayLen := Length(FFixedPointArray);
  if VArrayLen < VPointsCount then begin
    SetLength(FFixedPointArray, VPointsCount);
    VArrayLen := VPointsCount;
  end;
  if poly then begin
    VLonLat := pathll[VPointsCount - 1];
  end else begin
    VLonLat := pathll[0];
  end;
  FGeoConvert.CheckLonLatPos(VLonLat);
  VMapPoint := FGeoConvert.LonLat2PixelPosFloat(VLonLat, FZoom);
  VBitmapPointPrev := MapPixel2BitmapPixel(VMapPoint);

  ATargetPointsCount := 0;
  for i := 0 to VPointsCount - 1 do begin
    VLonLat := pathll[i];
    FGeoConvert.CheckLonLatPos(VLonLat);
    VMapPoint := FGeoConvert.LonLat2PixelPosFloat(VLonLat, FZoom);
    VBitmapPointCurr := MapPixel2BitmapPixel(VMapPoint);
    VDelta.X := VBitmapPointCurr.X - VBitmapPointPrev.X;
    VDelta.Y := VBitmapPointCurr.Y - VBitmapPointPrev.Y;
    VDist := Abs(VDelta.X) + Abs(VDelta.Y);
    if VDist > 1.0 then begin
      if Abs(VDelta.X) > Abs(VDelta.Y) then begin
        VMaxDelta := VDelta.X;
      end else begin
        VMaxDelta := VDelta.Y;
      end;
      VSplitCount := Trunc(Abs(VMaxDelta) / CRectSize) + 1;
      if VSplitCount > 1 then begin
        VStepDelta.X := VDelta.X / VSplitCount;
        VStepDelta.Y := VDelta.Y / VSplitCount;
        VTempPoint := VBitmapPointPrev;
        for j := 0 to VSplitCount - 1 do begin
          VTempPoint.X := VTempPoint.X + VStepDelta.X;
          VTempPoint.X := VTempPoint.X + VStepDelta.X;
          if (VTempPoint.x < CRectSize) and (VTempPoint.x > -CRectSize) and
            (VTempPoint.y < CRectSize) and (VTempPoint.y > -CRectSize) then begin
            if ATargetPointsCount >= VArrayLen then begin
              SetLength(FFixedPointArray, ATargetPointsCount * 2);
              VArrayLen := ATargetPointsCount * 2;
            end;
            FFixedPointArray[ATargetPointsCount] := FloatPoint(VTempPoint.x, VTempPoint.y);
            Inc(ATargetPointsCount);
          end;
        end;
      end;
      if
        (VBitmapPointCurr.x<CRectSize)and(VBitmapPointCurr.x>-CRectSize)and
        (VBitmapPointCurr.y<CRectSize)and(VBitmapPointCurr.y>-CRectSize)
      then begin
        if ATargetPointsCount >= VArrayLen then begin
          SetLength(FFixedPointArray, ATargetPointsCount * 2);
          VArrayLen := ATargetPointsCount * 2;
        end;
        FFixedPointArray[ATargetPointsCount] := FloatPoint(VBitmapPointCurr.x, VBitmapPointCurr.y);
        Inc(ATargetPointsCount);
      end;
      VBitmapPointPrev := VBitmapPointCurr;
    end;
  end;
  SetLength(FFixedPointArray, ATargetPointsCount);
end;

procedure TMapMarksLayer.drawPathNew(pathll: TExtendedPointArray; color1, color2: TColor32; linew: integer; poly: boolean);
var
  VTargetPointsCount: Integer;
  VPolyline: TArrayOfFloatPoint;
begin
  try
    try
      if length(pathll) > 0 then begin
        PreparePolygonNew(pathll, poly, VTargetPointsCount);
        if poly then begin
          PolygonFS(FLayer.Bitmap, FFixedPointArray, color2, pfWinding);
        end;
        PolylineFS(FLayer.Bitmap, FFixedPointArray, color1, poly, linew * 2, jsBevel);
      end;
    finally
    end;
  except
  end;
end;

{$ENDIF}

procedure TMapMarksLayer.DrawPoint(ALL: TExtendedPoint; AName, APicName: string;
  AMarkSize, AFontSize: integer; AColor1, AColor2: TColor32);
var
  xy: Tpoint;
  indexmi: integer;
  texth: integer;
  VIconSource: TCustomBitmap32;
begin
  xy := FGeoConvert.LonLat2PixelPos(ALL, FZoom);
  xy := MapPixel2BitmapPixel(xy);
  indexmi := GState.MarkIcons.IndexOf(APicName);
  if (indexmi = -1) and (GState.MarkIcons.Count > 0) then begin
    indexmi := 0;
  end;
  if (indexmi > -1) then begin
    VIconSource := TCustomBitmap32(GState.MarkIcons.Objects[indexmi]);
    FTempBmp.SetSize(VIconSource.Width, VIconSource.Height);
    FTempBmp.Draw(0, 0, VIconSource);
    FLayer.Bitmap.Draw(bounds(xy.x - (AMarkSize div 2), xy.y - AMarkSize, AMarkSize, AMarkSize), bounds(0, 0, FTempBmp.Width, FTempBmp.Height), FTempBmp);
  end;
  if AFontSize > 0 then begin
    FLayer.Bitmap.Font.Size := AFontSize;
    texth := FLayer.Bitmap.TextHeight(AName) div 2;
    FLayer.Bitmap.RenderText(xy.x + (AMarkSize div 2) + 2, xy.y - (AMarkSize div 2) - texth + 1, AName, 1, AColor2);
    FLayer.Bitmap.RenderText(xy.x + (AMarkSize div 2) + 1, xy.y - (AMarkSize div 2) - texth, AName, 1, AColor1);
  end;
end;

procedure TMapMarksLayer.DrawMarks;
var
  TestArrLenLonLatRect: TExtendedRect;
  TestArrLenPixelRect: TExtendedRect;
  VScale1: Integer;
  VPointCount: Integer;
  VMarksIterator: TMarksIteratorVisibleInRectIgnoreEdit;
  VMark: TMarkFull;
begin
  VMarksIterator := TMarksIteratorVisibleInRectIgnoreEdit.Create(FZoom, FLLRect, GState.show_point);
  try
    While VMarksIterator.Next do begin
      VMark := VMarksIterator.Current;
      VScale1 := VMark.Scale1;
      VPointCount := length(VMark.Points);
      if VPointCount > 1 then begin
        TestArrLenLonLatRect := VMark.LLRect;
        FGeoConvert.CheckLonLatRect(TestArrLenLonLatRect);
        TestArrLenPixelRect := FGeoConvert.LonLatRect2PixelRectFloat(TestArrLenLonLatRect, FZoom);
        if (abs(TestArrLenPixelRect.Left - TestArrLenPixelRect.Right) > VScale1 + 2) or (abs(TestArrLenPixelRect.Top - TestArrLenPixelRect.Bottom) > VScale1 + 2) then begin
{$IFDEF USE_VPR}
          drawPathNew(
            VMark.Points,
            VMark.Color1,
            VMark.Color2,
            VMark.Scale1,
            VMark.IsPoly
            );
{$ELSE}
          drawPath(
            VMark.Points,
            VMark.Color1,
            VMark.Color2,
            VMark.Scale1,
            VMark.IsPoly
            );
{$ENDIF}
        end;
      end else if VPointCount = 1 then begin
        DrawPoint(
          VMark.Points[0],
          VMark.name,
          VMark.PicName,
          VMark.Scale2,
          VMark.Scale1,
          VMark.Color1,
          VMark.Color2
          );
      end;
    end;
  finally
    VMarksIterator.Free;
  end;
end;

destructor TMapMarksLayer.Destroy;
begin
  FreeAndNil(FTempBmp);
  inherited;
end;

procedure TMapMarksLayer.DoRedraw;
var
  VBitmapSize: TPoint;
  VRect: TRect;
  VProv: IBitmapLayerProvider;
begin
  inherited;
  if (GState.show_point <> mshNone) then begin
    VProv := GState.MarksBitmapProvider;
    FLayer.Bitmap.DrawMode:=dmBlend;
    FLayer.Bitmap.CombineMode:=cmMerge;
    FLayer.Bitmap.Clear(clBlack);
    VBitmapSize := GetBitmapSizeInPixel;
    VRect.TopLeft := BitmapPixel2MapPixel(Point(0, 0));
    VRect.BottomRight := BitmapPixel2MapPixel(VBitmapSize);
    VProv.GetBitmapRect(FLayer.Bitmap, FGeoConvert, VRect, FZoom);

//    FGeoConvert.CheckPixelRect(VRect, FZoom, false);
//    FLLRect := FGeoConvert.PixelRect2LonLatRect(VRect, FZoom);
//    DrawMarks;
  end;
end;

end.
