unit u_MapMarksBitmapLayerProviderByMarksSubset;

interface

uses
  Types,
  GR32,
  Graphics,
  WinTypes,
  t_GeoTypes,
  i_CoordConverter,
  i_LocalCoordConverter,
  i_IMarkPicture,
  i_MarksSimple,
  i_BitmapLayerProvider,
  u_ClipPolygonByRect;

type
  TMapMarksBitmapLayerProviderByMarksSubset = class(TInterfacedObject, IBitmapLayerProvider)
  private
    FMarksSubset: IMarksSubset;
    FDeltaSizeInPixel: TPoint;
    FBitmapClip: IPolygonClip;

    FTempBmp: TCustomBitmap32;
    FBitmapWithText: TBitmap32;
    FPathPointsOnBitmap: TArrayOfDoublePoint;
    FPathPointsOnBitmapPrepared: TArrayOfDoublePoint;
    FPathFixedPoints: TArrayOfFixedPoint;
    procedure DrawSubset(
      AMarksSubset: IMarksSubset;
      ATargetBmp: TCustomBitmap32;
      ALocalConverter: ILocalCoordConverter
    );
    procedure DrawPath(
      ATargetBmp: TCustomBitmap32;
      ALocalConverter: ILocalCoordConverter;
      APointsLonLat: TArrayOfDoublePoint;
      color1: TColor32;
      linew: integer
    );
    procedure DrawPoly(
      ATargetBmp: TCustomBitmap32;
      ALocalConverter: ILocalCoordConverter;
      APointsLonLat: TArrayOfDoublePoint;
      color1, color2: TColor32;
      linew: integer
    );
    procedure DrawPoint(
      ATargetBmp: TCustomBitmap32;
      ALocalConverter: ILocalCoordConverter;
      ALL: TDoublePoint;
      AName: string;
      APic: IMarkPicture;
      AMarkSize, AFontSize: integer;
      AColor1, AColor2: TColor32
    );
  protected
    procedure GetBitmapRect(
      ATargetBmp: TCustomBitmap32;
      ALocalConverter: ILocalCoordConverter
    );
  public
    constructor Create(AMarksSubset: IMarksSubset);
    destructor Destroy; override;
  end;

implementation

uses
  Classes,
  ActiveX,
  SysUtils,
  GR32_Resamplers,
  GR32_Polygons,
  Ugeofun;

const
  CMaxFontSize = 20;

{ TMapMarksBitmapLayerProviderByMarksSubset }

constructor TMapMarksBitmapLayerProviderByMarksSubset.Create(
  AMarksSubset: IMarksSubset);
begin
  FMarksSubset := AMarksSubset;
  FDeltaSizeInPixel := Point(128, 128);

  FTempBmp := TCustomBitmap32.Create;
  FTempBmp.DrawMode := dmBlend;
  FTempBmp.CombineMode := cmMerge;
  FTempBmp.Resampler := TLinearResampler.Create;

  FBitmapWithText := TBitmap32.Create;
  FBitmapWithText.Font.Name := 'Tahoma';
  FBitmapWithText.Font.Style := [];
  FBitmapWithText.DrawMode := dmBlend;
  FBitmapWithText.CombineMode := cmMerge;
  FBitmapWithText.Font.Size := CMaxFontSize;
  FBitmapWithText.Resampler := TLinearResampler.Create;
end;

destructor TMapMarksBitmapLayerProviderByMarksSubset.Destroy;
begin
  FreeAndNil(FTempBmp);
  FreeAndNil(FBitmapWithText);
  inherited;
end;

procedure TMapMarksBitmapLayerProviderByMarksSubset.DrawPath(
  ATargetBmp: TCustomBitmap32;
  ALocalConverter: ILocalCoordConverter;
  APointsLonLat: TArrayOfDoublePoint;
  color1: TColor32;
  linew: integer
);
var
  polygon: TPolygon32;
  i: Integer;
  VPointsCount: Integer;
  VPointsProcessedCount: Integer;
  VLonLat: TDoublePoint;
  VGeoConvert: ICoordConverter;
  VIndex: Integer;
begin
  VGeoConvert := ALocalConverter.GetGeoConverter;
  VPointsCount := Length(APointsLonLat);
  if VPointsCount > 0 then begin
    if Length(FPathPointsOnBitmap) < VPointsCount then begin
      SetLength(FPathPointsOnBitmap, VPointsCount);
    end;
    for i := 0 to VPointsCount - 1 do begin
      VLonLat := APointsLonLat[i];
      if PointIsEmpty(VLonLat) then begin
        FPathPointsOnBitmap[i] := VLonLat;
      end else begin
        VGeoConvert.CheckLonLatPos(VLonLat);
        FPathPointsOnBitmap[i] := ALocalConverter.LonLat2LocalPixelFloat(VLonLat);
      end;
    end;
    try
      VPointsProcessedCount := FBitmapClip.Clip(FPathPointsOnBitmap[0], VPointsCount, FPathPointsOnBitmapPrepared);
      if VPointsProcessedCount > 0 then begin
        polygon := TPolygon32.Create;
        try
          polygon.Antialiased := true;
          polygon.AntialiasMode := am4times;
          polygon.Closed := False;
          if Length(FPathFixedPoints) < VPointsProcessedCount then begin
            SetLength(FPathFixedPoints, VPointsProcessedCount);
          end;
          VIndex := 0;
          for i := 0 to VPointsProcessedCount - 1 do begin
            if PointIsEmpty(FPathPointsOnBitmapPrepared[i]) then begin
              polygon.AddPoints(FPathFixedPoints[0], VIndex);
              polygon.NewLine;
              VIndex := 0;
            end else begin
              FPathFixedPoints[VIndex] := FixedPoint(FPathPointsOnBitmapPrepared[i].X, FPathPointsOnBitmapPrepared[i].Y);
              Inc(VIndex);
            end;
          end;
          polygon.AddPoints(FPathFixedPoints[0], VIndex);
          with Polygon.Outline do try
            with Grow(GR32.Fixed(linew / 2), 0.5) do try
              FillMode := pfWinding;
              DrawFill(ATargetBmp, color1);
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
    except
    end;
  end;
end;

procedure TMapMarksBitmapLayerProviderByMarksSubset.DrawPoly(
  ATargetBmp: TCustomBitmap32;
  ALocalConverter: ILocalCoordConverter;
  APointsLonLat: TArrayOfDoublePoint;
  color1, color2: TColor32;
  linew: integer
);
var
  polygon: TPolygon32;
  i: Integer;
  VPointsCount: Integer;
  VPointsProcessedCount: Integer;
  VLonLat: TDoublePoint;
  VGeoConvert: ICoordConverter;
begin
  VGeoConvert := ALocalConverter.GetGeoConverter;
  VPointsCount := Length(APointsLonLat);
  if VPointsCount > 0 then begin
    if Length(FPathPointsOnBitmap) < VPointsCount then begin
      SetLength(FPathPointsOnBitmap, VPointsCount);
    end;
    for i := 0 to VPointsCount - 1 do begin
      VLonLat := APointsLonLat[i];
      VGeoConvert.CheckLonLatPos(VLonLat);
      FPathPointsOnBitmap[i] := ALocalConverter.LonLat2LocalPixelFloat(VLonLat);
    end;
    try
      VPointsProcessedCount := FBitmapClip.Clip(FPathPointsOnBitmap[0], VPointsCount, FPathPointsOnBitmapPrepared);
      if VPointsProcessedCount > 0 then begin
        polygon := TPolygon32.Create;
        try
          polygon.Antialiased := true;
          polygon.AntialiasMode := am4times;
          polygon.Closed := True;
            if Length(FPathFixedPoints) < VPointsProcessedCount then begin
              SetLength(FPathFixedPoints, VPointsProcessedCount);
            end;
            for i := 0 to VPointsProcessedCount - 1 do begin
              FPathFixedPoints[i] := FixedPoint(FPathPointsOnBitmapPrepared[i].X, FPathPointsOnBitmapPrepared[i].Y);
            end;
            polygon.AddPoints(FPathFixedPoints[0], VPointsProcessedCount);
            Polygon.DrawFill(ATargetBmp, color2);
            with Polygon.Outline do try
              with Grow(GR32.Fixed(linew / 2), 0.5) do try
                FillMode := pfWinding;
                DrawFill(ATargetBmp, color1);
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
    except
    end;
  end;
end;

procedure TMapMarksBitmapLayerProviderByMarksSubset.DrawPoint(
  ATargetBmp: TCustomBitmap32;
  ALocalConverter: ILocalCoordConverter;
  ALL: TDoublePoint;
  AName: string;
  APic: IMarkPicture;
  AMarkSize, AFontSize: integer;
  AColor1, AColor2: TColor32
);
var
  xy: Tpoint;
  VDstRect: TRect;
  VSrcRect: TRect;
  VTextSize: TSize;
begin
  xy := ALocalConverter.LonLat2LocalPixel(ALL);
  if (APic <> nil) then begin
    APic.LoadBitmap(FTempBmp);
    VDstRect := bounds(xy.x - (AMarkSize div 2), xy.y - AMarkSize, AMarkSize, AMarkSize);
    VSrcRect := bounds(0, 0, FTempBmp.Width, FTempBmp.Height);
    ATargetBmp.Draw(VDstRect, VSrcRect, FTempBmp);
  end;
  if AFontSize > 0 then begin
    FBitmapWithText.MasterAlpha:=AlphaComponent(AColor1);
    FBitmapWithText.Font.Size := AFontSize;
    VTextSize := FBitmapWithText.TextExtent(AName);
    VTextSize.cx:=VTextSize.cx+2;
    VTextSize.cy:=VTextSize.cy+2;
    FBitmapWithText.SetSize(VTextSize.cx + 2,VTextSize.cy + 2);
    VDstRect.Left := xy.x + (AMarkSize div 2);
    VDstRect.Top := xy.y - (AMarkSize div 2) - VTextSize.cy div 2;
    VDstRect.Right := VDstRect.Left + VTextSize.cx;
    VDstRect.Bottom := VDstRect.Top + VTextSize.cy;
    VSrcRect := bounds(1, 1, VTextSize.cx, VTextSize.cy);
    FBitmapWithText.Clear(clBlack);
    FBitmapWithText.RenderText(2, 2, AName, 1, SetAlpha(AColor2,255));
    FBitmapWithText.RenderText(1, 1, AName, 1, SetAlpha(AColor1,255));
    ATargetBmp.Draw(VDstRect, VSrcRect, FBitmapWithText);
  end;
end;

procedure TMapMarksBitmapLayerProviderByMarksSubset.DrawSubset(
  AMarksSubset: IMarksSubset; ATargetBmp: TCustomBitmap32;
  ALocalConverter: ILocalCoordConverter);
var
  VEnumMarks: IEnumUnknown;
  VMark: IMarkFull;
  i: Cardinal;
  VScale1: Integer;
  VPointCount: Integer;
  TestArrLenLonLatRect: TDoubleRect;
  TestArrLenPixelRect: TDoubleRect;
  VOldClipRect: TRect;
begin
  VOldClipRect := ATargetBmp.ClipRect;
  ATargetBmp.ClipRect := ALocalConverter.GetLocalRect;
  try
    VEnumMarks := AMarksSubset.GetEnum;
    while (VEnumMarks.Next(1, VMark, @i) = S_OK) do begin
      VScale1 := VMark.Scale1;
      VPointCount := length(VMark.Points);
      if VPointCount > 1 then begin
        TestArrLenLonLatRect := VMark.LLRect;
        TestArrLenPixelRect := ALocalConverter.LonLatRect2LocalRectFloat(TestArrLenLonLatRect);
        if (abs(TestArrLenPixelRect.Left - TestArrLenPixelRect.Right) > VScale1 + 2) or (abs(TestArrLenPixelRect.Top - TestArrLenPixelRect.Bottom) > VScale1 + 2) then begin
          if VMark.IsPoly then begin
            DrawPoly(
              ATargetBmp,
              ALocalConverter,
              VMark.Points,
              VMark.Color1,
              VMark.Color2,
              VMark.Scale1
            );
          end else begin
            drawPath(
              ATargetBmp,
              ALocalConverter,
              VMark.Points,
              VMark.Color1,
              VMark.Scale1
            );
          end;
        end;
      end else if VPointCount = 1 then begin
        DrawPoint(
          ATargetBmp,
          ALocalConverter,
          VMark.Points[0],
          VMark.name,
          VMark.Pic,
          VMark.Scale2,
          VMark.Scale1,
          VMark.Color1,
          VMark.Color2
        );
      end;
    end;
  finally
    ATargetBmp.ClipRect := VOldClipRect;
  end;
end;

procedure TMapMarksBitmapLayerProviderByMarksSubset.GetBitmapRect(
  ATargetBmp: TCustomBitmap32; ALocalConverter: ILocalCoordConverter);
var
  VRectWithDelta: TRect;
  VLocalRect: TRect;
  VTargetRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VMarksSubset: IMarksSubset;
begin
  VLocalRect := ALocalConverter.GetLocalRect;
  VRectWithDelta.Left := VLocalRect.Left - FDeltaSizeInPixel.X;
  VRectWithDelta.Top := VLocalRect.Top - FDeltaSizeInPixel.Y;
  VRectWithDelta.Right := VLocalRect.Right + FDeltaSizeInPixel.X;
  VRectWithDelta.Bottom := VLocalRect.Bottom + FDeltaSizeInPixel.Y;
  VTargetRect := ALocalConverter.LocalRect2MapRectFloat(VRectWithDelta);
  VZoom := ALocalConverter.GetZoom;
  VConverter := ALocalConverter.GetGeoConverter;
  VConverter.CheckPixelRectFloat(VTargetRect, VZoom);
  VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VTargetRect, VZoom);
  VMarksSubset := FMarksSubset.GetSubsetByLonLatRect(VLonLatRect);

  FBitmapClip := TPolygonClipByRect.Create(VRectWithDelta);
  DrawSubset(VMarksSubset, ATargetBmp, ALocalConverter);
  FBitmapClip := nil;
end;

end.

