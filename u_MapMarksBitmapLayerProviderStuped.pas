unit u_MapMarksBitmapLayerProviderStuped;

interface

uses
  Types,
  GR32,
  graphics,
  i_ICoordConverter,
  i_IMarkPicture,
  i_IBitmapLayerProvider,
  WinTypes;

type
  TMapMarksBitmapLayerProviderStuped = class(TInterfacedObject, IBitmapLayerProvider)
  private
    procedure GetBitmapRect(
      ATargetBmp: TCustomBitmap32;
      AConverter: ICoordConverter;
      ATargetRect: TRect;
      ATargetZoom: Byte
    );
  public
  end;

implementation

uses
  Classes,
  SysUtils,
  GR32_Resamplers,
  GR32_Polygons,
  t_GeoTypes,
  t_CommonTypes,
  u_GlobalState,
  u_ClipPolygonByRect,
  u_MarksSimple,
  u_MarksReadWriteSimple;

const
  CMaxFontSize = 20;

{ TMapMarksBitmapLayerProviderStupedThreaded }

type
  TMapMarksBitmapLayerProviderStupedThreaded = class
  private
    FBitmapClip: IPolyClip;
    FPathPointsOnBitmap: TDoublePointArray;
    FPathPointsOnBitmapPrepared: TDoublePointArray;
    FPathFixedPoints: TArrayOfFixedPoint;
    FDeltaSizeInPixel: TPoint;
    FTargetBmp: TCustomBitmap32;
    FGeoConvert: ICoordConverter;
    FTargetRect: TRect;
    FZoom: Byte;
    FLLRect: TDoubleRect;
    FTempBmp: TCustomBitmap32;
    FBitmapWithText: TBitmap32;
    function MapPixel2BitmapPixel(Pnt: TPoint): TPoint; overload; virtual;
    function MapPixel2BitmapPixel(Pnt: TDoublePoint): TDoublePoint; overload; virtual;
    procedure drawPath(APointsLonLat: TDoublePointArray; color1, color2: TColor32; linew: integer; poly: boolean);
    procedure DrawPoint(ALL: TDoublePoint; AName: string; APic: IMarkPicture; AMarkSize, AFontSize: integer; AColor1, AColor2: TColor32);
  public
    constructor Create(
      ATargetBmp: TCustomBitmap32;
      AConverter: ICoordConverter;
      ATargetRect: TRect;
      ATargetZoom: Byte
    );
    destructor Destroy; override;
    procedure SyncGetBitmap;
  end;

constructor TMapMarksBitmapLayerProviderStupedThreaded.Create(
  ATargetBmp: TCustomBitmap32; AConverter: ICoordConverter;
  ATargetRect: TRect; ATargetZoom: Byte);
var
  VRectWithDelta: TRect;
begin
  FDeltaSizeInPixel := Point(128, 128);
  FTargetBmp := ATargetBmp;
  FGeoConvert := AConverter;
  FTargetRect := ATargetRect;
  FZoom := ATargetZoom;
  VRectWithDelta.Left := FTargetRect.Left - FDeltaSizeInPixel.X;
  VRectWithDelta.Top := FTargetRect.Top - FDeltaSizeInPixel.Y;
  VRectWithDelta.Right := FTargetRect.Right + FDeltaSizeInPixel.X;
  VRectWithDelta.Bottom := FTargetRect.Bottom + FDeltaSizeInPixel.Y;
  FGeoConvert.CheckPixelRect(VRectWithDelta, FZoom);
  FLLRect := FGeoConvert.PixelRect2LonLatRect(VRectWithDelta, FZoom);

  FTempBmp := TCustomBitmap32.Create;
  FTempBmp.DrawMode := dmBlend;
  FTempBmp.Resampler := TLinearResampler.Create;

  FBitmapWithText := TBitmap32.Create;
  FBitmapWithText.Font.Name := 'Tahoma';
  FBitmapWithText.Font.Style := [];
  FBitmapWithText.DrawMode := dmBlend;
  FBitmapWithText.CombineMode := cmMerge;
  FBitmapWithText.Font.Size := CMaxFontSize;
  FBitmapWithText.Resampler := TLinearResampler.Create;
  FBitmapClip := TPolyClipByRect.Create(MakeRect(-10, -10, FTargetBmp.Width + 10, FTargetBmp.Height + 10));
end;

function TMapMarksBitmapLayerProviderStupedThreaded.MapPixel2BitmapPixel(
  Pnt: TPoint): TPoint;
begin
  Result.X := Pnt.X - FTargetRect.Left;
  Result.Y := Pnt.Y - FTargetRect.Top;
end;

function TMapMarksBitmapLayerProviderStupedThreaded.MapPixel2BitmapPixel(
  Pnt: TDoublePoint): TDoublePoint;
begin
  Result.X := Pnt.X - FTargetRect.Left;
  Result.Y := Pnt.Y - FTargetRect.Top;
end;

destructor TMapMarksBitmapLayerProviderStupedThreaded.Destroy;
begin
  FreeAndNil(FTempBmp);
  FreeAndNil(FBitmapWithText);
  inherited;
end;

procedure TMapMarksBitmapLayerProviderStupedThreaded.drawPath(
  APointsLonLat: TDoublePointArray; color1, color2: TColor32; linew: integer;
  poly: boolean);
var
  polygon: TPolygon32;
  i: Integer;
  VPointsCount: Integer;
  VPointsProcessedCount: Integer;
  VLonLat: TDoublePoint;
begin
  VPointsCount := Length(APointsLonLat);
  if VPointsCount > 0 then begin
    if Length(FPathPointsOnBitmap) < VPointsCount then begin
      SetLength(FPathPointsOnBitmap, VPointsCount);
    end;
    for i := 0 to VPointsCount - 1 do begin
      VLonLat := APointsLonLat[i];
      FGeoConvert.CheckLonLatPos(VLonLat);
      FPathPointsOnBitmap[i] := MapPixel2BitmapPixel(FGeoConvert.LonLat2PixelPosFloat(VLonLat, FZoom));
    end;
    try
      VPointsProcessedCount := FBitmapClip.Clip(FPathPointsOnBitmap, VPointsCount, FPathPointsOnBitmapPrepared);
      if VPointsProcessedCount > 0 then begin
        polygon := TPolygon32.Create;
        try
          polygon.Antialiased := true;
          polygon.AntialiasMode := am4times;
          polygon.Closed := poly;
            if Length(FPathFixedPoints) < VPointsProcessedCount then begin
              SetLength(FPathFixedPoints, VPointsProcessedCount);
            end;
            for i := 0 to VPointsProcessedCount - 1 do begin
              FPathFixedPoints[i] := FixedPoint(FPathPointsOnBitmapPrepared[i].X, FPathPointsOnBitmapPrepared[i].Y);
            end;
            polygon.AddPoints(FPathFixedPoints[0], VPointsProcessedCount);
            if poly then begin
              Polygon.DrawFill(FTargetBmp, color2);
            end;
            with Polygon.Outline do try
              with Grow(GR32.Fixed(linew / 2), 0.5) do try
                FillMode := pfWinding;
                DrawFill(FTargetBmp, color1);
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

procedure TMapMarksBitmapLayerProviderStupedThreaded.DrawPoint(
  ALL: TDoublePoint; AName: string; APic: IMarkPicture; AMarkSize, AFontSize: integer;
  AColor1, AColor2: TColor32);
var
  xy: Tpoint;
  indexmi: integer;
  VIconSource: TCustomBitmap32;
  VDstRect: TRect;
  VSrcRect: TRect;
  VTextSize: TSize;
begin
  xy := FGeoConvert.LonLat2PixelPos(ALL, FZoom);
  xy := MapPixel2BitmapPixel(xy);
  if (APic <> nil) then begin
    APic.LoadBitmap(FTempBmp);
    VDstRect := bounds(xy.x - (AMarkSize div 2), xy.y - AMarkSize, AMarkSize, AMarkSize);
    VSrcRect := bounds(0, 0, FTempBmp.Width, FTempBmp.Height);
    FTargetBmp.Draw(VDstRect, VSrcRect, FTempBmp);
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
    FTargetBmp.Draw(VDstRect, VSrcRect, FBitmapWithText);
  end;
end;

procedure TMapMarksBitmapLayerProviderStupedThreaded.SyncGetBitmap;
var
  TestArrLenLonLatRect: TDoubleRect;
  TestArrLenPixelRect: TDoubleRect;
  VScale1: Integer;
  VPointCount: Integer;
  VMarksIterator: TMarksIteratorBase;
  VMark: TMarkFull;
begin
  VMarksIterator := GState.MarksDb.GetMarksIterator(FZoom, FLLRect, GState.show_point);
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
          drawPath(
            VMark.Points,
            VMark.Color1,
            VMark.Color2,
            VMark.Scale1,
            VMark.IsPoly
            );
        end;
      end else if VPointCount = 1 then begin
        DrawPoint(
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
    VMarksIterator.Free;
  end;
end;

{ TMapMarksBitmapLayerProviderStuped }

procedure TMapMarksBitmapLayerProviderStuped.GetBitmapRect(
  ATargetBmp: TCustomBitmap32; AConverter: ICoordConverter;
  ATargetRect: TRect; ATargetZoom: Byte);
var
  VWorker: TMapMarksBitmapLayerProviderStupedThreaded;
begin
  if (GState.show_point <> mshNone) then begin
    VWorker := TMapMarksBitmapLayerProviderStupedThreaded.Create(
      ATargetBmp, AConverter, ATargetRect, ATargetZoom);
    try
      TThread.Synchronize(nil, VWorker.SyncGetBitmap);
    finally
      VWorker.Free;
    end;
  end;
end;

end.
