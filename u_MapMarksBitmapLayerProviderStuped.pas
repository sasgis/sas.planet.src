unit u_MapMarksBitmapLayerProviderStuped;

interface

uses
  Types,
  GR32,
  graphics,
  i_ICoordConverter,
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
  Ugeofun,
  u_MarksSimple,
  u_MarksReadWriteSimple;

const
  CMaxFontSize = 20;

{ TMapMarksBitmapLayerProviderStupedThreaded }

type
  TMapMarksBitmapLayerProviderStupedThreaded = class
  private
    FTargetBmp: TCustomBitmap32;
    FGeoConvert: ICoordConverter;
    FTargetRect: TRect;
    FZoom: Byte;
    FLLRect: TExtendedRect;
    FTempBmp: TCustomBitmap32;
    FBitmapWithText: TBitmap32;
    function MapPixel2BitmapPixel(Pnt: TPoint): TPoint; overload; virtual;
    function MapPixel2BitmapPixel(Pnt: TExtendedPoint): TExtendedPoint; overload; virtual;
    procedure PreparePolygon(pathll: TExtendedPointArray; polygon: TPolygon32);
    procedure drawPath(pathll: TExtendedPointArray; color1, color2: TColor32; linew: integer; poly: boolean);
    procedure DrawPoint(ALL: TExtendedPoint; AName: string; APicName: string; AMarkSize, AFontSize: integer; AColor1, AColor2: TColor32);
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
  VDeltaLL: TExtendedPoint;
begin
  FTargetBmp := ATargetBmp;
  FGeoConvert := AConverter;
  FTargetRect := ATargetRect;
  FZoom := ATargetZoom;
  FLLRect := FGeoConvert.PixelRect2LonLatRect(FTargetRect, FZoom);

  FTempBmp := TCustomBitmap32.Create;
  FTempBmp.DrawMode := dmBlend;
  FTempBmp.Resampler := TLinearResampler.Create;

  FBitmapWithText := TBitmap32.Create;
  FBitmapWithText.Font.Name := 'Tahoma';
  FBitmapWithText.Font.Style := [];
  FBitmapWithText.DrawMode := dmBlend;
  FBitmapWithText.CombineMode:=cmMerge;
  FBitmapWithText.Font.Size := CMaxFontSize;
end;

function TMapMarksBitmapLayerProviderStupedThreaded.MapPixel2BitmapPixel(
  Pnt: TPoint): TPoint;
begin
  Result.X := Pnt.X - FTargetRect.Left;
  Result.Y := Pnt.Y - FTargetRect.Top;
end;

function TMapMarksBitmapLayerProviderStupedThreaded.MapPixel2BitmapPixel(
  Pnt: TExtendedPoint): TExtendedPoint;
begin
  Result.X := Pnt.X - FTargetRect.Left;
  Result.Y := Pnt.Y - FTargetRect.Top;
end;

procedure TMapMarksBitmapLayerProviderStupedThreaded.PreparePolygon(
  pathll: TExtendedPointArray; polygon: TPolygon32);
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

destructor TMapMarksBitmapLayerProviderStupedThreaded.Destroy;
begin
  FreeAndNil(FTempBmp);
  FreeAndNil(FBitmapWithText);
  inherited;
end;

procedure TMapMarksBitmapLayerProviderStupedThreaded.drawPath(
  pathll: TExtendedPointArray; color1, color2: TColor32; linew: integer;
  poly: boolean);
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
      end;
    finally
      polygon.Free;
    end;
  except
  end;
end;

procedure TMapMarksBitmapLayerProviderStupedThreaded.DrawPoint(
  ALL: TExtendedPoint; AName, APicName: string; AMarkSize, AFontSize: integer;
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
  indexmi := GState.MarkIcons.IndexOf(APicName);
  if (indexmi = -1) and (GState.MarkIcons.Count > 0) then begin
    indexmi := 0;
  end;
  if (indexmi > -1) then begin
    VIconSource := TCustomBitmap32(GState.MarkIcons.Objects[indexmi]);
    FTempBmp.SetSize(VIconSource.Width, VIconSource.Height);
    FTempBmp.Draw(0, 0, VIconSource);
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
  TestArrLenLonLatRect: TExtendedRect;
  TestArrLenPixelRect: TExtendedRect;
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
