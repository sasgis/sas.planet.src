unit u_LayerScaleLine;

interface

uses
  Types,
  GR32,
  GR32_Image,
  i_LocalCoordConverter,
  i_InternalPerformanceCounter,
  i_ViewPortState,
  i_ScaleLineConfig,
  u_WindowLayerWithPos;

type
  TLayerScaleLine = class(TWindowLayerFixedSizeWithBitmap)
  private
    FConfig: IScaleLineConfig;
    FTmpBitmap: TBitmap32;
    procedure OnConfigChange;
  protected
    procedure SetLayerCoordConverter(AValue: ILocalCoordConverter); override;
    function GetMapLayerLocationRect: TFloatRect; override;
    procedure DoRedraw; override;
  public
    procedure StartThreads; override;
  public
    constructor Create(
      APerfList: IInternalPerformanceCounterList;
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      AConfig: IScaleLineConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  SysUtils,
  GR32_Backends,
  i_CoordConverter,
  u_NotifyEventListener,
  u_ResStrings,
  t_GeoTypes;

const
  D2R: Double = 0.017453292519943295769236907684886;// Константа для преобразования градусов в радианы

{ TLayerScaleLine }

constructor TLayerScaleLine.Create(
  APerfList: IInternalPerformanceCounterList;
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  AConfig: IScaleLineConfig
);
var
  VSize: TPoint;
begin
  inherited Create(APerfList, AParentMap, AViewPortState);
  FConfig := AConfig;
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );

  FLayer.Bitmap.Font.Name := 'arial';
  FLayer.Bitmap.Font.Size := 8;

  FTmpBitmap := TBitmap32.Create;
  FTmpBitmap.Font := FLayer.Bitmap.Font;
  FTmpBitmap.Font.Size := FLayer.Bitmap.Font.Size;

  VSize.X := 400;
  VSize.Y := 40;
  FLayer.Bitmap.SetSize(VSize.X, VSize.Y);
  DoUpdateLayerSize(VSize);
end;

destructor TLayerScaleLine.Destroy;
begin
  FTmpBitmap.Free;
  inherited Destroy;
end;

procedure TLayerScaleLine.DoRedraw;

  procedure DrawOutlinedText(X, Y: Integer; Text: string);
  var
    I, J: Integer;
    VTextColor: Cardinal;
    VOutLineColor: Cardinal;
  begin
    VTextColor := SetAlpha(clWhite32, 255);
    VOutLineColor := SetAlpha(clBlack32, 90);

    FTmpBitmap.SetSize(FLayer.bitmap.TextWidth(Text) + 4, FLayer.bitmap.TextHeight(Text) + 4);
    FTmpBitmap.Clear(0);
    FTmpBitmap.DrawMode := dmOpaque;
    FTmpBitmap.RenderText(2, 2, Text, 0, VTextColor);
    for I := 1 to FTmpBitmap.Width - 2 do begin
      for J := 1 to FTmpBitmap.Height - 2 do begin
        if (FTmpBitmap.Pixel[I, J] <> VTextColor) and (FTmpBitmap.Pixel[I, J] <> VOutLineColor) then begin
          if
            (FTmpBitmap.Pixel[I + 1, J] = VTextColor) or
            (FTmpBitmap.Pixel[I - 1, J] = VTextColor) or
            (FTmpBitmap.Pixel[I, J + 1] = VTextColor) or
            (FTmpBitmap.Pixel[I, J - 1] = VTextColor) or
            (FTmpBitmap.Pixel[I + 1, J + 1] = VTextColor) or
            (FTmpBitmap.Pixel[I - 1, J + 1] = VTextColor) or
            (FTmpBitmap.Pixel[I + 1, J - 1] = VTextColor) or
            (FTmpBitmap.Pixel[I - 1, J - 1] = VTextColor)
          then begin
            FTmpBitmap.Pixel[I, J] := VOutLineColor;
          end;
        end;
      end;
    end;
    FTmpBitmap.DrawTo(FLayer.Bitmap, X, Y);
  end;

var
  rnum: integer;
  se: string;
  LL: TDoublePoint;
  num: real;
  VBitmapSize: TPoint;
  VRad: Extended;
  VConverter: ICoordConverter;
  VPixelsAtZoom: Double;
  VZoom: Byte;
  VVisualCoordConverter: ILocalCoordConverter;
  I: Integer;
  VStartX, VStartY: Integer;
begin
  inherited;
  VVisualCoordConverter := LayerCoordConverter;
  VBitmapSize := Point(FLayer.Bitmap.Width, FLayer.Bitmap.Height);
  VConverter := VVisualCoordConverter.GetGeoConverter;
  VZoom := VVisualCoordConverter.GetZoom;
  LL := VVisualCoordConverter.GetCenterLonLat;

  VRad := VConverter.Datum.GetSpheroidRadiusA;
  VPixelsAtZoom := VConverter.PixelsAtZoomFloat(VZoom);

  num := 300 / ((VPixelsAtZoom / (2 * PI)) / (VRad * cos(LL.y * D2R)));

  if num > 10000 then begin
    num := num / 1000;
    se := ' ' + SAS_UNITS_km + ' ';
  end else if num < 10 then begin
    num := num * 100;
    se := ' ' + SAS_UNITS_sm + ' ';
  end else begin
    se := ' ' + SAS_UNITS_m + ' ';
  end;
  rnum := round(num/2);

  FLayer.Bitmap.Clear(SetAlpha(clWhite32, 0));

  DrawOutlinedText(0, 4, '0 ' + se);
  DrawOutlinedText(150, 4, IntToStr(rnum) + ' ' + se);
  DrawOutlinedText(300, 4, IntToStr(rnum*2) + ' ' + se);

  // длинная горизонталь снизу
  FLayer.Bitmap.Line(0, VBitmapSize.Y - 1, 300, VBitmapSize.Y - 1, SetAlpha(clBlack32, 90));
  FLayer.Bitmap.Line(0, VBitmapSize.Y - 2, 300, VBitmapSize.Y - 2, SetAlpha(clWhite32, 255));
  FLayer.Bitmap.Line(0, VBitmapSize.Y - 3, 300, VBitmapSize.Y - 3, SetAlpha(clBlack32, 90));
  // заборчик: длинная/короткая вертикали
  for I := 0 to 4 do begin
    VStartX := 1 + I*75;
    if (I = 1) or (I = 3) then begin
      VStartY := VBitmapSize.Y - 10; // короткая вертикаль
    end else begin
      VStartY := VBitmapSize.Y - 20; // длинная вертикаль
    end;
    // вертикаль
    FLayer.Bitmap.Line(VStartX - 1, VStartY, VStartX - 1, VBitmapSize.Y - 1, SetAlpha(clBlack32, 90));
    FLayer.Bitmap.Line(VStartX,     VStartY, VStartX,     VBitmapSize.Y - 1, SetAlpha(clWhite32, 255));
    FLayer.Bitmap.Line(VStartX + 1, VStartY, VStartX + 1, VBitmapSize.Y - 1, SetAlpha(clBlack32, 90));
    // "шапка"
    FLayer.Bitmap.Line(VStartX - 1, VStartY, VStartX + 1, VStartY, SetAlpha(clBlack32, 90));
  end;
  FLayer.Bitmap.Line(0, VBitmapSize.Y - 1, 302, VBitmapSize.Y - 1, SetAlpha(clBlack32, 90));
  FLayer.Bitmap.Line(1, VBitmapSize.Y - 2, 301, VBitmapSize.Y - 2, SetAlpha(clWhite32, 255));
end;

function TLayerScaleLine.GetMapLayerLocationRect: TFloatRect;
var
  VSize: TPoint;
begin
  VSize := Point(FLayer.Bitmap.Width, FLayer.Bitmap.Height);
  Result.Left := 6;
  Result.Bottom := ViewCoordConverter.GetLocalRectSize.Y - 6 - FConfig.BottomMargin;
  Result.Right := Result.Left + VSize.X;
  Result.Top := Result.Bottom - VSize.Y;
end;

procedure TLayerScaleLine.OnConfigChange;
begin
  ViewUpdateLock;
  try
    SetVisible(FConfig.Visible);
    SetNeedRedraw;
    SetNeedUpdateLocation;
  finally
    ViewUpdateUnlock;
  end;
  ViewUpdate;
end;

procedure TLayerScaleLine.SetLayerCoordConverter(AValue: ILocalCoordConverter);
begin
  inherited;
  SetNeedRedraw;
end;

procedure TLayerScaleLine.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
