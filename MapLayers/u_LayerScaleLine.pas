unit u_LayerScaleLine;

interface

uses
  Types,
  GR32,
  GR32_Image,
  i_JclNotify,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_ILocalCoordConverter,
  u_MapViewPortState,
  u_WindowLayerWithPos;

type
  TLayerScaleLine = class(TWindowLayerFixedSizeWithPosWithBitmap)
  protected
    FBottomMargin: Integer;
    procedure DoRedraw; override;
    function GetMapLayerLocationRect: TFloatRect; override;
    procedure DoPosChange(ANewVisualCoordConverter: ILocalCoordConverter); override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    procedure LoadConfig(AConfigProvider: IConfigDataProvider); override;
    procedure SaveConfig(AConfigProvider: IConfigDataWriteProvider); override;
    property BottomMargin: Integer read FBottomMargin write FBottomMargin;
  end;

implementation

uses
  Math,
  SysUtils,
  i_ICoordConverter,
  UResStrings,
  t_GeoTypes,
  Ugeofun,
  u_NotifyEventListener,
  u_GlobalState;

const
  D2R: Double = 0.017453292519943295769236907684886;//  онстанта дл€ преобразовани€ градусов в радианы

{ TLayerScaleLine }

constructor TLayerScaleLine.Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
var
  VSize: TPoint;
begin
  inherited;
  FLayer.Bitmap.Font.Name := 'arial';
  FLayer.Bitmap.Font.Size := 10;
  VSize.X := 128;
  VSize.Y := 15;
  FLayer.Bitmap.SetSize(VSize.X, VSize.Y);
  DoUpdateLayerSize(VSize);
end;

procedure TLayerScaleLine.DoPosChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
begin
  inherited;
  Redraw;
end;

procedure TLayerScaleLine.DoRedraw;
var
  rnum, len_p, textstrt, textwidth: integer;
  s, se: string;
  LL: TDoublePoint;
  temp, num: real;
  VBitmapSize: TPoint;
  VRad: Extended;
  VConverter: ICoordConverter;
  VPixelsAtZoom: Double;
  VZoom: Byte;
  VScreenCenterVisual: TDoublePoint;
  VScreenCenterMap: TDoublePoint;
  VVisualCoordConverter: ILocalCoordConverter;
begin
  inherited;
  VVisualCoordConverter := FVisualCoordConverter;
  VBitmapSize := LayerSize;
  VScreenCenterVisual := DoublePoint(MapViewSize.X div 2, MapViewSize.Y div 2);
  VScreenCenterMap := VVisualCoordConverter.LocalPixelFloat2MapPixelFloat(VScreenCenterVisual);
  VConverter := VVisualCoordConverter.GetGeoConverter;
  VZoom := VVisualCoordConverter.GetZoom;
  VConverter.CheckPixelPosFloatStrict(VScreenCenterMap, VZoom, True);
  LL := VConverter.PixelPosFloat2LonLat(VScreenCenterMap, VZoom);

  VRad := VConverter.GetSpheroidRadius;
  VPixelsAtZoom := VConverter.PixelsAtZoomFloat(VZoom);
  num := 106 / ((VPixelsAtZoom / (2 * PI)) / (VRad * cos(LL.y * D2R)));
  if num > 10000 then begin
    num := num / 1000;
    se := ' ' + SAS_UNITS_km + ' ';
  end else if num < 10 then begin
    num := num * 100;
    se := ' ' + SAS_UNITS_sm + ' ';
  end else begin
    se := ' ' + SAS_UNITS_m + ' ';
  end;
  rnum := round(num);
  temp := power(5, (length(inttostr(rnum)) - 1));
  if ((rnum / temp) < 1.25) then begin
    rnum := round(temp);
  end else if ((rnum / temp) >= 3.75) then begin
    rnum := 5 * round(temp);
  end else begin
    rnum := round(2.5 * temp);
  end;
  len_p := round(106 / (num / rnum));
  s := inttostr(rnum) + se;
  textwidth := FLayer.bitmap.TextWidth(s);
  while (len_p < textwidth + 15) and (not (len_p = 0)) do begin
    rnum := rnum * 2;
    len_p := round(106 / (num / rnum));
  end;
  s := inttostr(rnum) + se;
  len_p := round(106 / (num / rnum));
  textwidth := FLayer.bitmap.TextWidth(s);

  FLayer.Bitmap.Clear(SetAlpha(clWhite32, 0));
  FLayer.Bitmap.FillRectS(Rect(0, 0, len_p, VBitmapSize.Y - 1), SetAlpha(clWhite32, 135));
  FLayer.bitmap.LineS(0, 0, 0, VBitmapSize.Y - 1, SetAlpha(clBlack32, 256));
  FLayer.bitmap.LineS(len_p - 1, 0, len_p - 1, VBitmapSize.Y - 1, SetAlpha(clBlack32, 256));
  textstrt := (len_p div 2) - (textwidth div 2);
  FLayer.bitmap.RenderText(textstrt, 0, s, 2, clBlack32);
end;

function TLayerScaleLine.GetMapLayerLocationRect: TFloatRect;
var
  VSize: TPoint;
begin
  VSize := LayerSize;
  Result.Left := 6;
  Result.Bottom := MapViewSize.Y - 6 - FBottomMargin;
  Result.Right := Result.Left + VSize.X;
  Result.Top := Result.Bottom - VSize.Y;
end;

procedure TLayerScaleLine.LoadConfig(AConfigProvider: IConfigDataProvider);
var
  VConfigProvider: IConfigDataProvider;
begin
  inherited;
  VConfigProvider := AConfigProvider.GetSubItem('VIEW');
  if VConfigProvider <> nil then begin
    Visible := VConfigProvider.ReadBool('ScaleLine', True);
  end else begin
    Visible := True;
  end;
end;

procedure TLayerScaleLine.SaveConfig(AConfigProvider: IConfigDataWriteProvider);
var
  VConfigProvider: IConfigDataWriteProvider;
begin
  inherited;
  VConfigProvider := AConfigProvider.GetOrCreateSubItem('VIEW');
  VConfigProvider.WriteBool('ScaleLine', Visible);
end;

end.
