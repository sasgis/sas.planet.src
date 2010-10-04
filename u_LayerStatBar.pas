unit u_LayerStatBar;

interface

uses
  Windows,
  Types,
  GR32_Image,
  t_GeoTypes,
  u_MapViewPortState,
  u_WindowLayerBasic;

type
  TLayerStatBar = class(TWindowLayerBasic)
  protected
    function GetBitmapSizeInPixel: TPoint; override;
    function GetFreezePointInVisualPixel: TPoint; override;
    function GetFreezePointInBitmapPixel: TPoint; override;
    function GetTimeInLonLat(ALonLat: TExtendedPoint): TDateTime;
    procedure DoRedraw; override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
  end;

implementation

uses
  SysUtils,
  GR32,
  u_GeoToStr,
  i_ICoordConverter,
  UResStrings,
  UTimeZones,
  Unit1,
  uMapType,
  u_GlobalState;

const
  D2R: Double = 0.017453292519943295769236907684886;//  онстанта дл€ преобразовани€ градусов в радианы

{ TLayerStatBar }

constructor TLayerStatBar.Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
begin
  inherited;
  FLayer.Bitmap.Font.Name := 'arial';
  FLayer.Bitmap.Font.Size := 10;
end;

function TLayerStatBar.GetBitmapSizeInPixel: TPoint;
begin
  Result.X := FParentMap.Width;
  Result.Y := 17;
end;

function TLayerStatBar.GetFreezePointInBitmapPixel: TPoint;
var
  VBitmapSize: TPoint;
begin
  VBitmapSize := GetBitmapSizeInPixel;
  Result := Point(0, VBitmapSize.Y);
end;

function TLayerStatBar.GetFreezePointInVisualPixel: TPoint;
var
  VVisibleSize: TPoint;
begin
  VVisibleSize := GetVisibleSizeInPixel;
  Result := Point(0, VVisibleSize.Y);
end;

function TLayerStatBar.GetTimeInLonLat(ALonLat: TExtendedPoint): TDateTime;
var
  prH, prM: integer;
  tz: real;
  st: TSystemTime;
begin
  tz := GetTZ_(ALonLat);
  GetSystemTime(st);
  prH := trunc(tz);
  prM := round(60 * frac(TZ));
  result := EncodeTime(abs(st.wHour + prH + 24) mod 24, abs(st.wMinute + prM + 60) mod 60, 0, 0);
end;

procedure TLayerStatBar.DoRedraw;
var
  ll: TextendedPoint;
  subs2: string;
  posnext: integer;
  TameTZ: TDateTime;
  VPoint: TPoint;
  VZoomCurr: Byte;
  VLonLatStr: String;
  VSize: TPoint;
  VRad: Extended;
  VTile: TPoint;
  VMap: TMapType;
  VConverter: ICoordConverter;
  VPixelsAtZoom: Extended;
begin
  inherited;
  GState.ViewState.LockRead;
  try
    VMap := GState.ViewState.GetCurrentMap;
    ll := GState.ViewState.VisiblePixel2LonLat(Fmain.m_m);
    VPoint := GState.ViewState.VisiblePixel2MapPixel(Fmain.m_m);
    VZoomCurr := GState.ViewState.GetCurrentZoom;
    VSize := GState.ViewState.GetVisibleSizeInPixel;
    VConverter := GState.ViewState.GetCurrentCoordConverter;
  finally
    GState.ViewState.UnLockRead;
  end;
  VMap.GeoConvert.CheckPixelPos(VPoint, VZoomCurr, True);
  VTile := VMap.GeoConvert.PixelPos2TilePos(VPoint, VZoomCurr);
  if GState.FirstLat then begin
    VLonLatStr := lat2str(ll.y, GState.llStrType) + ' ' + lon2str(ll.x, GState.llStrType);
  end else begin
    VLonLatStr := lon2str(ll.x, GState.llStrType) + ' ' + lat2str(ll.y, GState.llStrType);
  end;
  FLayer.Bitmap.Clear(SetAlpha(clWhite32, 160));
  FLayer.Bitmap.Line(0, 0, VSize.X, 0, SetAlpha(clBlack32, 256));
  FLayer.Bitmap.RenderText(4, 1, inttostr(VZoomCurr + 1) + 'x', 0, clBlack32);
  FLayer.Bitmap.RenderText(29, 1, '| ' + SAS_STR_coordinates + ' ' + VLonLatStr, 0, clBlack32);

  VRad := VConverter.GetSpheroidRadius;
  VPixelsAtZoom := VConverter.PixelsAtZoomFloat(VZoomCurr);
  subs2 := DistToStrWithUnits(1 / ((VPixelsAtZoom / (2 * PI)) / (VRad * cos(ll.y * D2R))), GState.num_format) + SAS_UNITS_mperp;
  FLayer.Bitmap.RenderText(278, 1, ' | ' + SAS_STR_Scale + ' ' + subs2, 0, clBlack32);
  posnext := 273 + FLayer.Bitmap.TextWidth(subs2) + 70;
  TameTZ := GetTimeInLonLat(ll);
  FLayer.Bitmap.RenderText(posnext, 1, ' | ' + SAS_STR_time + ' ' + TimeToStr(TameTZ), 0, clBlack32);
  posnext := posnext + FLayer.Bitmap.TextWidth(SAS_STR_time + ' ' + TimeToStr(TameTZ)) + 10;
  subs2 := VMap.GetTileShowName(VTile, VZoomCurr);
  FLayer.Bitmap.RenderText(posnext, 1, ' | ' + SAS_STR_load + ' ' + inttostr(GState.All_Dwn_Tiles) + ' (' + kb2KbMbGb(GState.All_Dwn_Kb) + ') | ' + SAS_STR_file + ' ' + subs2, 0, clBlack32);
end;

end.
