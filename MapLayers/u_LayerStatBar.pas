unit u_LayerStatBar;

interface

uses
  Windows,
  Types,
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  u_MapViewPortState,
  u_WindowLayerBasic;

type
  TLayerStatBar = class(TWindowLayerBasicWithBitmap)
  protected
    FHeight: Integer;
    FWidth: Integer;
    FMinUpdateTickCount: Cardinal;
    FLastUpdateTick: DWORD;
    function GetBitmapSizeInPixel: TPoint; override;
    function GetTimeInLonLat(ALonLat: TDoublePoint): TDateTime;
    function GetMapLayerLocationRect: TFloatRect; override;
    procedure DoRedraw; override;
    procedure DoUpdateLayerSize; override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    procedure LoadConfig(AConfigProvider: IConfigDataProvider); override;
    procedure SaveConfig(AConfigProvider: IConfigDataWriteProvider); override;
    property Height: Integer read FHeight;
  end;

implementation

uses
  SysUtils,
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
  FHeight := 17;
  FLastUpdateTick := 0;
  FMinUpdateTickCount := 100;
end;

function TLayerStatBar.GetBitmapSizeInPixel: TPoint;
begin
  Result.X := FWidth;
  Result.Y := FHeight;
end;

function TLayerStatBar.GetMapLayerLocationRect: TFloatRect;
begin

end;

function TLayerStatBar.GetTimeInLonLat(ALonLat: TDoublePoint): TDateTime;
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

procedure TLayerStatBar.LoadConfig(AConfigProvider: IConfigDataProvider);
var
  VConfigProvider: IConfigDataProvider;
begin
  inherited;
  VConfigProvider := AConfigProvider.GetSubItem('VIEW');
  if VConfigProvider <> nil then begin
    Visible := VConfigProvider.ReadBool('StatusBar', True);
  end else begin
    Visible := True;
  end;
end;

procedure TLayerStatBar.SaveConfig(AConfigProvider: IConfigDataWriteProvider);
var
  VSubItem: IConfigDataWriteProvider;
begin
  inherited;
  VSubItem := AConfigProvider.GetOrCreateSubItem('VIEW');
  VSubItem.WriteBool('StatusBar', Visible);
end;

procedure TLayerStatBar.DoRedraw;
var
  ll: TDoublePoint;
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
  VPixelsAtZoom: Double;
  VCurrentTick: DWORD;
begin
  inherited;
  VCurrentTick := GetTickCount;
  if (VCurrentTick < FLastUpdateTick) or (VCurrentTick > FLastUpdateTick + 100) then begin
    GState.ViewState.LockRead;
    try
      VMap := GState.ViewState.GetCurrentMap;
      ll := GState.ViewState.VisiblePixel2LonLat(Fmain.MouseCursorPos);
      VPoint := GState.ViewState.VisiblePixel2MapPixel(Fmain.MouseCursorPos);
      VZoomCurr := GState.ViewState.GetCurrentZoom;
      VSize := GState.ViewState.GetViewSizeInVisiblePixel;
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
    FLayer.Bitmap.RenderText(4, 1, 'z' + inttostr(VZoomCurr + 1), 0, clBlack32);
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
    FLastUpdateTick := GetTickCount;
  end;
end;

procedure TLayerStatBar.DoUpdateLayerSize;
begin
  inherited;

end;

end.
