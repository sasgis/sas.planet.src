unit u_LayerStatBar;

interface

uses
  Types,
  GR32_Image,
  u_WindowLayerBasic;

type
  TLayerStatBar = class(TWindowLayerBasic)
  protected
    function GetBitmapSizeInPixel: TPoint; override;
    function GetFreezePointInVisualPixel: TPoint; override;
    function GetFreezePointInBitmapPixel: TPoint; override;
    procedure DoRedraw; override;
  public
    constructor Create(AParentMap: TImage32);
  end;

implementation

uses
  SysUtils,
  GR32,
  t_GeoTypes,
  u_GeoToStr,
  UResStrings,
  Unit1,
  uMapType,
  u_GlobalState;

{ TLayerStatBar }

constructor TLayerStatBar.Create(AParentMap: TImage32);
begin
  inherited Create(AParentMap);
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

procedure TLayerStatBar.DoRedraw;
var
  ll:TextendedPoint;
  subs2:string;
  posnext:integer;
  TameTZ:TDateTime;
  VPoint: TPoint;
  VZoomCurr: Byte;
  VLonLatStr: String;
  VSize: TPoint;
begin
  inherited;
  VSize := GetBitmapSizeInPixel;
  VZoomCurr := GState.zoom_size - 1;
  VPoint := FMain.VisiblePixel2MapPixel(m_m);
  GState.sat_map_both.GeoConvert.CheckPixelPos(VPoint, VZoomCurr, GState.CiclMap);
  ll := GState.sat_map_both.GeoConvert.PixelPos2LonLat(VPoint, VZoomCurr);
  if GState.FirstLat then begin
    VLonLatStr := lat2str(ll.y, GState.llStrType)+' '+lon2str(ll.x, GState.llStrType);
  end else begin
    VLonLatStr := lon2str(ll.x, GState.llStrType)+' '+lat2str(ll.y, GState.llStrType);
  end;
  FLayer.Bitmap.Clear(SetAlpha(clWhite32,160));
  FLayer.Bitmap.Line(0, 0, VSize.X, 0, SetAlpha(clBlack32,256));
  FLayer.Bitmap.RenderText(4, 1, inttostr(GState.zoom_size) + 'x', 0, clBlack32);
  FLayer.Bitmap.RenderText(29, 1, '| '+SAS_STR_coordinates + ' ' + VLonLatStr, 0, clBlack32);

  TameTZ := FMain.timezone(ll.x,ll.y);
  subs2 := DistToStrWithUnits(1/((zoom[GState.zoom_size]/(2*PI))/(GState.sat_map_both.radiusa*cos(ll.y*D2R))), GState.num_format)+SAS_UNITS_mperp;
  FLayer.Bitmap.RenderText(278,1,' | '+SAS_STR_Scale+' '+subs2, 0, clBlack32);
  posnext:=273+FLayer.Bitmap.TextWidth(subs2)+70;
  FLayer.Bitmap.RenderText(posnext,1,' | '+SAS_STR_time+' '+ TimeToStr(TameTZ), 0, clBlack32);
  posnext:=posnext+FLayer.Bitmap.TextWidth(SAS_STR_time+' '+TimeToStr(TameTZ))+10;
  subs2:=GState.sat_map_both.GetTileShowName(VPoint.X, VPoint.Y, GState.zoom_size);
  FLayer.Bitmap.RenderText(posnext,1,' | '+SAS_STR_load+' '+inttostr(GState.All_Dwn_Tiles)+' ('+kb2KbMbGb(GState.All_Dwn_Kb)+') | '+SAS_STR_file+' '+subs2, 0, clBlack32);
end;

end.
