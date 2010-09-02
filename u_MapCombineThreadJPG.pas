unit u_MapCombineThreadJPG;

interface

uses
  Windows,
  Types,
  SysUtils,
  Classes,
  GR32,
  ijl,
  UMapType,
  UImgFun,
  UGeoFun,
  bmpUtil,
  t_GeoTypes,
  UResStrings,
  u_ThreadMapCombineBase;

type
  PArrayBGR = ^TArrayBGR;
  TArrayBGR = array [0..0] of TBGR;

  P256ArrayBGR = ^T256ArrayBGR;
  T256ArrayBGR = array[0..255] of PArrayBGR;

  TMapCombineThreadJPG = class(TThreadMapCombineBase)
  private
    FArray256BGR: P256ArrayBGR;
    sx,ex,sy,ey:integer;
    btmm:TBitmap32;
    btmh:TBitmap32;
    FQuality: Integer;

    procedure ReadLineBMP(Line:cardinal;LineRGB:PLineRGBb);
  protected
    procedure saveRECT; override;
  public
    constructor Create(
      AMapCalibrationList: IInterfaceList;
      AFileName: string;
      APolygon: TExtendedPointArray;
      ASplitCount: TPoint;
      Azoom: byte;
      Atypemap: TMapType;
      AHtypemap: TMapType;
      AusedReColor,
      AusedMarks: boolean;
      AQuality: Integer
    );
  end;

implementation

uses
  i_ICoordConverter,
  u_GlobalState;

constructor TMapCombineThreadJPG.Create(
  AMapCalibrationList: IInterfaceList;
  AFileName:string;
  APolygon: TExtendedPointArray;
  ASplitCount: TPoint;
  Azoom:byte;
  Atypemap,AHtypemap:TMapType;
  AusedReColor,AusedMarks:boolean;
  AQuality: Integer
);
begin
  inherited Create(AMapCalibrationList, AFileName, APolygon, ASplitCount,
    Azoom, Atypemap, AHtypemap, AusedReColor, AusedMarks);
  FQuality := AQuality;
end;

procedure TMapCombineThreadJPG.ReadLineBMP(Line: cardinal;
  LineRGB: PLineRGBb);
var
  i,j,rarri,lrarri,p_x,p_y,Asx,Asy,Aex,Aey,starttile:integer;
  p_h:TPoint;
  p:PColor32array;
  VTileRect: TRect;
begin
  if line<(256-sy) then begin
    starttile:=sy+line
  end else begin
    starttile:=(line-(256-sy)) mod 256;
  end;
  if (starttile=0)or(line=0) then begin
    FTilesProcessed:=line;
    ProgressFormUpdateOnProgress;
    p_y:=(FCurrentPieceRect.Top+line)-((FCurrentPieceRect.Top+line) mod 256);
    p_x:=FCurrentPieceRect.Left-(FCurrentPieceRect.Left mod 256);
    p_h := FTypeMap.GeoConvert.PixelPos2OtherMap(Point(p_x,p_y), Fzoom, FHTypeMap.GeoConvert);
    lrarri:=0;
    if line>(255-sy) then Asy:=0 else Asy:=sy;
    if (p_y div 256)=(FCurrentPieceRect.Bottom div 256) then Aey:=ey else Aey:=255;
    Asx:=sx;
    Aex:=255;
    while p_x<=FCurrentPieceRect.Right do begin
      if not(RgnAndRgn(FPoly, p_x+128, p_y+128, false)) then begin
        btmm.Clear(Color32(GState.BGround))
      end else begin
        btmm.Clear(Color32(GState.BGround));
        FLastTile := Point(p_x shr 8, p_y shr 8);
        VTileRect := FTypeMap.GeoConvert.TilePos2PixelRect(FLastTile, FZoom);
        FTypeMap.LoadTileOrPreZ(btmm, FLastTile, FZoom, false, True);
        if FHTypeMap<>nil then begin
          btmh.Clear($FF000000);
          FHTypeMap.LoadTileUni(btmh, FLastTile, FZoom, False, FTypeMap.GeoConvert, True, True, True);
          btmh.DrawMode:=dmBlend;
          btmm.Draw(0,0,btmh);
        end;
        if FUsedMarks then begin
          GState.MarksBitmapProvider.GetBitmapRect(btmm, FTypeMap.GeoConvert, VTileRect, FZoom);
        end;
      end;
      if FUsedReColor then Gamma(btmm);
      if (p_x+256)>FCurrentPieceRect.Right then Aex:=ex;
      for j:=Asy to Aey do begin
        p:=btmm.ScanLine[j];
        rarri:=lrarri;
        for i:=Asx to Aex do begin
          CopyMemory(@FArray256BGR[j]^[rarri],Pointer(integer(p)+(i*4)),3);
          inc(rarri);
        end;
      end;
      lrarri:=rarri;
      Asx:=0;
      inc(p_x,256);
      inc(p_h.x,256);
    end;
  end;
  CopyMemory(LineRGB,FArray256BGR^[starttile],(FCurrentPieceRect.Right-FCurrentPieceRect.Left)*3);
end;

procedure TMapCombineThreadJPG.saveRECT;
var
  iNChannels, iWidth, iHeight: integer;
  k: integer;
  jcprops: TJPEG_CORE_PROPERTIES;
begin
  sx:=(FCurrentPieceRect.Left mod 256);
  sy:=(FCurrentPieceRect.Top mod 256);
  ex:=(FCurrentPieceRect.Right mod 256);
  ey:=(FCurrentPieceRect.Bottom mod 256);
  iWidth  := FMapPieceSize.X;
  iHeight := FMapPieceSize.y;
  try
    getmem(FArray256BGR,256*sizeof(P256ArrayBGR));
    for k:=0 to 255 do getmem(FArray256BGR[k],(iWidth+1)*3);
    btmm:=TBitmap32.Create;
    btmh:=TBitmap32.Create;
    btmm.Width:=256;
    btmm.Height:=256;
    btmh.Width:=256;
    btmh.Height:=256;
    ijlInit(@jcprops);
    iNChannels := 3;
    jcprops.DIBWidth := iWidth;
    jcprops.DIBHeight := -iHeight;
    jcprops.DIBChannels := iNChannels;
    jcprops.DIBColor := IJL_BGR;
    jcprops.DIBPadBytes := ((((iWidth*iNChannels)+3) div 4)*4)-(iWidth*3);
    GetMem(jcprops.DIBBytes,(iWidth*3+ (iWidth mod 4))*iHeight);
    if jcprops.DIBBytes<>nil then begin
      for k:=0 to iHeight-1 do begin
        ReadLineBMP(k,Pointer(integer(jcprops.DIBBytes)+(((iWidth*3+ (iWidth mod 4))*iHeight)-(iWidth*3+ (iWidth mod 4))*(k+1))));
        if IsCancel then break;
      end;
    end else begin
      ShowMessageSync(SAS_ERR_Memory+'.'+#13#10+SAS_ERR_UseADifferentFormat);
      exit;
    end;
    jcprops.JPGFile := PChar(FCurrentFileName);
    jcprops.JPGWidth := iWidth;
    jcprops.JPGHeight := iHeight;
    jcprops.JPGChannels := 3;
    jcprops.JPGColor := IJL_YCBCR;
    jcprops.jquality := FQuality;
    ijlWrite(@jcprops,IJL_JFILE_WRITEWHOLEIMAGE);
  Finally
    freemem(jcprops.DIBBytes,iWidth*iHeight*3);
    for k:=0 to 255 do freemem(FArray256BGR[k],(iWidth+1)*3);
    freemem(FArray256BGR,256*((iWidth+1)*3));
    ijlFree(@jcprops);
    btmm.Free;
    btmh.Free;
  end;
end;

end.
