unit u_MapCombineThreadECW;

interface

uses
  Types,
  SysUtils,
  Classes,
  GR32,
  UECWWrite,
  UMapType,
  UImgFun,
  UGeoFun,
  bmpUtil,
  t_GeoTypes,
  UResStrings,
  u_MapCombineThreadBase;

type
  PRow = ^TRow;
  TRow = array[0..0] of byte;

  P256rgb = ^T256rgb;
  T256rgb = array[0..255] of PRow;

  PArrayBGR = ^TArrayBGR;
  TArrayBGR = array [0..0] of TBGR;

  P256ArrayBGR = ^T256ArrayBGR;
  T256ArrayBGR = array[0..255] of PArrayBGR;

  TMapCombineThreadECW = class(TMapCombineThreadBase)
  private
    sx,ex,sy,ey:integer;
    Rarr:P256rgb;
    Garr:P256rgb;
    Barr:P256rgb;
    FECWWriter:TECWWrite;
    btmm:TBitmap32;
    btmh:TBitmap32;
    FQuality: Integer;

    function ReadLineECW(Line:cardinal;var LineR,LineG,LineB:PLineRGB):boolean;
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
  ECWWriter,
  i_ICoordConverter,
  u_GlobalState;

constructor TMapCombineThreadECW.Create(
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

function TMapCombineThreadECW.ReadLineECW(Line: cardinal; var LineR, LineG,
  LineB: PLineRGB): boolean;
var
  i,j,rarri,lrarri,p_x,p_y,Asx,Asy,Aex,Aey,starttile:integer;
  p_h:TPoint;
  p:PColor32array;
  VTileRect: TRect;
begin
  Result := True;
  if line<(256-sy) then begin
    starttile:=sy+line;
  end else begin
    starttile:=(line-(256-sy)) mod 256;
  end;
  if (starttile=0)or(line=0) then begin
    FProgressOnForm:=line;
    Synchronize(UpdateProgressFormBar);
    FShowOnFormLine1:=SAS_STR_Processed+': '+inttostr(Round((line/(FMapPieceSize.Y))*100))+'%';
    Synchronize(UpdateProgressFormStr2);
    p_y:=(FCurrentPieceRect.Top+line)-((FCurrentPieceRect.Top+line) mod 256);
    p_x:=FCurrentPieceRect.Left-(FCurrentPieceRect.Left mod 256);
    p_h := FTypeMap.GeoConvert.PixelPos2OtherMap(Point(p_x,p_y), FZoom, FHTypeMap.GeoConvert);
    lrarri:=0;
    if line>(255-sy) then Asy:=0 else Asy:=sy;
    if (p_y div 256)=(FCurrentPieceRect.Bottom div 256) then Aey:=ey else Aey:=255;
    Asx:=sx;
    Aex:=255;
    while p_x<=FCurrentPieceRect.Right do begin
      // запомнием координаты обрабатываемого тайла для случая если произойдет ошибка
      FLastTile := Point(p_x shr 8, p_y shr 8);
      if not(RgnAndRgn(FPoly, p_x+128, p_y+128, false)) then begin
        btmm.Clear(Color32(GState.BGround))
      end else begin
        btmm.Clear(Color32(GState.BGround));
        VTileRect := FTypeMap.GeoConvert.TilePos2PixelRect(FLastTile, FZoom);
        FTypeMap.LoadTileOrPreZ(btmm, FLastTile, FZoom,false, true);
        if FHTypeMap<>nil then begin
          btmh.Clear($FF000000);
          FHTypeMap.LoadTileUni(btmh, FLastTile, FZoom, False, FTypeMap.GeoConvert, True, True, True);
          btmh.DrawMode:=dmBlend;
          btmm.Draw(0,0,btmh);
        end;
        FLastTile := Point(p_x shr 8, p_y shr 8);
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
          Rarr^[j]^[rarri]:=(cardinal(p^[i]) shr 16);
          Garr^[j]^[rarri]:=(cardinal(p^[i]) shr 8);
          Barr^[j]^[rarri]:=(cardinal(p^[i]));
          inc(rarri);
        end;
      end;
      lrarri:=rarri;
      Asx:=0;
      inc(p_x,256);
      inc(p_h.x,256);
    end;
  end;
  for i:=0 to (FCurrentPieceRect.Right-FCurrentPieceRect.Left)-1 do begin
    LineR^[i]:=Rarr^[starttile]^[i];
    LineG^[i]:=Garr^[starttile]^[i];
    LineB^[i]:=Barr^[starttile]^[i];
  end;
end;

procedure TMapCombineThreadECW.saveRECT;
var
  k: integer;
  Datum, Proj: string;
  Units: TCellSizeUnits;
  CellIncrementX, CellIncrementY, OriginX, OriginY: extended;
  errecw: integer;
  Path: string;
begin
  sx:=(FCurrentPieceRect.Left mod 256);
  sy:=(FCurrentPieceRect.Top mod 256);
  ex:=(FCurrentPieceRect.Right mod 256);
  ey:=(FCurrentPieceRect.Bottom mod 256);
  try
    FECWWriter:=TECWWrite.Create;
    btmm:=TBitmap32.Create;
    btmh:=TBitmap32.Create;
    btmm.Width:=256;
    btmm.Height:=256;
    btmh.Width:=256;
    btmh.Height:=256;
    getmem(Rarr,256*sizeof(PRow));
    for k:=0 to 255 do getmem(Rarr[k],(FMapSize.X+1)*sizeof(byte));
    getmem(Garr,256*sizeof(PRow));
    for k:=0 to 255 do getmem(Garr[k],(FMapSize.X+1)*sizeof(byte));
    getmem(Barr,256*sizeof(PRow));
    for k:=0 to 255 do getmem(Barr[k],(FMapSize.X+1)*sizeof(byte));
    FShowOnFormLine0:=SAS_STR_Resolution+': '+inttostr((FMapSize.X))+'x'+inttostr((FMapSize.Y));
    Synchronize(UpdateProgressFormStr1);
    Datum := 'EPSG:' + IntToStr(FTypeMap.GeoConvert.GetDatumEPSG);
    Proj := 'EPSG:' + IntToStr(FTypeMap.GeoConvert.GetProjectionEPSG);
    Units := FTypeMap.GeoConvert.GetCellSizeUnits;
    CalculateWFileParams(
      FTypeMap.GeoConvert.PixelPos2LonLat(FCurrentPieceRect.TopLeft, FZoom),
      FTypeMap.GeoConvert.PixelPos2LonLat(FCurrentPieceRect.BottomRight, FZoom),
      FMapPieceSize.X, FMapPieceSize.Y, FTypeMap.GeoConvert,
      CellIncrementX,CellIncrementY,OriginX,OriginY
    );
    errecw:=FECWWriter.Encode(FCurrentFileName,FMapPieceSize.X, FMapPieceSize.Y, 101-FQuality, COMPRESS_HINT_BEST, ReadLineECW, IsCancel, nil,
    Datum,Proj,Units,CellIncrementX,CellIncrementY,OriginX,OriginY);
    if (errecw>0)and(errecw<>52) then begin
      path:=FTypeMap.GetTileShowName(FLastTile, FZoom);
      FMessageForShow:=SAS_ERR_Save+' '+SAS_ERR_Code+inttostr(errecw)+#13#10+path;
      Synchronize(SynShowMessage);
    end;
  finally
    {$IFDEF VER80}
      for k:=0 to 255 do freemem(Rarr[k],(Poly1.X-Poly0.X+1)*sizeof(byte));
      freemem(Rarr,256*((Poly1.X-Poly0.X+1)*sizeof(byte)));
      for k:=0 to 255 do freemem(Garr[k],(Poly1.X-Poly0.X+1)*sizeof(byte));
      freemem(Garr,256*((Poly1.X-Poly0.X+1)*sizeof(byte)));
      for k:=0 to 255 do freemem(Barr[k],(Poly1.X-Poly0.X+1)*sizeof(byte));
      freemem(Barr,256*((Poly1.X-Poly0.X+1)*sizeof(byte)));
    {$ELSE}
      for k:=0 to 255 do freemem(Rarr[k]);
      FreeMem(Rarr);
      for k:=0 to 255 do freemem(Garr[k]);
      FreeMem(Garr);
      for k:=0 to 255 do freemem(Barr[k]);
      FreeMem(Barr);
    {$ENDIF}
    btmm.Free;
    btmh.Free;
    FECWWriter.Free;
  end;
end;

end.
