unit u_ThreadMapCombineKMZ;

interface

uses
  Windows,
  Types,
  SysUtils,
  Classes,
  GR32,
  ijl,
  UMapType,
  UGeoFun,
  bmpUtil,
  t_GeoTypes,
  i_MarksSimple,
  i_IBitmapPostProcessingConfig,
  UResStrings,
  u_ThreadMapCombineBase;

type
  PArrayBGR = ^TArrayBGR;
  TArrayBGR = array [0..0] of TBGR;

  P256ArrayBGR = ^T256ArrayBGR;
  T256ArrayBGR = array[0..255] of PArrayBGR;

  TThreadMapCombineKMZ = class(TThreadMapCombineBase)
  private
    FArray256BGR: P256ArrayBGR;
    sx, ex, sy, ey: integer;
    btmm: TCustomBitmap32;
    FQuality: Integer;

    procedure ReadLineBMP(Line: cardinal; LineRGB: PLineRGBb);
  protected
    procedure saveRECT; override;
  public
    constructor Create(
      AMapCalibrationList: IInterfaceList;
      AFileName: string;
      APolygon: TDoublePointArray;
      ASplitCount: TPoint;
      Azoom: byte;
      Atypemap: TMapType;
      AHtypemap: TMapType;
      AusedReColor: Boolean;
      ARecolorConfig: IBitmapPostProcessingConfigStatic;
      AMarksSubset: IMarksSubset;
      AQuality: Integer
    );
  end;

implementation

uses
  KAZip,
  i_ICoordConverter,
  i_ILocalCoordConverter,
  u_MapCalibrationKml,
  u_GeoToStr,
  u_GlobalState;

constructor TThreadMapCombineKMZ.Create(
  AMapCalibrationList: IInterfaceList;
  AFileName: string;
  APolygon: TDoublePointArray;
  ASplitCount: TPoint;
  Azoom: byte;
  Atypemap, AHtypemap: TMapType;
  AusedReColor: Boolean;
  ARecolorConfig: IBitmapPostProcessingConfigStatic;
  AMarksSubset: IMarksSubset;
  AQuality: Integer
);
begin
  inherited Create(AMapCalibrationList, AFileName, APolygon, ASplitCount,
    Azoom, Atypemap, AHtypemap, AusedReColor, ARecolorConfig, AMarksSubset);
  FQuality := AQuality;
end;

procedure TThreadMapCombineKMZ.ReadLineBMP(Line: cardinal;
  LineRGB: PLineRGBb);
var
  i, j, rarri, lrarri, p_x, p_y, Asx, Asy, Aex, Aey, starttile: integer;
  p_h: TPoint;
  p: PColor32array;
  VConverter: ILocalCoordConverter;
begin
  if line < (256 - sy) then begin
    starttile := sy + line;
  end else begin
    starttile := (line - (256 - sy)) mod 256;
  end;
  if (starttile = 0) or (line = 0) then begin
    FTilesProcessed := line;
    ProgressFormUpdateOnProgress;
    p_y := (FCurrentPieceRect.Top + line) - ((FCurrentPieceRect.Top + line) mod 256);
    p_x := FCurrentPieceRect.Left - (FCurrentPieceRect.Left mod 256);
    p_h := FTypeMap.GeoConvert.PixelPos2OtherMap(Point(p_x, p_y), Fzoom, FHTypeMap.GeoConvert);
    lrarri := 0;
    if line > (255 - sy) then begin
      Asy := 0;
    end else begin
      Asy := sy;
    end;
    if (p_y div 256) = (FCurrentPieceRect.Bottom div 256) then begin
      Aey := ey;
    end else begin
      Aey := 255;
    end;
    Asx := sx;
    Aex := 255;
    while p_x <= FCurrentPieceRect.Right do begin
      if not (RgnAndRgn(FPoly, p_x + 128, p_y + 128, false)) then begin
        btmm.Clear(FBackGroundColor);
      end else begin
        FLastTile := Point(p_x shr 8, p_y shr 8);
        VConverter := CreateConverterForTileImage(FLastTile);
        PrepareTileBitmap(btmm, VConverter);
      end;
      if (p_x + 256) > FCurrentPieceRect.Right then begin
        Aex := ex;
      end;
      for j := Asy to Aey do begin
        p := btmm.ScanLine[j];
        rarri := lrarri;
        for i := Asx to Aex do begin
          CopyMemory(@FArray256BGR[j]^[rarri], Pointer(integer(p) + (i * 4)), 3);
          inc(rarri);
        end;
      end;
      lrarri := rarri;
      Asx := 0;
      inc(p_x, 256);
      inc(p_h.x, 256);
    end;
  end;
  CopyMemory(LineRGB, FArray256BGR^[starttile], (FCurrentPieceRect.Right - FCurrentPieceRect.Left) * 3);
end;

procedure TThreadMapCombineKMZ.saveRECT;
var
  iNChannels, iWidth, iHeight: integer;
  k, i, j: integer;
  jcprops: TJPEG_CORE_PROPERTIES;
  Ckml: TMapCalibrationKml;
  BufRect: TRect;
  FileName: string;

  kmlm, jpgm: TMemoryStream;
  LL1, LL2: TDoublePoint;
  str: UTF8String;
  VFileName: String;
  bFMapPieceSizey: integer;
  nim: TPoint;

  Zip: TKaZip;
begin
  nim.X := (FMapPieceSize.X div 1024) + 1;
  nim.Y := (FMapPieceSize.Y div 1024) + 1;

  bFMapPieceSizey := FMapPieceSize.y;

  iWidth := FMapPieceSize.X div (nim.X);
  iHeight := FMapPieceSize.y div (nim.Y);

  FMapPieceSize.y := iHeight;

  if ((nim.X * nim.Y) > 100) and (FNumImgsSaved = 0) then begin
    ShowMessageSync(SAS_MSG_GarminMax1Mp);
  end;
  BufRect := FCurrentPieceRect;

  Zip := TKaZip.Create(nil);
  Zip.FileName := ChangeFileExt(FCurrentFileName, '.kmz');
  Zip.CreateZip(ChangeFileExt(FCurrentFileName, '.kmz'));
  Zip.CompressionType := ctFast;
  Zip.Active := true;
  //Zip.Open(ChangeFileExt(FCurrentFileName,'.kmz'));

  kmlm := TMemoryStream.Create;
  str := ansiToUTF8('<?xml version="1.0" encoding="UTF-8"?>' + #13#10 + '<kml xmlns="http://earth.google.com/kml/2.2">' + #13#10 + '<Folder>' + #13#10 + '<name>' + ExtractFileName(FCurrentFileName) + '</name>' + #13#10);

  for i := 1 to nim.X do begin
    for j := 1 to nim.Y do begin
      jpgm := TMemoryStream.Create;
      FileName := ChangeFileExt(FCurrentFileName, inttostr(i) + inttostr(j) + '.jpg');
      VFileName := 'files/' + ExtractFileName(FileName);
      try
        str := str + ansiToUTF8('<GroundOverlay>' + #13#10 + '<name>' + ExtractFileName(FileName) + '</name>' + #13#10 + '<drawOrder>75</drawOrder>' + #13#10);
        str := str + ansiToUTF8('<Icon><href>' + VFileName + '</href>' + '<viewBoundScale>0.75</viewBoundScale></Icon>' + #13#10);

        FCurrentPieceRect.Left := BufRect.Left + iWidth * (i - 1);
        FCurrentPieceRect.Right := BufRect.Left + iWidth * i;
        FCurrentPieceRect.Top := BufRect.Top + iHeight * (j - 1);
        FCurrentPieceRect.Bottom := BufRect.Top + iHeight * j;

        sx := (FCurrentPieceRect.Left mod 256);
        sy := (FCurrentPieceRect.Top mod 256);
        ex := (FCurrentPieceRect.Right mod 256);
        ey := (FCurrentPieceRect.Bottom mod 256);

        LL1 := FTypeMap.GeoConvert.PixelPos2LonLat(FCurrentPieceRect.TopLeft, FZoom);
        LL2 := FTypeMap.GeoConvert.PixelPos2LonLat(FCurrentPieceRect.BottomRight, FZoom);
        str := str + ansiToUTF8('<LatLonBox>' + #13#10);
        str := str + ansiToUTF8('<north>' + R2StrPoint(LL1.y) + '</north>' + #13#10);
        str := str + ansiToUTF8('<south>' + R2StrPoint(LL2.y) + '</south>' + #13#10);
        str := str + ansiToUTF8('<east>' + R2StrPoint(LL2.x) + '</east>' + #13#10);
        str := str + ansiToUTF8('<west>' + R2StrPoint(LL1.x) + '</west>' + #13#10);
        str := str + ansiToUTF8('</LatLonBox>' + #13#10 + '</GroundOverlay>' + #13#10);

        getmem(FArray256BGR, 256 * sizeof(P256ArrayBGR));
        for k := 0 to 255 do begin
          getmem(FArray256BGR[k], (iWidth + 1) * 3);
        end;
        btmm := TCustomBitmap32.Create;
        btmm.Width := 256;
        btmm.Height := 256;

        ijlInit(@jcprops);
        iNChannels := 3;
        jcprops.DIBWidth := iWidth;
        jcprops.DIBHeight := -iHeight;
        jcprops.DIBChannels := iNChannels;
        jcprops.DIBColor := IJL_BGR;
        jcprops.DIBPadBytes := ((((iWidth * iNChannels) + 3) div 4) * 4) - (iWidth * 3);
        GetMem(jcprops.DIBBytes, (iWidth * 3 + (iWidth mod 4)) * iHeight);
        jcprops.JPGSizeBytes := iWidth * iHeight * 3;
        GetMem(jcprops.JPGBytes, jcprops.JPGSizeBytes);
        if jcprops.DIBBytes <> nil then begin
          for k := 0 to iHeight - 1 do begin
            ReadLineBMP(k, Pointer(integer(jcprops.DIBBytes) + (((iWidth * 3 + (iWidth mod 4)) * iHeight) - (iWidth * 3 + (iWidth mod 4)) * (k + 1))));
            if IsCancel then begin
              break;
            end;
          end;
        end else begin
          ShowMessageSync(SAS_ERR_Memory + '.' + #13#10 + SAS_ERR_UseADifferentFormat);
          exit;
        end;
        jcprops.JPGWidth := iWidth;
        jcprops.JPGHeight := iHeight;
        jcprops.JPGChannels := 3;
        jcprops.JPGColor := IJL_YCBCR;
        jcprops.jquality := FQuality;
        ijlWrite(@jcprops, IJL_JBUFF_WRITEWHOLEIMAGE);
        jpgm.Write(jcprops.JPGBytes^, jcprops.JPGSizeBytes);
        jpgm.Position := 0;
        Zip.AddStream(VFileName, jpgm);
      Finally
        freemem(jcprops.DIBBytes, iWidth * iHeight * 3);
        for k := 0 to 255 do begin
          freemem(FArray256BGR[k], (iWidth + 1) * 3);
        end;
        freemem(FArray256BGR, 256 * ((iWidth + 1) * 3));
        ijlFree(@jcprops);
        btmm.Free;
        jpgm.Free;
      end;
    end;
  end;
  FMapPieceSize.y := bFMapPieceSizey;
  str := str + ansiToUTF8('</Folder>' + #13#10 + '</kml>');
  kmlm.Write(str[1], length(str));
  kmlm.Position := 0;
  Zip.AddStream('doc.kml', kmlm);
  Zip.Active := false;
  Zip.Close;
  Zip.Free;
  kmlm.Free;
  inc(FNumImgsSaved);
end;

end.
