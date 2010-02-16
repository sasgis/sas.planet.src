unit UThreadScleit;

interface

uses
  Windows,
  Types,
  Forms,
  SysUtils,
  Classes,
  Dialogs,
  Graphics,
  GR32,
  ijl,
  UECWWrite,
  UMapType,
  UImgFun,
  UGeoFun,
  bmpUtil,
  t_GeoTypes,
  UResStrings,
  unit4;

type
  PRow = ^TRow;
  TRow = array[0..0] of byte;

  P256rgb = ^T256rgb;
  T256rgb = array[0..255] of PRow;

  PArrayBGR = ^TArrayBGR;
  TArrayBGR = array [0..0] of TBGR;

  P256ArrayBGR = ^T256ArrayBGR;
  T256ArrayBGR = array[0..255] of PArrayBGR;

  TThreadScleit = class(TThread)
  private
    FZoom: byte;
    FPoly: TPointArray;
    FLLRect: TRect;
    FMapCalibrationList: IInterfaceList;
    FSplitCount: TPoint;
    FFileName: string;
    FFilePath: string;
    FFileExt: string;
    FCurrentFileName: string;
    FTypeMap: TMapType;
    FHTypeMap: TMapType;
    FMapRect: TRect;
    FMapSize: TPoint;
    FMapPieceSize: TPoint;
    FCurrentPieceRect: TRect;
    FUsedReColor: boolean;
    FUsedMarks: boolean;

    FProgressForm: TFprogress2;

    Array256BGR:P256ArrayBGR;
    sx,ex,sy,ey:integer;
    Rarr:P256rgb;
    Garr:P256rgb;
    Barr:P256rgb;
    ecw:TECWWrite;
    btmm:TBitmap32;
    btmh:TBitmap32;
    prStr1, prStr2: string;
    prBar:integer;
    Message_:string;
    LastXY: TPoint;
    function ReadLineECW(Line:cardinal;var LineR,LineG,LineB:PLineRGB):boolean;
    function ReadLineBMP(Line:cardinal;LineRGB:PLineRGBb):boolean;
    function IsCancel: Boolean;
    procedure DrawMarks2Tile;
    procedure UpdateProgressFormBar;
    procedure UpdateProgressFormStr1;
    procedure UpdateProgressFormStr2;
    procedure UpdateProgressFormClose;
    procedure SynShowMessage;
    procedure saveRECT;
    procedure Save_ECW;
    procedure Save_BMP;
    procedure Save_JPG;
  protected
    procedure Execute; override;
  public
    constructor Create(
      AMapCalibrationList: IInterfaceList;
      AFName: string;
      APolygon_: TPointArray;
      numTilesG: integer;
      numTilesV: integer;
      Azoom: byte;
      Atypemap: TMapType;
      AHtypemap: TMapType;
      AusedReColor,
      AusedMarks: boolean
    );
  end;

implementation

uses
  ECWWriter,
  i_IMapCalibration,
  u_GlobalState,
  usaveas,
  u_MapMarksLayer,
  Unit1;

constructor TThreadScleit.Create(AMapCalibrationList: IInterfaceList; AFName:string;APolygon_:TPointArray;numTilesG,numTilesV:integer;Azoom:byte;Atypemap,AHtypemap:TMapType;AusedReColor,AusedMarks:boolean);
var
  VProcessTiles: Int64;
begin
  inherited Create(false);
  Priority := tpLower;
  FreeOnTerminate := true;
  FPoly := APolygon_;
  FZoom := Azoom;
  FSplitCount.X := numTilesG;
  FSplitCount.Y := numTilesV;
  FFilePath := ExtractFilePath(AFName);
  FFileExt := ExtractFileExt(AFName);
  FFileName := ChangeFileExt(ExtractFileName(AFName), '');
  FTypeMap := Atypemap;
  FHTypeMap := AHtypemap;
  FUsedReColor := AusedReColor;
  FUsedMarks := AusedMarks;
  FMapCalibrationList := AMapCalibrationList;


  Application.CreateForm(TFProgress2, FProgressForm);
  FProgressForm.Visible := true;

  VProcessTiles := GetDwnlNum(FMapRect.TopLeft, FMapRect.BottomRight, FPoly, true);
  GetMinMax(FMapRect.TopLeft, FMapRect.BottomRight, FPoly,false);

  FMapSize.X := FMapRect.Right - FMapRect.Left;
  FMapSize.Y := FMapRect.Bottom - FMapRect.Top;
  FMapPieceSize.X := FMapSize.X div FSplitCount.X;
  FMapPieceSize.Y := FMapSize.Y div FSplitCount.Y;
  FProgressForm.ProgressBar1.Max := FMapPieceSize.Y;

  FProgressForm.Caption := 'Склеить: '+inttostr((FMapSize.Y) div 256+1)+'x'
    +inttostr((FMapSize.Y) div 256+1) +'('+inttostr(VProcessTiles)+') '+SAS_STR_files;
end;

procedure TThreadScleit.SynShowMessage;
begin
 ShowMessage(Message_);
end;

procedure TThreadScleit.UpdateProgressFormClose;
begin
  FProgressForm.Close;
end;

procedure TThreadScleit.UpdateProgressFormStr1;
begin
  FProgressForm.MemoInfo.Lines[0] := prStr1;
end;

procedure TThreadScleit.UpdateProgressFormStr2;
begin
  FProgressForm.MemoInfo.Lines[1] := prStr2;
end;

procedure TThreadScleit.UpdateProgressFormBar;
begin
  FProgressForm.ProgressBar1.Progress1 := prBar;
end;

function TThreadScleit.IsCancel: Boolean;
begin
  result := not(FProgressForm.Visible);
end;

procedure TThreadScleit.saveRECT;
var
  i, j, pti: integer;
begin
  prStr1:=SAS_STR_Resolution+': '+inttostr(FMapSize.X)+'x'+inttostr(FMapSize.Y)+' '+SAS_STR_DivideInto+' '+inttostr(FSplitCount.X*FSplitCount.Y)+' '+SAS_STR_files;
  Synchronize(UpdateProgressFormStr1);

  prBar:=0;
  Synchronize(UpdateProgressFormBar);
  prStr2:=SAS_STR_Processed+' 0';
  Synchronize(UpdateProgressFormStr2);

  FCurrentFileName := FFilePath + FFileName + FFileExt;
  for i:=1 to FSplitCount.X do begin
    for j:=1 to FSplitCount.Y do begin
      FCurrentPieceRect.Left := FMapRect.Left + FMapPieceSize.X * (i-1);
      FCurrentPieceRect.Right := FMapRect.Left + FMapPieceSize.X * i;
      FCurrentPieceRect.Top := FMapRect.Top + FMapPieceSize.Y * (j-1);
      FCurrentPieceRect.Bottom := FMapRect.Top + FMapPieceSize.Y * j;

      if (FSplitCount.X > 1) or (FSplitCount.Y > 1) then begin
        FCurrentFileName := FFilePath + FFileName + '_'+inttostr(i)+'-'+inttostr(j) + FFileExt;
      end;

      for pti := 0 to FMapCalibrationList.Count - 1 do begin
        try
          (FMapCalibrationList.get(pti) as IMapCalibration).SaveCalibrationInfo(FCurrentFileName, FCurrentPieceRect.TopLeft, FCurrentPieceRect.BottomRight, FZoom - 1, FTypeMap.GeoConvert);
        except
          //TODO: Добавить сюда нормальную обработку ошибок.
        end;
      end;
      try
        if (UpperCase(FFileExt)='.ECW')or(UpperCase(FFileExt)='.JP2') then begin
          Save_ECW;
          continue;
        end else if (UpperCase(FFileExt)='.BMP') then begin
          Save_BMP;
          continue;
        end else begin
          Save_JPG;
        end;
      except
        On E:Exception do begin
          Message_:=E.Message;
          Synchronize(SynShowMessage);
          exit;
        end;
      end;
    end;
  end;
end;

procedure TThreadScleit.Execute;
begin
 saveRECT;
 Synchronize(UpdateProgressFormClose);
end;

function TThreadScleit.ReadLineECW(Line: cardinal; var LineR, LineG,
  LineB: PLineRGB): boolean;
var
  i,j,rarri,lrarri,p_x,p_y,Asx,Asy,Aex,Aey,starttile:integer;
  p_h:TPoint;
  p:PColor32array;
begin
  Result := True;
  if line<(256-sy) then begin
    starttile:=sy+line;
  end else begin
    starttile:=(line-(256-sy)) mod 256;
  end;
  if (starttile=0)or(line=0) then begin
    prBar:=line;
    Synchronize(UpdateProgressFormBar);
    prStr2:=SAS_STR_Processed+': '+inttostr(Round((line/(FMapPieceSize.Y))*100))+'%';
    Synchronize(UpdateProgressFormStr2);
    p_y:=(FCurrentPieceRect.Top+line)-((FCurrentPieceRect.Top+line) mod 256);
    p_x:=FCurrentPieceRect.Left-(FCurrentPieceRect.Left mod 256);
    p_h := FTypeMap.GeoConvert.Pos2OtherMap(Point(p_x,p_y), (FZoom - 1) + 8, FHTypeMap.GeoConvert);
    lrarri:=0;
    if line>(255-sy) then Asy:=0 else Asy:=sy;
    if (p_y div 256)=(FCurrentPieceRect.Bottom div 256) then Aey:=ey else Aey:=255;
    Asx:=sx;
    Aex:=255;
    while p_x<=FCurrentPieceRect.Right do begin
      FLLRect:=bounds(p_x,p_y,256,256);
      // запомнием координаты обрабатываемого тайла для случая если произойдет ошибка
      LastXY.X := p_x;
      LastXY.Y := p_y;
      if not(RgnAndRgn(FPoly, p_x+128, p_y+128, false)) then begin
        btmm.Clear(Color32(GState.BGround))
      end else begin
        btmm.Clear(Color32(GState.BGround));
        FTypeMap.LoadTileOrPreZ(btmm,p_x,p_y, FZoom,false, true);
        if FHTypeMap<>nil then begin
          btmh.Clear($FF000000);
          FHTypeMap.LoadTileOrPreZ(btmh,p_h.x,p_h.y, FZoom,false, True);
          btmh.DrawMode:=dmBlend;
          btmm.Draw(0,0-((p_h.y mod 256)),btmh);
          if p_h.y<>p_y then begin
            btmh.Clear($FF000000);
            FHTypeMap.LoadTileOrPreZ(btmh,p_h.x,p_h.y+256, FZoom, false, True);
            btmh.DrawMode:=dmBlend;
            btmm.Draw(0,256-(p_h.y mod 256),bounds(0,0,256,(p_h.y mod 256)),btmh);
          end;
        end;
      end;
      if FUsedReColor then Gamma(btmm);
      if FUsedMarks then Synchronize(DrawMarks2Tile);
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

procedure TThreadScleit.DrawMarks2Tile;
var LLRect:TExtendedRect;
begin
 LLRect:=FTypeMap.GeoConvert.PixelRect2LonLatRect(FLLRect,FZoom-1);
 FMain.LayerMapMarks.DoRedraw2Bitmap(btmm,FTypeMap,LLRect,FZoom)
end;

function TThreadScleit.ReadLineBMP(Line: cardinal;
  LineRGB: PLineRGBb): boolean;
var
  i,j,rarri,lrarri,p_x,p_y,Asx,Asy,Aex,Aey,starttile:integer;
  p_h:TPoint;
  p:PColor32array;
begin
  if line<(256-sy) then begin
    starttile:=sy+line
  end else begin
    starttile:=(line-(256-sy)) mod 256;
  end;
  if (starttile=0)or(line=0) then begin
    prBar:=line;
    Synchronize(UpdateProgressFormBar);
    if line=0 then begin
      prStr2:=SAS_STR_CreateFile
    end else begin
      prStr2:=SAS_STR_Processed+': '+inttostr(Round((line/(FMapPieceSize.Y))*100))+'%';
    end;
    Synchronize(UpdateProgressFormStr2);
    p_y:=(FCurrentPieceRect.Top+line)-((FCurrentPieceRect.Top+line) mod 256);
    p_x:=FCurrentPieceRect.Left-(FCurrentPieceRect.Left mod 256);
    p_h := FTypeMap.GeoConvert.Pos2OtherMap(Point(p_x,p_y), (Fzoom - 1) + 8, FHTypeMap.GeoConvert);
    lrarri:=0;
    if line>(255-sy) then Asy:=0 else Asy:=sy;
    if (p_y div 256)=(FCurrentPieceRect.Bottom div 256) then Aey:=ey else Aey:=255;
    Asx:=sx;
    Aex:=255;
    while p_x<=FCurrentPieceRect.Right do begin
      if not(RgnAndRgn(FPoly, p_x+128, p_y+128, false)) then begin
        btmm.Clear(Color32(GState.BGround))
      end else begin
        FLLRect:=bounds(p_x,p_y,256,256);
        btmm.Clear(Color32(GState.BGround));
        FTypeMap.LoadTileOrPreZ(btmm,p_x,p_y, FZoom, false, True);
        if FHTypeMap<>nil then begin
          btmh.Clear($FF000000);
          FHTypeMap.LoadTileOrPreZ(btmh,p_h.x,p_h.y, FZoom, false, True);
          btmh.DrawMode:=dmBlend;
          btmm.Draw(0,0-((p_h.y mod 256)),btmh);
          if p_h.y<>p_y then begin
            btmh.Clear($FF000000);
            FHTypeMap.LoadTileOrPreZ(btmh,p_h.x,p_h.y+256, FZoom, false, True);
            btmh.DrawMode:=dmBlend;
            btmm.Draw(0,256-(p_h.y mod 256),bounds(0,0,256,(p_h.y mod 256)),btmh);
          end;
        end;
      end;
      if FUsedReColor then Gamma(btmm);
      if FUsedMarks then Synchronize(DrawMarks2Tile);
      if (p_x+256)>FCurrentPieceRect.Right then Aex:=ex;
      for j:=Asy to Aey do begin
        p:=btmm.ScanLine[j];
        rarri:=lrarri;
        for i:=Asx to Aex do begin
          CopyMemory(@Array256BGR[j]^[rarri],Pointer(integer(p)+(i*4)),3);
          inc(rarri);
        end;
      end;
      lrarri:=rarri;
      Asx:=0;
      inc(p_x,256);
      inc(p_h.x,256);
    end;
  end;
  CopyMemory(LineRGB,Array256BGR^[starttile],(FCurrentPieceRect.Right-FCurrentPieceRect.Left)*3);
end;

procedure TThreadScleit.Save_ECW;
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
    ecw:=TECWWrite.Create;
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
    prStr1:=SAS_STR_Resolution+': '+inttostr((FMapSize.X))+'x'+inttostr((FMapSize.Y));
    Synchronize(UpdateProgressFormStr1);
    Datum := 'EPSG:' + IntToStr(FTypeMap.GeoConvert.GetDatumEPSG);
    Proj := 'EPSG:' + IntToStr(FTypeMap.GeoConvert.GetProjectionEPSG);
    Units := FTypeMap.GeoConvert.GetCellSizeUnits;
    CalculateWFileParams(
      FTypeMap.GeoConvert.PixelPos2LonLat(FCurrentPieceRect.TopLeft, FZoom - 1),
      FTypeMap.GeoConvert.PixelPos2LonLat(FCurrentPieceRect.BottomRight, FZoom - 1),
      FMapPieceSize.X, FMapPieceSize.Y, FTypeMap.GeoConvert,
      CellIncrementX,CellIncrementY,OriginX,OriginY
    );
    errecw:=ecw.Encode(FCurrentFileName,FMapPieceSize.X, FMapPieceSize.Y, 101-Fsaveas.QualitiEdit.Value, COMPRESS_HINT_BEST, ReadLineECW, IsCancel, nil,
    Datum,Proj,Units,CellIncrementX,CellIncrementY,OriginX,OriginY);
    if (errecw>0)and(errecw<>52) then begin
      path:=FTypeMap.GetTileShowName(LastXY.x, LastXY.Y, FZoom);
      Message_:=SAS_ERR_Save+' '+SAS_ERR_Code+inttostr(errecw)+#13#10+path;
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
    ecw.Free;
  end;
end;

procedure TThreadScleit.Save_BMP;
var
  k: integer;
begin
  sx:=(FCurrentPieceRect.Left mod 256);
  sy:=(FCurrentPieceRect.Top mod 256);
  ex:=(FCurrentPieceRect.Right mod 256);
  ey:=(FCurrentPieceRect.Bottom mod 256);
  try
    btmm:=TBitmap32.Create;
    btmh:=TBitmap32.Create;
    btmm.Width:=256;
    btmm.Height:=256;
    btmh.Width:=256;
    btmh.Height:=256;
    getmem(Array256BGR,256*sizeof(P256ArrayBGR));
    for k:=0 to 255 do getmem(Array256BGR[k],(FMapPieceSize.X+1)*3);
    prStr1:=SAS_STR_Resolution+': '+inttostr(FMapPieceSize.X)+'x'+inttostr(FMapPieceSize.Y);
    Synchronize(UpdateProgressFormStr1);
    SaveBMP(FMapPieceSize.X, FMapPieceSize.Y, FCurrentFileName, ReadLineBMP, IsCancel);
  finally
    {$IFDEF VER80}
      for k:=0 to 255 do freemem(Array256BGR[k],(FMapPieceSize.X+1)*3);
      freemem(Array256BGR,256*((FMapPieceSize.X+1)*3));
    {$ELSE}
      for k:=0 to 255 do freemem(Array256BGR[k]);
      FreeMem(Array256BGR);
    {$ENDIF}
    btmm.Free;
    btmh.Free;
  end;
end;

procedure TThreadScleit.Save_JPG;
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
    getmem(Array256BGR,256*sizeof(P256ArrayBGR));
    for k:=0 to 255 do getmem(Array256BGR[k],(iWidth+1)*3);
    prStr1:=SAS_STR_Resolution+': '+inttostr(iWidth)+'x'+inttostr(iHeight);
    Synchronize(UpdateProgressFormStr1);
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
    new(jcprops.DIBBytes);
    GetMem(jcprops.DIBBytes,(iWidth*3+ (iWidth mod 4))*iHeight);
    if jcprops.DIBBytes<>nil then begin
      for k:=0 to iHeight-1 do begin
        ReadLineBMP(k,Pointer(integer(jcprops.DIBBytes)+(((iWidth*3+ (iWidth mod 4))*iHeight)-(iWidth*3+ (iWidth mod 4))*(k+1))));
        if IsCancel then break;
      end;
    end else begin
      Message_:=SAS_ERR_Memory+'.'+#13#10+SAS_ERR_UseADifferentFormat;
      Synchronize(SynShowMessage);
      exit;
    end;
    jcprops.JPGFile := PChar(FCurrentFileName);
    jcprops.JPGWidth := iWidth;
    jcprops.JPGHeight := iHeight;
    jcprops.JPGChannels := 3;
    jcprops.JPGColor := IJL_YCBCR;
    jcprops.jquality := FSaveAs.QualitiEdit.Value;
    ijlWrite(@jcprops,IJL_JFILE_WRITEWHOLEIMAGE);
  Finally
    freemem(jcprops.DIBBytes,iWidth*iHeight*3);
    for k:=0 to 255 do freemem(Array256BGR[k],(iWidth+1)*3);
    freemem(Array256BGR,256*((iWidth+1)*3));
    ijlFree(@jcprops);
    btmm.Free;
    btmh.Free;
  end;
end;

end.
