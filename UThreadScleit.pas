unit UThreadScleit;

interface

uses
  Windows,
  Forms,
  SysUtils,
  Classes,
  Dialogs,
  Graphics,
  math,
  GR32,
  Jpeg,
  ijl,
  ECWWrite,
  UMapType,
  UImgFun,
  UGeoFun,
  bmpUtil,
  UResStrings,
  unit4;

type
  PRow = ^TRow;
  TRow = array[0..0] of byte;

  P256rgb = ^T256rgb;
  T256rgb = array[0..255] of PRow;

  TBGR = record
   b,g,r:byte;
  end;

  PArrayBGR = ^TArrayBGR;
  TArrayBGR = array [0..0] of TBGR;

  P256ArrayBGR = ^T256ArrayBGR;
  T256ArrayBGR = array[0..255] of PArrayBGR;

  ThreadScleit = class(TThread)
  public
    ProcessTiles:integer;
    PolyMin:TPoint;
    PolyMax:TPoint;
    Fprogress: TFprogress2;
  private
    Array256BGR:P256ArrayBGR;
    sx,ex,sy,ey:integer;
    Rarr:P256rgb;
    Garr:P256rgb;
    Barr:P256rgb;
    Poly0:TPoint;
    Poly1:TPoint;
    Poly:array of TPoint;
    Zoom:byte;
    typemap,Htypemap:TMapType;
    colors:byte;
    numTlg:integer;
    numTlv:integer;
    ToOzi:boolean;
    ToTab:boolean;
    ToWorld:boolean;
    ecw:TECWWrite;
    btmm:TBitmap32;
    btmh:TBitmap32;
    usedReColor:boolean;
    FName:string;
    prStr1,prStr2,prCaption:string;
    prBar:integer;
    Message_:string;
    LastXY: TPoint;
  protected
    procedure UpdateProgressFormCapt;
    procedure UpdateProgressFormBar;
    procedure UpdateProgressFormStr1;
    procedure UpdateProgressFormStr2;
    procedure UpdateProgressFormClose;
    procedure SynShowMessage;
    procedure Execute; override;
    procedure saveRECT;
  public
    constructor Create(CrSusp:Boolean;AFName:string; APolygon_:array of TPoint;numTilesG,numTilesV:integer;Azoom:byte;Atypemap,AHtypemap:TMapType;Acolors:byte;AToOzi,AToTab,AToWorld,AusedReColor:boolean);
  end;

implementation
uses
  StrUtils,
  ECWWriter,
  ECWReader,
  unit1,
  usaveas,
  uozi;

procedure ThreadScleit.SynShowMessage;
begin
 ShowMessage(Message_);
end;

procedure ThreadScleit.UpdateProgressFormClose;
begin
 fprogress.Close;
end;

procedure ThreadScleit.UpdateProgressFormCapt;
begin
 fprogress.Caption:=prCaption;
end;

procedure ThreadScleit.UpdateProgressFormStr1;
begin
 fprogress.MemoInfo.Lines[0]:=prStr1;
end;

procedure ThreadScleit.UpdateProgressFormStr2;
begin
 fprogress.MemoInfo.Lines[1]:=prStr2;
end;

procedure ThreadScleit.UpdateProgressFormBar;
begin
 fprogress.ProgressBar1.Progress1:=prBar;
end;


function ReadLineBMP(Sender:TObject;Line:cardinal;LineRGB:PLineRGBb):boolean;
var i,j,rarri,lrarri,p_x,p_y,Asx,Asy,Aex,Aey,starttile:integer;
    p_h:TPoint;
    p:PColor32array;
    VThread: ThreadScleit;
begin
 VThread := ThreadScleit(Sender);
 if line<(256-VThread.sy) then starttile:=VThread.sy+line
                  else starttile:=(line-(256-VThread.sy)) mod 256;
 if (starttile=0)or(line=0) then
  begin

   VThread.prBar:=line;
   VThread.Synchronize(VThread.UpdateProgressFormBar);
   if line=0 then VThread.prStr2:=SAS_STR_CreateFile
             else VThread.prStr2:=SAS_STR_Processed+': '+inttostr(Round((line/(VThread.Poly1.Y-VThread.Poly0.Y))*100))+'%';
   VThread.Synchronize(VThread.UpdateProgressFormStr2);
   p_y:=(VThread.Poly0.Y+line)-((VThread.Poly0.Y+line) mod 256);
   p_x:=VThread.poly0.x-(VThread.poly0.x mod 256);
   p_h := VThread.typemap.GeoConvert.Pos2OtherMap(Point(p_x,p_y), (VThread.zoom - 1) + 8, VThread.Htypemap.GeoConvert);
   lrarri:=0;
   if line>(255-VThread.sy) then Asy:=0 else Asy:=VThread.sy;
   if (p_y div 256)=(VThread.poly1.y div 256) then Aey:=VThread.ey else Aey:=255;
   Asx:=VThread.sx;
   Aex:=255;
   while p_x<=VThread.poly1.x do
    begin
     if not(RgnAndRgn(VThread.Poly,p_x+128,p_y+128,false)) then VThread.btmm.Clear(clSilver)
     else
     begin
     VThread.btmm.Clear(clSilver);
     if (VThread.typemap.Tileexists(p_x,p_y,VThread.zoom)) then begin
                                 if not(VThread.typemap.LoadTile(VThread.btmm,p_x,p_y,VThread.zoom,false))
                                  then Fmain.loadpre(VThread.btmm,p_x,p_y,VThread.zoom,VThread.typemap);
                                end
                           else Fmain.loadpre(VThread.btmm,p_x,p_y,VThread.zoom,VThread.typemap);
     if VThread.usedReColor then Gamma(VThread.btmm);
     if VThread.Htypemap<>nil then
      begin
       VThread.btmh.Clear(clBlack);
       VThread.btmh.Draw(0,(p_h.y mod 256),bounds(0,0,256,256-(p_h.y mod 256)),VThread.btmm);
       if (VThread.Htypemap.Tileexists(p_h.x,p_h.y,VThread.zoom)) then VThread.Htypemap.LoadTile(VThread.btmh,p_h.x,p_h.y,VThread.zoom,false);
       VThread.btmm.Draw(0,0-((p_h.y mod 256)),VThread.btmh);
       if p_h.y<>p_y then
        begin
         VThread.btmh.Clear(clBlack);
         VThread.btmh.Draw(0,0,bounds(0,256-(p_h.y mod 256),256,(p_h.y mod 256)),VThread.btmm);
         if (VThread.Htypemap.Tileexists(p_h.x,p_h.y+256,VThread.zoom)) then VThread.Htypemap.LoadTile(VThread.btmh,p_h.x,p_h.y+256,VThread.zoom,false);
         VThread.btmm.Draw(0,256-(p_h.y mod 256),bounds(0,0,256,(p_h.y mod 256)),VThread.btmh);
        end;
      end;
     end;
     if (p_x+256)>VThread.poly1.x
      then Aex:=VThread.ex;
     for j:=Asy to Aey do
      begin
       p:=VThread.btmm.ScanLine[j];
       rarri:=lrarri;
       for i:=Asx to Aex do
        begin
         CopyMemory(@VThread.Array256BGR[j]^[rarri],Pointer(integer(p)+(i*4)),3);
         inc(rarri);
        end;
      end;
     lrarri:=rarri;
     Asx:=0;
     inc(p_x,256);
     inc(p_h.x,256);
    end;
  end;
 CopyMemory(LineRGB,VThread.Array256BGR^[starttile],(VThread.poly1.x-VThread.poly0.x)*3);
end;

function ReadLine(Sender:TObject;Line:cardinal;var LineR,LineG,LineB:PLineRGB):boolean;
var i,j,rarri,lrarri,p_x,p_y,Asx,Asy,Aex,Aey,starttile:integer;
    p_h:TPoint;
    p:PColor32array;
    VThread: ThreadScleit;
begin
 VThread := ThreadScleit(Sender);
 if line<(256-VThread.sy) then starttile:=VThread.sy+line
                  else starttile:=(line-(256-VThread.sy)) mod 256;
 if (starttile=0)or(line=0) then
  begin
   VThread.prBar:=line;
   VThread.Synchronize(VThread.UpdateProgressFormBar);
   VThread.prStr2:=SAS_STR_Processed+': '+inttostr(Round((line/(VThread.Poly1.Y-VThread.Poly0.Y))*100))+'%';
   VThread.Synchronize(VThread.UpdateProgressFormStr2);
   p_y:=(VThread.Poly0.Y+line)-((VThread.Poly0.Y+line) mod 256);
   p_x:=VThread.poly0.x-(VThread.poly0.x mod 256);
   p_h := VThread.typemap.GeoConvert.Pos2OtherMap(Point(p_x,p_y), (VThread.zoom - 1) + 8, VThread.Htypemap.GeoConvert);
   lrarri:=0;
   if line>(255-VThread.sy) then Asy:=0 else Asy:=VThread.sy;
   if (p_y div 256)=(VThread.poly1.y div 256) then Aey:=VThread.ey else Aey:=255;
   Asx:=VThread.sx;
   Aex:=255;
   while p_x<=VThread.poly1.x do
    begin
     // запомнием координаты обрабатываемого тайла для случая если произойдет ошибка
     VThread.LastXY.X := p_x;
     VThread.LastXY.Y := p_y;
     if not(RgnAndRgn(VThread.Poly,p_x+128,p_y+128,false)) then VThread.btmm.Clear(clSilver)
     else
     begin
     VThread.btmm.Clear(clSilver);
     if (VThread.typemap.Tileexists(p_x,p_y,VThread.zoom))
      then begin
            if not(VThread.typemap.LoadTile(VThread.btmm,p_x,p_y,VThread.zoom,false))
             then Fmain.loadpre(VThread.btmm,p_x,p_y,VThread.zoom,VThread.typemap);
           end
      else Fmain.loadpre(VThread.btmm,p_x,p_y,VThread.zoom,VThread.typemap);
     if VThread.usedReColor then Gamma(VThread.btmm);
     if VThread.Htypemap<>nil then
      begin
       VThread.btmh.Clear(clBlack);
       VThread.btmh.Draw(0,(p_h.y mod 256),bounds(0,0,256,256-(p_h.y mod 256)),VThread.btmm);
       if (VThread.Htypemap.Tileexists(p_h.x,p_h.y,VThread.zoom)) then VThread.Htypemap.LoadTile(VThread.btmh,p_h.x,p_h.y,VThread.zoom,false);
       VThread.btmm.Draw(0,0-((p_h.y mod 256)),VThread.btmh);
       if p_h.y<>p_y then
        begin
         VThread.btmh.Clear(clBlack);
         VThread.btmh.Draw(0,0,bounds(0,256-(p_h.y mod 256),256,(p_h.y mod 256)),VThread.btmm);
         if (VThread.Htypemap.Tileexists(p_h.x,p_h.y+256,VThread.zoom)) then VThread.Htypemap.LoadTile(VThread.btmh,p_h.x,p_h.y+256,VThread.zoom,false);
         VThread.btmm.Draw(0,256-(p_h.y mod 256),bounds(0,0,256,(p_h.y mod 256)),VThread.btmh);
        end;
      end;
     end;
     if (p_x+256)>VThread.poly1.x
      then Aex:=VThread.ex;
     for j:=Asy to Aey do
      begin
       p:=VThread.btmm.ScanLine[j];
       rarri:=lrarri;
       for i:=Asx to Aex do
        begin
         VThread.Rarr^[j]^[rarri]:=((cardinal(p^[i]) shl 8) shr 24);
         VThread.Garr^[j]^[rarri]:=((cardinal(p^[i]) shl 16) shr 24);
         VThread.Barr^[j]^[rarri]:=((cardinal(p^[i]) shl 24) shr 24);
         inc(rarri);
        end;
      end;
     lrarri:=rarri;
     Asx:=0;
     inc(p_x,256);
     inc(p_h.x,256);
    end;
  end;
 for i:=0 to (VThread.poly1.x-VThread.poly0.x)-1 do
  begin
   LineR^[i]:=VThread.Rarr^[starttile]^[i];
   LineG^[i]:=VThread.Garr^[starttile]^[i];
   LineB^[i]:=VThread.Barr^[starttile]^[i];
  end;
end;

procedure ThreadScleit.saveRECT;
var p_x,p_y,i,j,k,errecw:integer;
    p_h:TPoint;
    scachano:integer;
    btm:TBitmap32;
    err,Fnamebuf:string;
    jpg:TJpegImage;
    Datum,Proj:string;
    CellIncrementX,CellIncrementY,OriginX,OriginY:extended;
    Tlbfull,TlbTile:TBitmap32;
    b:TBitmap;
    Units:CellSizeUnits;
    jcprops : TJPEG_CORE_PROPERTIES;
    iNChannels,iWidth,iHeight:integer;
    path: string;
begin
 prCaption:='Склеить: '+inttostr((PolyMax.x-PolyMin.x-1) div 256+1)+'x'
                       +inttostr((PolyMax.y-PolyMin.y-1) div 256+1)
                       +'('+inttostr(ProcessTiles)+') '+SAS_STR_files;
 Synchronize(UpdateProgressFormCapt);
 prStr1:=SAS_STR_Resolution+': '+inttostr((PolyMax.x-PolyMin.x))+'x'+inttostr((PolyMax.y-PolyMin.y))+' '+SAS_STR_DivideInto+' '+inttostr(numTlg*numTlv)+' '+SAS_STR_files;
 Synchronize(UpdateProgressFormStr1);

 FProgress.ProgressBar1.Max:=0;
 for i:=1 to numTlg do
  for j:=1 to numTlv do
   begin
    Poly0.X:=PolyMin.x+((PolyMax.x-PolyMin.x)div numTlg)*(i-1);
    Poly1.X:=PolyMin.x+((PolyMax.x-PolyMin.x)div numTlg)*(i-1)+((PolyMax.x-PolyMin.x)div numTlg);
    Poly0.Y:=PolyMin.y+((PolyMax.y-PolyMin.y)div numTlv)*(j-1);
    Poly1.Y:=PolyMin.y+((PolyMax.y-PolyMin.y)div numTlv)*(j-1)+((PolyMax.y-PolyMin.y)div numTlv);
    FProgress.ProgressBar1.Max:=FProgress.ProgressBar1.Max+(((PolyMax.x-PolyMin.x)div 256)+2)*(((PolyMax.y-PolyMin.y)div 256)+2);
   end;
 prBar:=0;
 Synchronize(UpdateProgressFormBar);
 prStr2:=SAS_STR_Processed+' '+inttostr(FProgress.ProgressBar1.Progress1);
 Synchronize(UpdateProgressFormStr2);

 Fnamebuf:=fname;
 for i:=1 to numTlg do
  for j:=1 to numTlv do
  begin
   Poly0.X:=PolyMin.x+((PolyMax.x-PolyMin.x)div numTlg)*(i-1);
   Poly1.X:=PolyMin.x+((PolyMax.x-PolyMin.x)div numTlg)*(i-1)+((PolyMax.x-PolyMin.x)div numTlg);
   Poly0.Y:=PolyMin.y+((PolyMax.y-PolyMin.y)div numTlv)*(j-1);
   Poly1.Y:=PolyMin.y+((PolyMax.y-PolyMin.y)div numTlv)*(j-1)+((PolyMax.y-PolyMin.y)div numTlv);

   fname:=Fnamebuf;
   if (numTlg>1)or(numTlv>1) then Insert('_'+inttostr(i)+'-'+inttostr(j),fname,posex('.',fname,length(fname)-4));

   if toOzi then toOziMap(fname,poly0,poly1,zoom,typemap);
   if toTab then toTabMap(fname,poly0,poly1,zoom,typemap);
   if toWorld then begin
                    toWorldFiles(fname,poly0,poly1,zoom,typemap);
                    toPrj(fname,typemap);
                    toAuxXml(fname,typemap);
                   end;

   if (UpperCase(ExtractFileExt(fname))='.ECW')or(UpperCase(ExtractFileExt(fname))='.JP2') then
   begin
   sx:=(Poly0.X mod 256);
   sy:=(Poly0.Y mod 256);
   ex:=(Poly1.X mod 256);
   ey:=(Poly1.Y mod 256);
   try
   ecw:=TECWWrite.Create;
   btmm:=TBitmap32.Create;
   btmh:=TBitmap32.Create;
   btmm.Width:=256;
   btmm.Height:=256;
   btmh.Width:=256;
   btmh.Height:=256;
   getmem(Rarr,256*sizeof(PRow));
   for k:=0 to 255 do getmem(Rarr[k],(Poly1.X-Poly0.X+1)*sizeof(byte));
   getmem(Garr,256*sizeof(PRow));
   for k:=0 to 255 do getmem(Garr[k],(Poly1.X-Poly0.X+1)*sizeof(byte));
   getmem(Barr,256*sizeof(PRow));
   for k:=0 to 255 do getmem(Barr[k],(Poly1.X-Poly0.X+1)*sizeof(byte));
   FProgress.ProgressBar1.Max:=Poly1.y-Poly0.y;
   prStr1:=SAS_STR_Resolution+': '+inttostr((poly1.x-poly0.x))+'x'+inttostr((poly1.y-poly0.y));
   Synchronize(UpdateProgressFormStr1);
   case TypeMap.projection of
    1: begin
        Datum:='EPSG:7059';
        Proj:='EPSG:3785';
        Units:=ECW_CELL_UNITS_METERS;
       end;
    2: begin
        Datum:='EPSG:3395';
        Proj:='EPSG:3395';
        Units:=ECW_CELL_UNITS_METERS;
       end;
    3: begin
        Datum:='EPSG:4326';
        Proj:='EPSG:4326';
        Units:=ECW_CELL_UNITS_DEGREES;
       end;
   end;
   CalculateMercatorCoordinates(typemap.GeoConvert.Pos2LonLat(Poly0,(Zoom - 1) + 8),typemap.GeoConvert.Pos2LonLat(Poly1,(Zoom - 1) + 8),
                                Poly1.X-Poly0.X,Poly1.y-Poly0.y,TypeMap,CellIncrementX,CellIncrementY,OriginX,OriginY,Units);
   errecw:=ecw.Encode(self,fname,Poly1.X-Poly0.X,Poly1.y-Poly0.y,101-Fsaveas.QualitiEdit.Value, COMPRESS_HINT_BEST, @ReadLine, nil,
             Datum,Proj,Units,CellIncrementX,CellIncrementY,OriginX,OriginY);
   if (errecw>0)and(errecw<>52) then
    begin
     //Имя файла для вывода в сообщении. Заменить на обобобщенное имя тайла
     path:=typemap.GetTileFileName(LastXY.x, LastXY.Y, zoom);
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
   continue;
   end;

   if (UpperCase(ExtractFileExt(fname))='.BMP') then
   begin
   sx:=(Poly0.X mod 256);
   sy:=(Poly0.Y mod 256);
   ex:=(Poly1.X mod 256);
   ey:=(Poly1.Y mod 256);
   try
   btmm:=TBitmap32.Create;
   btmh:=TBitmap32.Create;
   btmm.Width:=256;
   btmm.Height:=256;
   btmh.Width:=256;
   btmh.Height:=256;
   getmem(Array256BGR,256*sizeof(P256ArrayBGR));
   for k:=0 to 255 do getmem(Array256BGR[k],(Poly1.X-Poly0.X+1)*3);
   FProgress.ProgressBar1.Max:=Poly1.y-Poly0.y;
   prStr1:=SAS_STR_Resolution+': '+inttostr((poly1.x-poly0.x))+'x'+inttostr((poly1.y-poly0.y));
   Synchronize(UpdateProgressFormStr1);
   SaveBMP(self, Poly1.X-Poly0.X,Poly1.y-Poly0.y, fname,@ReadLineBMP);
   finally
   {$IFDEF VER80}
   for k:=0 to 255 do freemem(Array256BGR[k],(Poly1.X-Poly0.X+1)*3);
   freemem(Array256BGR,256*((Poly1.X-Poly0.X+1)*3));
   {$ELSE}
   for k:=0 to 255 do freemem(Array256BGR[k]);
   FreeMem(Array256BGR);
   {$ENDIF}
   btmm.Free;
   btmh.Free;
   ecw.Free;
   end;
   continue;
   end;

   try
   try
    sx:=(Poly0.X mod 256);
    sy:=(Poly0.Y mod 256);
    ex:=(Poly1.X mod 256);
    ey:=(Poly1.Y mod 256);
    iWidth  := poly1.x-poly0.x;
    iHeight := poly1.y-poly0.y;
    getmem(Array256BGR,256*sizeof(P256ArrayBGR));
    for k:=0 to 255 do getmem(Array256BGR[k],(iWidth+1)*3);
    FProgress.ProgressBar1.Max:=Poly1.y-Poly0.y;
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
    if jcprops.DIBBytes<>nil then
    for k:=0 to iHeight-1 do
     begin
       ReadLineBMP(self,k,Pointer(integer(jcprops.DIBBytes)+(((iWidth*3+ (iWidth mod 4))*iHeight)-(iWidth*3+ (iWidth mod 4))*(k+1))));
       if not(Fprogress.Visible) then break;
     end
    else
     begin
      Message_:=SAS_ERR_Memory;
      Synchronize(SynShowMessage);
      exit;
     end;
    jcprops.JPGFile := PChar(fname);
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
   except
    On E:Exception do
    begin
     Message_:=E.Message;
     Synchronize(SynShowMessage);
     exit;
    end;
   end;
  end;
end;

constructor ThreadScleit.Create(CrSusp:Boolean;AFName:string;APolygon_:array of TPoint;numTilesG,numTilesV:integer;Azoom:byte;Atypemap,AHtypemap:TMapType;Acolors:byte;AToOzi,AToTab,AToWorld,AusedReColor:boolean);
var i:integer;
begin
  inherited Create(CrSusp);
  Application.CreateForm(TFProgress2, FProgress);
  FProgress.Visible:=true;
  FName:=AFName;
  numTlg:=numTilesG;
  numTlv:=numTilesV;
  ToWorld:=AToWorld;
  ToOzi:=AToOzi;
  ToTab:=AToTab;
  usedReColor:=AusedReColor;
  for i:=1 to length(APolygon_) do
   begin
    setlength(Poly,i);
    poly[i-1]:=Apolygon_[i-1];
   end;
  zoom:=Azoom;
  typemap:=Atypemap;
  Htypemap:=AHtypemap;
  colors:=Acolors;
end;

procedure ThreadScleit.Execute;
begin
 saveRECT;
 Synchronize(UpdateProgressFormClose);
end;

end.
