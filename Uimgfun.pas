unit Uimgfun;
                               
interface
uses IJL,Classes,pngimage,StrUtils,SysUtils,Windows,Types,UMaptype,RxGIF,Math,GR32,jpeg,graphics;

const
  FILE_DOES_NOT_EXIST = DWORD(-1);

var
  CacheList:TStringList;
  defoultMap:TBitmap;// TPNGObject;
  function SaveTileInCache(btm:TObject;path:string):boolean;
  function TileExists(path:string):boolean;
  function LoadTilefromCache(btm:Tobject;path:string;caching:boolean):boolean;
  function DelFile(path:string):boolean;
  function Copy_File(pathfrom,pathto:string;zamena:boolean):boolean;
  procedure SetDefoultMap;
  function PNGintoBitmap32(destBitmap: TBitmap32; PNGObject: TPNGObject): boolean;
  procedure CropPNGImage(var png:TPNGObject;dx,dy,cx,cy:integer);
  function InStr(I: Integer): string;
  Procedure ClearCache;

implementation
uses unit1;

procedure CropPNGImage(var png:TPNGObject;dx,dy,cx,cy:integer);
var p:TPNGObject;
    j:integer;
begin
 p:= tpngobject.createblank(png.Header.ColorType, png.Header.BitDepth, cx, cy);
 for j:=0 to cy-1 do
  begin
   CopyMemory(p.Scanline[j],Pointer(longint(png.Scanline[j+dy])+(dx*3)),cx*3);
   CopyMemory(p.AlphaScanline[j],Pointer(longint(png.AlphaScanline[j+dy])+dx),cx);
  end;
 png:=p;
end;

function PNGintoBitmap32(destBitmap: TBitmap32; PNGObject: TPNGObject): boolean;
var TransparentColor: TColor32;
    PixelPtr: PColor32;
    AlphaPtr: PByte;
    X, Y: Integer;
begin
 try
  result:=false;
  //destBitmap.ResetAlpha;
  case PNGObject.TransparencyMode of
    ptmPartial:
     begin
      if (PNGObject.Header.ColorType in [COLOR_GRAYSCALEALPHA,COLOR_RGBALPHA]) then
       begin
        destBitmap.Assign(PNGObject);
        PixelPtr:=PColor32(@destBitmap.Bits[0]);
        for Y:=0 to destBitmap.Height-1 do
         begin
          AlphaPtr:=PByte(PNGObject.AlphaScanline[Y]);
          for X:=0 to destBitmap.Width-1 do
           begin
            PixelPtr^:=(PixelPtr^ and $00FFFFFF)or(TColor32(AlphaPtr^)shl 24);
            Inc(PixelPtr);
            Inc(AlphaPtr);
           end;
         end;
       end;
      if (PNGObject.Header.ColorType in [COLOR_PALETTE]) then
       begin
        TransparentColor:=Color32(PNGObject.TransparentColor);
        if PNGObject.Chunks.Item[3].Index=0 then destBitmap.Height:=destBitmap.Height;
        PixelPtr:=PColor32(@destBitmap.Bits[0]);
        for Y:=0 to destBitmap.Height-1 do
        begin
        AlphaPtr:=PByte(PNGObject.Scanline[Y]);
        for X:=0 to (destBitmap.Width-1) do
         begin
          if AlphaPtr^=0 then PixelPtr^:=PixelPtr^ and $00000000
                         else PixelPtr^:=Color32(PNGObject.Pixels[X,Y]);
          Inc(PixelPtr);
          Inc(AlphaPtr);
         end;
        end;
       end;
     end;
    ptmBit:
      begin
       destBitmap.Assign(PNGObject);
       TransparentColor:=Color32(PNGObject.TransparentColor);
       PixelPtr:=PColor32(@destBitmap.Bits[0]);
       for X:=0 to ((destBitmap.Height)*(destBitmap.Width))-1 do
        begin
         if PixelPtr^=TransparentColor then PixelPtr^:=PixelPtr^ and $00FFFFFF;
         Inc(PixelPtr);
        end;
      end;
  end;
  result:=true;
 except
 end;
end;

procedure SetDefoultMap;
var b:TPNGObject;
begin
 b:=TPNGObject.Create;
 b.LoadFromResourceName(HInstance, 'MAINMAP');
 DefoultMap:=TBitmap.Create;
 DefoultMap.Assign(b);
 b.LoadFromResourceName(HInstance, 'ICONI');
 Sm_Map.PlusButton:=TBitmap32.Create;
 PNGintoBitmap32(Sm_Map.PlusButton,b);
 Sm_Map.PlusButton.DrawMode:=dmTransparent;
 b.LoadFromResourceName(HInstance, 'ICONII');
 Sm_Map.MinusButton:=TBitmap32.Create;
 Sm_Map.MinusButton.DrawMode:=dmTransparent;
 PNGintoBitmap32(Sm_Map.MinusButton,b);
 b.LoadFromResourceName(HInstance, 'ICONIII');
 GOToSelIcon:=TBitmap32.Create;
 PNGintoBitmap32(GOToSelIcon,b);
 GOToSelIcon.DrawMode:=dmBlend;
 FreeAndNil(b);
end;

function SaveTileInCache(btm:TObject;path:string):boolean;
var
    Jpg_ex:TJpegImage;
    png_ex:TPNGObject;
    Gif_ex:TGIFImage;
    btm_ex:TBitmap;
begin
 if (btm is TBitmap32) then
  begin
   btm_ex:=TBitmap.Create;
   btm_ex.Assign(btm as TBitmap32);
   if UpperCase(ExtractFileExt(path))='.JPG' then
    begin
     Jpg_ex:=TJpegImage.Create;
     Jpg_ex.CompressionQuality:=85;
     Jpg_ex.Assign(btm_ex);
     Jpg_ex.SaveToFile(path);
     Jpg_ex.Free;
    end;
   if UpperCase(ExtractFileExt(path))='.GIF' then
    begin
     Gif_ex:=TGifImage.Create;
     Gif_ex.Assign(btm_ex);
     Gif_ex.SaveToFile(path);
     Gif_ex.Free;
    end;
   if UpperCase(ExtractFileExt(path))='.PNG' then
    begin
     PNG_ex:=TPNGObject.Create;
     PNG_ex.Assign(btm_ex);
     PNG_ex.SaveToFile(path);
     PNG_ex.Free;
    end;
   if UpperCase(ExtractFileExt(path))='.BMP' then btm_ex.SaveToFile(path);
   btm_ex.Free;
  end;
 if (btm is TJPEGimage) then TJPEGimage(btm).SaveToFile(path) else
 if (btm is TPNGObject) then TPNGObject(btm).SaveToFile(path) else
 if (btm is TMemoryStream) then TMemoryStream(btm).SaveToFile(path) else
// if (btm is TLinearBitmap) then TLinearBitmap(btm).SaveToFile(path);
 if (btm is TPicture) then TPicture(btm).SaveToFile(path);
end;

function InStr(I: Integer): string;
var
  S: string;
begin
  Str(I, S);
  InStr := S;
end;

function TileExists(path:string):boolean;
begin
//  result:=GetFileAttributes(@path[1])<>FILE_DOES_NOT_EXIST;
  result:=Fileexists(path);
end;

function DelFile(path:string):boolean;
begin
 result:=DeleteFile(PChar(path));
end;

function Copy_File(pathfrom,pathto:string;zamena:boolean):boolean;
begin
 CopyFile(Pchar(pathfrom),Pchar(pathto),zamena);
end;

function GetFileSize(namefile: string): Integer;
var InfoFile: TSearchRec;
begin
  if FindFirst(namefile, faAnyFile, InfoFile) <> 0
    then Result := -1
    else Result := InfoFile.Size;
  SysUtils.FindClose(InfoFile);
end;

//ABGR
//ARGB
procedure RGBA2BGRA2(pData : Pointer; Width, Height : Integer);
var W, H : Integer;
    p : PInteger;
begin
  p := PInteger(pData);
  for H := 0 to Height-1 do
  begin
    for W := 0 to Width-1 do
    begin
//      p^:=(integer(byte(p^ shr 24)) shl 24) or byte(p^ shr 16) or
//          (integer(byte(p^ shr 8)) shl 8) or (integer(byte(p^)) shl 16);
      p^:= (byte(p^ shr 24) shl 24) or byte(p^ shr 16) or
          (integer(byte(p^ shr 8)) shl 8) or (integer(byte(p^)) shl 16);
      Inc(p);
    end;
  end;
end;

function LoadJPG32(FileName : string;Btm:TBitmap32):boolean;
const
  sRead : array [Boolean] of String = ('JFILE_READ = ','JBUFF_READ = ');
var
  iWidth, iHeight, iNChannels : Integer;
  iStatus : Integer;
  pBuf : PByte;
  iIndex : Integer;
  R : TRect;
  pfd: PIXELFORMATDESCRIPTOR;
  jcprops : TJPEG_CORE_PROPERTIES;
//  DIB : TDIBSection;
begin
 try
    result:=true;
    iStatus := ijlInit(@jcprops);
    if iStatus < 0 then
     begin
      result:=false;
      exit;
     end;
//      raise Exception.Create('Own: Error in IJL function.');
    iIndex := 0;
    jcprops.JPGFile := PChar(FileName);
    iStatus := ijlRead(@jcprops,IJL_JFILE_READPARAMS);
    if iStatus < 0 then
     begin
      result:=false;
      exit;
     end;
//      raise Exception.Create('Own: Error in IJL function.');
    iWidth := jcprops.JPGWidth;
    iHeight := jcprops.JPGHeight;
    iNChannels := 4;
    Btm.SetSize(iWidth,iHeight);
//    GetObject(Btm.BitmapHandle,SizeOf(DIB),@DIB);
    jcprops.DIBWidth := iWidth;
    jcprops.DIBHeight := iHeight;
    jcprops.DIBChannels := iNChannels;
    jcprops.DIBColor := IJL_RGBA_FPX;
    jcprops.DIBPadBytes := ((((iWidth*iNChannels)+3) div 4)*4)-(iWidth*iNChannels);
    jcprops.DIBBytes := PByte(Btm.Bits);// PByte(DIB.dsBm.bmBits);
    if (jcprops.JPGChannels = 3) then
      jcprops.JPGColor := IJL_YCBCR
    else if (jcprops.JPGChannels = 4) then
      jcprops.JPGColor := IJL_YCBCRA_FPX
    else if (jcprops.JPGChannels = 1) then
      jcprops.JPGColor := IJL_G
    else
    begin
      jcprops.DIBColor := TIJL_COLOR (IJL_OTHER);
      jcprops.JPGColor := TIJL_COLOR (IJL_OTHER);
    end;
    iStatus := ijlRead(@jcprops,IJL_JFILE_READWHOLEIMAGE);
    if iStatus < 0 then
     begin
      result:=false;
      exit;
     end;
//      raise Exception.Create('Own: Error in IJL function.');
    if jcprops.DIBColor = IJL_RGBA_FPX then
      RGBA2BGRA2(jcprops.DIBBytes,iWidth,iHeight);
    ijlFree(@jcprops);
  except
    on E: Exception do
    begin
      result:=false;
      ijlFree(@jcprops);
    end;
  end;
end;

Procedure ClearCache;
var i:integer;
begin
 for i:=0 to CacheList.Count-1 do
   TBitmap32(CacheList.Objects[i]).Free;
 CacheList.Clear;
end;

function LoadTilefromCache(btm:Tobject;path:string;caching:boolean):boolean;
//var str:TMemoryStream;
   // fsize, hFile:integer;
//    jpg:TJPEGImage;
var btmcache:TObject;
    s,i:integer;
    p1,p2:Pointer;
begin
{  str:=TMemoryStream.Create;
  hFile := FileOpen(path,fmOpenRead);
  fSize := FileSeek(hFile,0,2);
  FileSeek(hFile,0,0);
  str.SetSize(fsize);
  FileRead(hFile,str.Memory^,fSize);
  if ExtractFileExt(path)='.jpg' then begin
                                       jpg:=TJPEGImage.Create;
                                       jpg.LoadFromStream(str); }
  result:=false;

  if GetFileSize(path)=0 then exit;
  try
   if (btm is TBitmap32) then
    begin
      if not(caching) then
       begin
        if ExtractFileExt(path)='.jpg'
         then begin
               if not(LoadJPG32(path,TBitmap32(btm))) then begin result:=false; exit; end
              end
         else TBitmap32(btm).LoadFromFile(path);
        result:=true;
        exit;
       end;
      i:=CacheList.IndexOf(AnsiUpperCase(path));
      if i<0 then
       begin
        if ExtractFileExt(path)='.jpg'
         then begin
                if not(LoadJPG32(path,TBitmap32(btm))) then begin result:=false; exit; end
              end
         else TBitmap32(btm).LoadFromFile(path);
        btmcache:=TBitmap32.Create;
        TBitmap32(btmcache).Assign(TBitmap32(btm));
        CacheList.AddObject(AnsiUpperCase(path), btmcache);
        if CacheList.Count > TilesOCache then
         begin
          CacheList.Objects[0].Free;
//          TBitmap32(CacheList.Objects[0]).Free;
          CacheList.Delete(0);
         end;
       end
      else
       begin
        TBitmap32(btm).Assign(TBitmap32(CacheList.Objects[i]));
        result:=true;
        exit;
       end;
     end
   else
   if (btm is TGraphic) then
    TGraphic(btm).LoadFromFile(path) else
   if (btm is TPicture) then
    TPicture(btm).LoadFromFile(path) else
   if (btm is TJPEGimage) then
    TJPEGimage(btm).LoadFromFile(path) else
   if (btm is TPNGObject) then
    TPNGObject(btm).LoadFromFile(path);
   result:=true;
  except
  end;
{                                       jpg.Free;
                                      end;
  str.Free;
  FileClose(hFile);      }
end;

end.
