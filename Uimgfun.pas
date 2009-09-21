unit Uimgfun;

interface
uses IJL,Classes,pngimage,StrUtils,SysUtils,Windows,Types,UMaptype,RxGIF,Math,GR32,jpeg,graphics;

const
  FILE_DOES_NOT_EXIST = DWORD(-1);

var
  defoultMap:TBitmap;// TPNGObject;
  function SaveTileInCache(btm:TObject;path:string):boolean;
  function DelFile(path:string):boolean;
  function Copy_File(pathfrom,pathto:string;zamena:boolean):boolean;
  procedure SetDefoultMap;
  function PNGintoBitmap32(destBitmap: TBitmap32; PNGObject: TPNGObject): boolean;
  procedure CropPNGImage(var png:TPNGObject;dx,dy,cx,cy:integer);
  function InStr(I: Integer): string;

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
        TransparentColor:=Color32(PNGObject.TransparentColor);
        if PNGObject.Chunks.Item[3].Index=0 then destBitmap.Height:=destBitmap.Height;
        PixelPtr:=PColor32(@destBitmap.Bits[0]);
        for Y:=0 to destBitmap.Height-1 do
        begin
        AlphaPtr:=PByte(PNGObject.Scanline[Y]);
        for X:=0 to (destBitmap.Width-1) do
         begin
          if PNGObject.Pixels[X,Y]=0 then PixelPtr^:=PixelPtr^ and $00000000
                         else PixelPtr^:=Color32(PNGObject.Pixels[X,Y]);
          Inc(PixelPtr);
          //Inc(AlphaPtr);
         end;
        end;
      end;
     ptmNone:
       begin
        destBitmap.Assign(PNGObject);
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

function DelFile(path:string):boolean;
begin
 result:=DeleteFile(PChar(path));
end;

function Copy_File(pathfrom,pathto:string;zamena:boolean):boolean;
begin
 CopyFile(Pchar(pathfrom),Pchar(pathto),zamena);
end;
end.
