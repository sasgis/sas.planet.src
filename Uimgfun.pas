unit Uimgfun;
                               
interface
uses Classes,pngimage,StrUtils,SysUtils,Windows,Types,UMaptype,RxGIF,Math,GR32,jpeg,graphics;

const
  FILE_DOES_NOT_EXIST = DWORD(-1);

var
  defoultMap:TBitmap;// TPNGObject;
  function SaveTileInCache(btm:TObject;path:string):boolean;
  function TileExists(path:string):boolean;
  function LoadTilefromCache(btm:Tobject;path:string):boolean;
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
  destBitmap.Assign(PNGObject);
  destBitmap.ResetAlpha;
  case PNGObject.TransparencyMode of
    ptmPartial:
      if (PNGObject.Header.ColorType in [COLOR_GRAYSCALEALPHA,COLOR_RGBALPHA]) then
       begin
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
    ptmBit:
      begin
       TransparentColor:=Color32(PNGObject.TransparentColor);
       PixelPtr:=PColor32(@destBitmap.Bits[0]);
       for X:=0 to (destBitmap.Height-1)*(destBitmap.Width-1) do
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
 b.LoadFromResourceID(HInstance, 102);
 DefoultMap:=TBitmap.Create;
 DefoultMap.Assign(b);
 b.LoadFromResourceID(HInstance, 150);
 Sm_Map.PlusButton:=TBitmap32.Create;
 PNGintoBitmap32(Sm_Map.PlusButton,b);
 Sm_Map.PlusButton.DrawMode:=dmTransparent;
 b.LoadFromResourceID(HInstance, 151);
 Sm_Map.MinusButton:=TBitmap32.Create;
 Sm_Map.MinusButton.DrawMode:=dmTransparent;
 PNGintoBitmap32(Sm_Map.MinusButton,b);
 b.LoadFromResourceID(HInstance, 152);
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
   if UpperCase(ExtractFileExt(path))='BMP' then btm_ex.SaveToFile(path);
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

function LoadTilefromCache(btm:Tobject;path:string):boolean;
//var str:TMemoryStream;
   // fsize, hFile:integer;
//    jpg:TJPEGImage;
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
                                       if (btm is TBitmap32) then TBitmap32(btm).LoadFromFile(path){Assign(jpg)} else
                                       if (btm is TGraphic) then TGraphic(btm).LoadFromFile(path) else
                                       if (btm is TPicture) then TPicture(btm).LoadFromFile(path) else
                                       if (btm is TJPEGimage) then TJPEGimage(btm).LoadFromFile(path) else
                                       if (btm is TPNGObject) then TPNGObject(btm).LoadFromFile(path);
 if sat_map_both.DelAfterShow then delFile(path);
{                                       jpg.Free;
                                      end;

  str.Free;
  FileClose(hFile);      }
end;

end.
