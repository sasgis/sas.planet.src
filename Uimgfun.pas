unit Uimgfun;

interface
uses IJL,Classes,pngimage,StrUtils,SysUtils,Windows,Types,UMaptype,RxGIF,Math,GR32,jpeg,graphics;

const
  FILE_DOES_NOT_EXIST = DWORD(-1);

var
  defoultMap:TBitmap;
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
var
    PixelPtr: PColor32;
    AlphaPtr: PByte;
    X, Y: Integer;
begin
 try
  result:=false;
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
