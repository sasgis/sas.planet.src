unit Uimgfun;

interface

uses
  Windows,
  Classes,
  graphics,
  StrUtils,
  SysUtils,
  Math,
  Types,
  GR32,
  pngimage,
  UMapType;

const
  FILE_DOES_NOT_EXIST = DWORD(-1);

type
  TTileResamplingType = (
    trtBox = 0,
    trtLinear = 1,
    trtCosine = 2,
    trtSpline = 3,
    trtMitchell = 4,
    trtCubic = 5,
    trtHermite = 6,
    trtLanczos = 7,
    trtGaussian = 8,
    trtBlackman = 9,
    trtHannKernel = 10,
    trtHamming = 11,
    trtSinsh = 12
  );

  procedure SetDefoultMap;
  function PNGintoBitmap32(destBitmap: TBitmap32; PNGObject: TPNGObject): boolean;
  procedure CropPNGImage(var png:TPNGObject;dx,dy,cx,cy:integer);
  function CreateResampler(AResampling: TTileResamplingType): TCustomResampler;
  procedure Gamma(Bitmap: TBitmap32);
  function loadpre(var spr:TBitmap32;x,y:integer;Azoom:byte;Amap:TMapType):boolean;
  procedure Contrast(Bitmap: TBitmap32; Value: double);

implementation

uses
  GR32_Resamplers,
  GR32_Filters,
  u_GlobalState,
  unit1;

function CreateResampler(AResampling: TTileResamplingType): TCustomResampler;
begin
  if AResampling = trtLinear then begin
    Result := TLinearResampler.Create;
  end else begin
    Result:=TKernelResampler.Create;
    case AResampling of
      trtBox:
        TKernelResampler(Result).Kernel:=TBoxKernel.Create;
      trtCosine:
        TKernelResampler(Result).Kernel:=TCosineKernel.Create;
      trtSpline:
        TKernelResampler(Result).Kernel:=TSplineKernel.Create;
      trtMitchell:
        TKernelResampler(Result).Kernel:=TMitchellKernel.Create;
      trtCubic:
        TKernelResampler(Result).Kernel:=TCubicKernel.Create;
      trtHermite:
        TKernelResampler(Result).Kernel:=THermiteKernel.Create;
      trtLanczos:
        TKernelResampler(Result).Kernel:=TLanczosKernel.Create;
      trtGaussian:
        TKernelResampler(Result).Kernel:=TGaussianKernel.Create;
      trtBlackman:
        TKernelResampler(Result).Kernel:=TBlackmanKernel.Create;
      trtHannKernel:
        TKernelResampler(Result).Kernel:=THannKernel.Create;
      trtHamming:
        TKernelResampler(Result).Kernel:=THammingKernel.Create;
      trtSinsh:
        TKernelResampler(Result).Kernel:=TSinshKernel.Create;
    end;
  end;
end;

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
    RGBPtr:PColor32Array;
    TransparentColor: TColor32;
    AlphaPtr: pByteArray;
    png:TPNGObject;
    X, Y: Integer;
begin
 try
  result:=false;
  destBitmap.Clear;
  destBitmap.Width:=PNGObject.Width;
  destBitmap.Height:=PNGObject.Height;
  case PNGObject.TransparencyMode of
    ptmPartial:
     begin
      if (PNGObject.Header.ColorType in [COLOR_GRAYSCALEALPHA,COLOR_RGBALPHA]) then
       begin
        destBitmap.Draw(bounds(0,0,destBitmap.Width,destBitmap.Height),bounds(0,0,destBitmap.Width,destBitmap.Height),PNGObject.Canvas.Handle);// Assign(PNGObject);
        for Y:=0 to destBitmap.Height-1 do
         begin
          RGBPtr:=destBitmap.ScanLine[Y];
          AlphaPtr:=PNGObject.AlphaScanline[Y];
          for X:=0 to destBitmap.Width-1 do begin
           RGBPtr^[x]:=(RGBPtr^[x])or(AlphaPtr^[X] shl 24);
          end;
         end;
       end;
      if (PNGObject.Header.ColorType in [COLOR_PALETTE]) then
       begin
        for Y:=0 to destBitmap.Height-1 do
         begin
          RGBPtr:=destBitmap.ScanLine[Y];
          AlphaPtr:=PNGObject.Scanline[Y];
          for X:=0 to (destBitmap.Width-1) do begin
            RGBPtr^[x]:=TColor32(integer(TChunkPLTE(PNGObject.Chunks.ItemFromClass(TChunkPLTE)).Item[AlphaPtr^[X]])
             or TChunktRNS(PNGObject.Chunks.ItemFromClass(TChunktRNS)).PaletteValues[AlphaPtr^[X]] shl 24);
          end;
         end;
       end;
      end;
    ptmBit:
      if (PNGObject.Header.ColorType in [COLOR_PALETTE]) then
       begin
        for Y:=0 to destBitmap.Height-1 do
         begin
          RGBPtr:=destBitmap.ScanLine[Y];
          AlphaPtr:=PNGObject.Scanline[Y];
          for X:=0 to (destBitmap.Width-1) do begin
           if PNGObject.Pixels[X,Y]=PNGObject.TransparentColor
            then RGBPtr^[x]:=RGBPtr^[x] and $00000000
            else RGBPtr^[x]:=Color32(PNGObject.Pixels[X,Y]);
          end
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
 Sm_Map.DefoultMap:=TBitmap.Create;
 Sm_Map.DefoultMap.Assign(b);
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

procedure Contrast(Bitmap: TBitmap32; Value: double);
 function BLimit(B:Integer):Byte;
  begin
   if B<0 then Result:=0
          else if B>255 then Result:=255
                        else Result:=B;
  end;
var Dest: PColor32;
    x,y,mr,mg,mb,i:Integer;
    ContrastTable:array [0..255] of byte;
    vd: Double;
begin
  if Value=0 then Exit;
  Value:=Value/10;
  mR:=128;
  mG:=128;
  mB:=128;
  if Value>0 then vd:=1+(Value/10)
             else vd:=1-(Sqrt(-Value)/10);
  for i:=0 to 255 do begin
    ContrastTable[i]:=BLimit(mR+Trunc((i-mR)*vd));
  end;

  Dest:=@Bitmap.Bits[0];
  for y:=0 to Bitmap.Width*Bitmap.Height-1 do
   begin
      Dest^:=GR32.Color32(ContrastTable[RedComponent(dest^)],
                          ContrastTable[GreenComponent(dest^)],
                          ContrastTable[BlueComponent(dest^)],AlphaComponent(dest^));
      Inc(Dest);
   end;
end;

procedure InvertBitmap(Bitmap: TBitmap32);
begin
 if GState.InvertColor then InvertRGB(Bitmap,Bitmap);
end;

procedure Gamma(Bitmap: TBitmap32);
  function Power(Base, Exponent: Extended): Extended;
  begin
    Result := Exp(Exponent * Ln(Base));
  end;
var Dest: PColor32;
    X,Y: integer;
    GammaTable:array[0..255] of byte;
    L:Double;
begin
  Contrast(Bitmap, GState.ContrastN);
  InvertBitmap(Bitmap);
  if GState.GammaN<>50 then
   begin
    if GState.GammaN<50 then L:=1/((GState.GammaN*2)/100)
                 else L:=1/((GState.GammaN-40)/10);
    GammaTable[0]:=0;
    for X := 1 to 255 do GammaTable[X]:=round(255*Power(X/255,L));
    Dest:=@Bitmap.Bits[0];
    for Y := 0 to Bitmap.Height*Bitmap.Width-1 do
     begin
      Dest^:= GR32.Color32(GammaTable[RedComponent(dest^)],GammaTable[GreenComponent(dest^)],GammaTable[BlueComponent(dest^)],AlphaComponent(dest^));
      Inc(Dest);
     end;
   end;
end;

function loadpre(var spr:TBitmap32;x,y:integer;Azoom:byte;Amap:TMapType):boolean;
var i,c_x,c_y,dZ:integer;
    bmp:TBitmap32;
    VTileExists: Boolean;
begin
 result:=false;
 if not(GState.UsePrevZoom) then
  begin
   spr.Clear(SetAlpha(Color32(clSilver),0));
   exit;
  end;
  VTileExists := false;
 for i:=(Azoom-1) downto 1 do
  begin
   dZ:=(Azoom-i);
   if AMap.TileExists(x shr dZ,y shr dZ,i) then begin
    VTileExists := true;
    break;
   end;
  end;
 if not(VTileExists)or(dZ>8) then
  begin
   spr.Clear(SetAlpha(Color32(clSilver),0));
   exit;
  end;
 bmp:=TBitmap32.Create;
 if not(AMap.LoadTile(bmp,x shr dZ,y shr dZ, Azoom - dZ,true))then
  begin
   spr.Clear(SetAlpha(Color32(clSilver),0));
   bmp.Free;
   exit;
  end;
 bmp.Resampler := CreateResampler(GState.Resampling);
 c_x:=((x-(x mod 256))shr dZ)mod 256;
 c_y:=((y-(y mod 256))shr dZ)mod 256;
 try
  spr.Draw(bounds(-c_x shl dZ,-c_y shl dZ,256 shl dZ,256 shl dZ),bounds(0,0,256,256),bmp);
 except
 end;
 bmp.Free;
 result:=true;
end;

end.
