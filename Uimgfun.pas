unit Uimgfun;

interface

uses
  Windows,
  Classes,
  graphics,
  SysUtils,
  Types,
  GR32,
  pngimage;

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

  function PNGintoBitmap32(destBitmap: TBitmap32; PNGObject: TPNGObject): boolean;
  procedure CropPNGImage(var png:TPNGObject;dx,dy,cx,cy:integer);
  function CreateResampler(AResampling: TTileResamplingType): TCustomResampler;
  procedure Gamma(Bitmap: TBitmap32);
  procedure Contrast(Bitmap: TBitmap32; Value: double);
  function str2r(inp:string):Extended;

implementation

uses
  GR32_Resamplers,
  GR32_Filters,
  u_GlobalState;

function str2r(inp:string):Extended;
var p:integer;
begin
 p:=System.pos(DecimalSeparator,inp);
 if p=0 then begin
              if DecimalSeparator='.' then p:=System.pos(',',inp)
                                      else p:=System.pos('.',inp);
              inp[p]:=DecimalSeparator;
             end;
 result:=strtofloat(inp);
end;

function CreateResampler(AResampling: TTileResamplingType): TCustomResampler;
begin
  if AResampling = trtLinear then begin
    Result := TLinearResampler.Create;
  end else begin
    Result:=TKernelResampler.Create;
    case AResampling of
      trtBox:
        result:=nil;
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


function GetByteArrayPixel(const png: TPngObject; const X: Integer; scLine:pByteArray;ChunkPLTE:TChunkPLTE;DataDepth,ColorType:Byte): TColor32;
var ByteData: Byte;
begin
    if DataDepth>7 then begin
      ByteData := scLine^[X];
    end else begin
      ByteData := scLine^[X div (8 div DataDepth)];
      ByteData := (ByteData shr ((8 - DataDepth) -(X mod (8 div DataDepth)) * DataDepth));
      ByteData:= ByteData and ($FF shr (8 - DataDepth));
    end;
    case ColorType of
      COLOR_PALETTE:
        with ChunkPLTE.Item[ByteData] do
           result:=color32(png.GammaTable[rgbRed], png.GammaTable[rgbGreen], png.GammaTable[rgbBlue], $FF);
      COLOR_GRAYSCALE:
      begin
        if DataDepth = 1
        then ByteData := png.GammaTable[Byte(ByteData * 255)]
        else ByteData := png.GammaTable[Byte(ByteData * ((1 shl DataDepth) + 1))];
        Result :=color32(ByteData, ByteData, ByteData, $FF);;
      end;
      else Result := 0;
    end;
end;

function PNGintoBitmap32(destBitmap: TBitmap32; PNGObject: TPNGObject): boolean;
var
    RGBPtr:PColor32Array;
    AlphaPtr: pByteArray;
    X, Y: Integer;
    trColor:TColor32;
    PixelPtr: PColor32;
    ChunkPLTE:TChunkPLTE;
    ChunktRNS:TChunktRNS;
    DataDepth: Byte;
    ColorType: Byte;
begin
 try
  destBitmap.Clear;
  destBitmap.SetSize(PNGObject.Width,PNGObject.Height);
  case PNGObject.TransparencyMode of
    ptmPartial:
     begin
      if (PNGObject.Header.ColorType in [COLOR_GRAYSCALEALPHA,COLOR_RGBALPHA]) then
       begin
        destBitmap.Draw(bounds(0,0,destBitmap.Width,destBitmap.Height),bounds(0,0,destBitmap.Width,destBitmap.Height),PNGObject.Canvas.Handle);
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
        ChunkPLTE:=TChunkPLTE(PNGObject.Chunks.ItemFromClass(TChunkPLTE));
        ChunktRNS:=TChunktRNS(PNGObject.Chunks.ItemFromClass(TChunktRNS));
        for Y:=0 to destBitmap.Height-1 do
         begin
          RGBPtr:=destBitmap.ScanLine[Y];
          AlphaPtr:=PNGObject.Scanline[Y];
          for X:=0 to (destBitmap.Width-1) do begin
            with ChunkPLTE.Item[AlphaPtr^[X]] do
             RGBPtr^[x]:=color32(PNGObject.GammaTable[rgbRed], PNGObject.GammaTable[rgbGreen],
                                  PNGObject.GammaTable[rgbBlue], ChunktRNS.PaletteValues[AlphaPtr^[X]]);
          end;
         end;
       end;
      end;
    ptmBit:
      begin
       ChunkPLTE:=TChunkPLTE(PNGObject.Chunks.ItemFromClass(TChunkPLTE));
       DataDepth:=PNGObject.Header.BitDepth;
       ColorType:=PNGObject.Header.ColorType;
       TrColor:=Color32(PNGObject.TransparentColor);
       PixelPtr:=PColor32(@destBitmap.Bits[0]);
       for Y:=0 to (destBitmap.Height-1) do begin
         AlphaPtr:=PNGObject.Scanline[Y];
         for X:=0 to (destBitmap.Width-1) do begin
           PixelPtr^:=GetByteArrayPixel(PNGObject,X,AlphaPtr,ChunkPLTE,DataDepth,ColorType);
           if PixelPtr^=TrColor then begin
             PixelPtr^:=PixelPtr^ and $00000000;
           end;
           Inc(PixelPtr);
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
  result:=false;
 end;
end;

procedure Contrast(Bitmap: TBitmap32; Value: double);
 function BLimit(B:Integer):Byte;
  begin
   if B<0 then Result:=0
          else if B>255 then Result:=255
                        else Result:=B;
  end;
var Dest: PColor32;
    y,mr,i:Integer;
    ContrastTable:array [0..255] of byte;
    vd: Double;
begin
  if Value=0 then Exit;
  Value:=Value/10;
  mR:=128;
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

end.
