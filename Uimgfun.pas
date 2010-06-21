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
  function CreateResampler(AResampling: TTileResamplingType): TCustomResampler;
  procedure Gamma(Bitmap: TBitmap32);

implementation

uses
  GR32_Resamplers,
  GR32_Filters,
  u_GlobalState;

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

procedure PngWithPaletteToBitmap32(destBitmap: TBitmap32; PNGObject: TPNGObject; DataDepth: Byte);
var
  ChunkPLTE:TChunkPLTE;
  ChunktRNS:TChunktRNS;
  X, Y: Integer;
  RGBPtr:PColor32Array;
  AlphaPtr: pByteArray;
  VPalette: array of TColor32;
  i: Integer;
  VByteData: Byte;
begin
  ChunkPLTE:=TChunkPLTE(PNGObject.Chunks.ItemFromClass(TChunkPLTE));
  ChunktRNS:=TChunktRNS(PNGObject.Chunks.ItemFromClass(TChunktRNS));
  SetLength(VPalette, ChunkPLTE.Count);
  for i := 0 to ChunkPLTE.Count - 1 do begin
    with ChunkPLTE.Item[i] do begin
      VPalette[i] := color32(PNGObject.GammaTable[rgbRed], PNGObject.GammaTable[rgbGreen],
                            PNGObject.GammaTable[rgbBlue], ChunktRNS.PaletteValues[i]);
    end;
  end;
  if DataDepth >= 8 then begin
    for Y:=0 to destBitmap.Height-1 do begin
      RGBPtr:=destBitmap.ScanLine[Y];
      AlphaPtr:=PNGObject.Scanline[Y];
      for X:=0 to (destBitmap.Width-1) do begin
        RGBPtr^[x] := VPalette[AlphaPtr^[X]];
      end;
    end;
  end else begin
    for Y:=0 to destBitmap.Height-1 do begin
      RGBPtr:=destBitmap.ScanLine[Y];
      AlphaPtr:=PNGObject.Scanline[Y];
      for X:=0 to (destBitmap.Width-1) do begin
        VByteData := AlphaPtr^[X div (8 div DataDepth)];
        VByteData := (VByteData shr ((8 - DataDepth) -(X mod (8 div DataDepth)) * DataDepth));
        VByteData:= VByteData and ($FF shr (8 - DataDepth));
        RGBPtr^[x] := VPalette[VByteData];
      end;
    end;
  end;
  VPalette := nil;
end;

procedure PngRgbAlfaToBitmap32(destBitmap: TBitmap32; PNGObject: TPNGObject);
var
  X, Y: Integer;
  RGBPtr:PColor32Array;
  AlphaPtr: pByteArray;
  VRGB: TRGBTriple;
  SourcePtr: pRGBLine;
begin
  for Y:=0 to destBitmap.Height-1 do begin
    RGBPtr:=destBitmap.ScanLine[Y];
    AlphaPtr:=PNGObject.AlphaScanline[Y];
    SourcePtr := PNGObject.Scanline[Y];
    for X:=0 to destBitmap.Width-1 do begin
      VRGB := SourcePtr^[X];
      RGBPtr^[x] := Color32(VRGB.rgbtRed, VRGB.rgbtGreen, VRGB.rgbtBlue, AlphaPtr^[X]);
    end;
  end;
end;

procedure PngGrayScaleAlfaToBitmap32(destBitmap: TBitmap32; PNGObject: TPNGObject);
var
  X, Y: Integer;
  RGBPtr:PColor32Array;
  AlphaPtr: pByteArray;
begin
  destBitmap.Draw(bounds(0,0,destBitmap.Width,destBitmap.Height),bounds(0,0,destBitmap.Width,destBitmap.Height),PNGObject.Canvas.Handle);
  for Y:=0 to destBitmap.Height-1 do begin
    RGBPtr:=destBitmap.ScanLine[Y];
    AlphaPtr:=PNGObject.AlphaScanline[Y];
    for X:=0 to destBitmap.Width-1 do begin
      RGBPtr^[x]:=(RGBPtr^[x])or(AlphaPtr^[X] shl 24);
    end;
  end;
end;
procedure PngRGBToBitmap32(destBitmap: TBitmap32; PNGObject: TPNGObject);
var
  X, Y: Integer;
  DestPtr:PColor32Array;
  VRGB: TRGBTriple;
  SourcePtr: pRGBLine;
  trColor:TColor32;
begin
  TrColor:=Color32(PNGObject.TransparentColor);
  for Y:=0 to (destBitmap.Height-1) do begin
    DestPtr := destBitmap.ScanLine[Y];
    SourcePtr:=PNGObject.Scanline[Y];
    for X:=0 to (destBitmap.Width-1) do begin
      VRGB := SourcePtr^[X];
      DestPtr^[x] := Color32(VRGB.rgbtRed, VRGB.rgbtGreen, VRGB.rgbtBlue);
      if DestPtr^[x] = trColor then begin
        DestPtr^[x] := 0;
      end;
    end;
  end;
end;

function PNGintoBitmap32(destBitmap: TBitmap32; PNGObject: TPNGObject): boolean;
var
    AlphaPtr: pByteArray;
    X, Y: Integer;
    trColor:TColor32;
    PixelPtr: PColor32;
    ChunkPLTE:TChunkPLTE;
    DataDepth: Byte;
    ColorType: Byte;
begin
 try
  destBitmap.Clear;
  destBitmap.SetSize(PNGObject.Width,PNGObject.Height);
  DataDepth:=PNGObject.Header.BitDepth;
  case PNGObject.TransparencyMode of
    ptmPartial:
     begin
      case PNGObject.Header.ColorType of
        COLOR_GRAYSCALEALPHA: begin
          PngGrayScaleAlfaToBitmap32(destBitmap, PNGObject);
        end;
        COLOR_RGBALPHA: begin
          PngRgbAlfaToBitmap32(destBitmap, PNGObject);
        end;
        COLOR_PALETTE: begin
          PngWithPaletteToBitmap32(destBitmap, PNGObject, DataDepth);
        end;
      end;
     end;
    ptmBit:
      begin
        case PNGObject.Header.ColorType of
          COLOR_GRAYSCALE: begin
            ChunkPLTE:=TChunkPLTE(PNGObject.Chunks.ItemFromClass(TChunkPLTE));
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
          COLOR_RGB: begin
            PngRGBToBitmap32(destBitmap, PNGObject);
          end;
          COLOR_PALETTE: begin
            PngWithPaletteToBitmap32(destBitmap, PNGObject, DataDepth);
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
