unit Uimgfun;

interface

uses
  Windows,
  Classes,
  graphics,
  SysUtils,
  Types,
  GR32;

  procedure Gamma(Bitmap: TCustomBitmap32; ContrastN: Integer; GammaN: Integer; InvertColor: Boolean);

implementation

uses
  GR32_Filters;

procedure Contrast(Bitmap: TCustomBitmap32; Value: double);
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

procedure Gamma(Bitmap: TCustomBitmap32; ContrastN: Integer; GammaN: Integer; InvertColor: Boolean);
  function Power(Base, Exponent: Extended): Extended;
  begin
    Result := Exp(Exponent * Ln(Base));
  end;
var Dest: PColor32;
    X,Y: integer;
    GammaTable:array[0..255] of byte;
    L:Double;
begin
  Contrast(Bitmap, ContrastN);
  if InvertColor then InvertRGB(Bitmap,Bitmap);;
  if GammaN<>50 then
   begin
    if GammaN<50 then L:=1/((GammaN*2)/100)
                 else L:=1/((GammaN-40)/10);
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
