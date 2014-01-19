unit u_MarkerDrawableCenterScale;

interface

uses
  i_BitmapMarker,
  i_Bitmap32StaticFactory,
  u_MarkerDrawableByBitmapMarker;

type
  TMarkerDrawableCenterScale = class(TMarkerDrawableByBitmapMarker)
  private
    function CreateBitmapMarker(
      const ABitmapFactory: IBitmap32StaticFactory
    ): IBitmapMarker;
  public
    constructor Create(const ABitmapFactory: IBitmap32StaticFactory);
  end;

implementation

uses
  Types,
  SysUtils,
  GR32,
  i_Bitmap32Static,
  u_BitmapMarker,
  u_GeoFunc;

{ TMarkerDrawableCenterScale }

constructor TMarkerDrawableCenterScale.Create(
  const ABitmapFactory: IBitmap32StaticFactory
);
begin
  inherited Create(CreateBitmapMarker(ABitmapFactory));
end;

function TMarkerDrawableCenterScale.CreateBitmapMarker(
  const ABitmapFactory: IBitmap32StaticFactory
): IBitmapMarker;
var
  VBitmap: TBitmap32;
  VHalfSize: TPoint;
  i: integer;
  r: Double;
  xy, xy1: TPoint;
  VSize: TPoint;
  VTextWdth: integer;
  VRadius: Integer;
  VFontSize: Integer;
  VDigitsOffset: Integer;
  VBitmapStatic: IBitmap32Static;
begin
  Result := nil;
  VBitmap := TBitmap32.Create;
  try
    VRadius := 115;
    VDigitsOffset := 20;
    VFontSize := 12;
    VBitmap.Font.Size := VFontSize;
    VTextWdth := VBitmap.TextWidth('270°');
    VSize := Types.Point((VRadius * 2) + (VDigitsOffset * 2) + (VTextWdth * 2), (VRadius * 2) + (VDigitsOffset * 2) + (VTextWdth * 2));
    VHalfSize := Types.Point(VSize.X div 2, VSize.Y div 2);
    VBitmap.SetSize(VSize.X, VSize.Y);
    VBitmap.Clear(0);
    VBitmap.Font.Size := VFontSize - 3;

    i := 0;
    While i < 360 do begin
      VBitmap.Font.Size := VFontSize - 3;
      if (i mod 90) = 0 then begin
        r := 0;
        VBitmap.Font.Size := VFontSize;
      end else if (i mod 45) = 0 then begin
        r := VRadius - 40;
        VBitmap.Font.Size := VFontSize - 1;
      end else begin
        r := VRadius - 10;
      end;
      xy.x := round(VHalfSize.X + VRadius * cos(i * (Pi / 180)));
      xy.y := round(VHalfSize.Y + VRadius * sin(i * (Pi / 180)));
      xy1.x := round(VHalfSize.X + r * cos(i * (Pi / 180)));
      xy1.y := round(VHalfSize.Y + r * sin(i * (Pi / 180)));
      VBitmap.LineFS(xy.x, xy.y, xy1.x, xy1.y, SetAlpha(clRed32, 180));
      if (i mod 15) = 0 then begin
        xy1.x := round(VHalfSize.X + (VRadius + VDigitsOffset) * cos(i * (Pi / 180))) - VBitmap.TextWidth(inttostr((i + 90) mod 360) + '°') div 2;
        xy1.y := round(VHalfSize.X + (VRadius + VDigitsOffset) * sin(i * (Pi / 180))) - 2 - VBitmap.Font.size div 2;
        VBitmap.RenderText(xy1.x + 1, xy1.y + 1, inttostr((i + 90) mod 360) + '°', 3, SetAlpha(clWhite32, 150));
        VBitmap.RenderText(xy1.x, xy1.y, inttostr((i + 90) mod 360) + '°', 3, SetAlpha(clBlue32, 210));
      end;
      inc(i, 5);
    end;
    VBitmapStatic := ABitmapFactory.Build(VSize, VBitmap.Bits);
    Result := TBitmapMarker.Create(VBitmapStatic, DoublePoint(VHalfSize));
  finally
    VBitmap.Free;
  end;
end;

end.
