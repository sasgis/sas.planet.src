unit u_ThreadMapCombineBMP;

interface

uses
  Windows,
  Types,
  SysUtils,
  Classes,
  GR32,
  UGeoFun,
  bmpUtil,
  u_ThreadMapCombineBase;

type
  PArrayBGR = ^TArrayBGR;
  TArrayBGR = array [0..0] of TBGR;

  P256ArrayBGR = ^T256ArrayBGR;
  T256ArrayBGR = array[0..255] of PArrayBGR;

  TThreadMapCombineBMP = class(TThreadMapCombineBase)
  private
    FArray256BGR: P256ArrayBGR;
    sx, ex, sy, ey: integer;
    btmm: TCustomBitmap32;

    procedure ReadLineBMP(Line: cardinal; LineRGB: PLineRGBb);
  protected
    procedure saveRECT; override;
  public
  end;

implementation

uses
  i_LocalCoordConverter;

procedure TThreadMapCombineBMP.ReadLineBMP(Line: cardinal;
  LineRGB: PLineRGBb);
var
  i, j, rarri, lrarri, p_x, p_y, Asx, Asy, Aex, Aey, starttile: integer;
  p: PColor32array;
  VConverter: ILocalCoordConverter;
begin
  if line < (256 - sy) then begin
    starttile := sy + line;
  end else begin
    starttile := (line - (256 - sy)) mod 256;
  end;
  if (starttile = 0) or (line = 0) then begin
    FTilesProcessed := Line;
    ProgressFormUpdateOnProgress;
    p_y := (FCurrentPieceRect.Top + line) - ((FCurrentPieceRect.Top + line) mod 256);
    p_x := FCurrentPieceRect.Left - (FCurrentPieceRect.Left mod 256);
    lrarri := 0;
    if line > (255 - sy) then begin
      Asy := 0;
    end else begin
      Asy := sy;
    end;
    if (p_y div 256) = (FCurrentPieceRect.Bottom div 256) then begin
      Aey := ey;
    end else begin
      Aey := 255;
    end;
    Asx := sx;
    Aex := 255;
    while p_x <= FCurrentPieceRect.Right do begin
      if not (RgnAndRgn(FPoly, p_x + 128, p_y + 128, false)) then begin
        btmm.Clear(FBackGroundColor);
      end else begin
        FLastTile := Point(p_x shr 8, p_y shr 8);
        VConverter := CreateConverterForTileImage(FLastTile);
        PrepareTileBitmap(btmm, VConverter);
      end;
      if (p_x + 256) > FCurrentPieceRect.Right then begin
        Aex := ex;
      end;
      for j := Asy to Aey do begin
        p := btmm.ScanLine[j];
        rarri := lrarri;
        for i := Asx to Aex do begin
          CopyMemory(@FArray256BGR[j]^[rarri], Pointer(integer(p) + (i * 4)), 3);
          inc(rarri);
        end;
      end;
      lrarri := rarri;
      Asx := 0;
      inc(p_x, 256);
    end;
  end;
  CopyMemory(LineRGB, FArray256BGR^[starttile], (FCurrentPieceRect.Right - FCurrentPieceRect.Left) * 3);
end;

procedure TThreadMapCombineBMP.saveRECT;
var
  k: integer;
begin
  sx := (FCurrentPieceRect.Left mod 256);
  sy := (FCurrentPieceRect.Top mod 256);
  ex := (FCurrentPieceRect.Right mod 256);
  ey := (FCurrentPieceRect.Bottom mod 256);
  try
    btmm := TCustomBitmap32.Create;
    btmm.Width := 256;
    btmm.Height := 256;
    getmem(FArray256BGR, 256 * sizeof(P256ArrayBGR));
    for k := 0 to 255 do begin
      getmem(FArray256BGR[k], (FMapPieceSize.X + 1) * 3);
    end;
    SaveBMP(FMapPieceSize.X, FMapPieceSize.Y, FCurrentFileName, ReadLineBMP, IsCancel);
  finally
    {$IFDEF VER80}
    for k := 0 to 255 do begin
      freemem(FArray256BGR[k], (FMapPieceSize.X + 1) * 3);
    end;
    freemem(FArray256BGR, 256 * ((FMapPieceSize.X + 1) * 3));
    {$ELSE}
    for k := 0 to 255 do begin
      freemem(FArray256BGR[k]);
    end;
    FreeMem(FArray256BGR);
    {$ENDIF}
    btmm.Free;
  end;
end;

end.
