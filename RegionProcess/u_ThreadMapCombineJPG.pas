unit u_ThreadMapCombineJPG;

interface

uses
  Windows,
  Types,
  SysUtils,
  Classes,
  GR32,
  UMapType,
  UGeoFun,
  bmpUtil,
  t_GeoTypes,
  i_MarksSimple,
  i_IBitmapPostProcessingConfig,
  UResStrings,
  u_ThreadMapCombineBase,
  Imaging,
  ImagingTypes,
  ImagingJpeg;

type
  PArrayBGR = ^TArrayBGR;
  TArrayBGR = array [0..0] of TBGR;

  P256ArrayBGR = ^T256ArrayBGR;
  T256ArrayBGR = array[0..255] of PArrayBGR;

  TThreadMapCombineJPG = class(TThreadMapCombineBase)
  private
    FArray256BGR: P256ArrayBGR;
    sx, ex, sy, ey: integer;
    btmm: TCustomBitmap32;
    FQuality: Integer;

    procedure ReadLineBMP(Line: cardinal; LineRGB: PLineRGBb);
  protected
    procedure saveRECT; override;
  public
    constructor Create(
      AMapCalibrationList: IInterfaceList;
      AFileName: string;
      APolygon: TArrayOfDoublePoint;
      ASplitCount: TPoint;
      Azoom: byte;
      Atypemap: TMapType;
      AHtypemap: TMapType;
      AusedReColor: Boolean;
      ARecolorConfig: IBitmapPostProcessingConfigStatic;
      AMarksSubset: IMarksSubset;
      AQuality: Integer
    );
  end;

implementation

uses
  i_ICoordConverter,
  i_ILocalCoordConverter,
  u_GlobalState;

constructor TThreadMapCombineJPG.Create(
  AMapCalibrationList: IInterfaceList;
  AFileName: string;
  APolygon: TArrayOfDoublePoint;
  ASplitCount: TPoint;
  Azoom: byte;
  Atypemap, AHtypemap: TMapType;
  AusedReColor: Boolean;
  ARecolorConfig: IBitmapPostProcessingConfigStatic;
  AMarksSubset: IMarksSubset;
  AQuality: Integer
);
begin
  inherited Create(AMapCalibrationList, AFileName, APolygon, ASplitCount,
    Azoom, Atypemap, AHtypemap, AusedReColor, ARecolorConfig, AMarksSubset);
  FQuality := AQuality;
end;

procedure TThreadMapCombineJPG.ReadLineBMP(Line: cardinal;
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
    FTilesProcessed := line;
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

procedure TThreadMapCombineJPG.saveRECT;
var
  iWidth, iHeight: integer;
  k: integer;
  VImage: TImageData;
  VFormat: TJpegFileFormat;
  IArray: TDynImageDataArray;
begin
  sx := (FCurrentPieceRect.Left mod 256);
  sy := (FCurrentPieceRect.Top mod 256);
  ex := (FCurrentPieceRect.Right mod 256);
  ey := (FCurrentPieceRect.Bottom mod 256);

  iWidth := FMapPieceSize.X;
  iHeight := FMapPieceSize.y;
  InitImage(VImage);
  VFormat := TJpegFileFormat.Create();
  try
    getmem(FArray256BGR, 256 * sizeof(P256ArrayBGR));
    for k := 0 to 255 do begin
      getmem(FArray256BGR[k], (iWidth + 1) * 3);
    end;
    btmm := TCustomBitmap32.Create;
    btmm.Width := 256;
    btmm.Height := 256;

    VFormat.Quality := FQuality;
    NewImage(iWidth, iHeight, ifR8G8B8, VImage);

    if VImage.Bits <> nil then begin
      for k := 0 to iHeight - 1 do begin
        ReadLineBMP(k, Pointer(integer(VImage.Bits) + ((iWidth * 3) * k)));
        if IsCancel then begin
          break;
        end;
      end;
    end else begin
      ShowMessageSync(SAS_ERR_Memory + '.' + #13#10 + SAS_ERR_UseADifferentFormat);
      exit;
    end;
    SetLength(IArray, 1);
    IArray[0] := VImage;
    if not VFormat.SaveToFile(FCurrentFileName, IArray, True) then begin
      ShowMessageSync('Ошибка записи файла');
    end;
  Finally
    for k := 0 to 255 do begin
      freemem(FArray256BGR[k], (iWidth + 1) * 3);
    end;
    freemem(FArray256BGR, 256 * ((iWidth + 1) * 3));
    VFormat.Free;
    FreeImage(VImage);
    btmm.Free;
  end;
end;

end.
