unit u_ThreadMapCombineECW;

interface

uses
  Types,
  SysUtils,
  Classes,
  GR32,
  i_GlobalViewMainConfig,
  i_BitmapLayerProvider,
  i_VectorItemLonLat,
  i_VectorItemProjected,
  i_LocalCoordConverterFactorySimpe,
  u_ECWWrite,
  u_MapType,
  u_GeoFun,
  t_GeoTypes,
  i_BitmapPostProcessingConfig,
  u_ResStrings,
  u_ThreadMapCombineBase;

type
  PRow = ^TRow;
  TRow = array[0..0] of byte;

  P256rgb = ^T256rgb;
  T256rgb = array[0..255] of PRow;

  TThreadMapCombineECW = class(TThreadMapCombineBase)
  private
    Rarr: P256rgb;
    Garr: P256rgb;
    Barr: P256rgb;
    FQuality: Integer;

    function ReadLine(ALine: cardinal; var LineR, LineG, LineB: PLineRGB): Boolean; reintroduce;
  protected
    procedure SaveRect; override;
  public
    constructor Create(
      AViewConfig: IGlobalViewMainConfig;
      AMarksImageProvider: IBitmapLayerProvider;
      ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      AMapCalibrationList: IInterfaceList;
      AFileName: string;
      APolygon: ILonLatPolygon;
      AProjectedPolygon: IProjectedPolygon;
      ASplitCount: TPoint;
      Atypemap: TMapType;
      AHtypemap: TMapType;
      AusedReColor: Boolean;
      ARecolorConfig: IBitmapPostProcessingConfigStatic;
      AQuality: Integer
    );
  end;

implementation

uses
  LibECW,
  i_CoordConverter,
  i_LocalCoordConverter;

constructor TThreadMapCombineECW.Create(
  AViewConfig: IGlobalViewMainConfig;
  AMarksImageProvider: IBitmapLayerProvider;
  ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  AMapCalibrationList: IInterfaceList;
  AFileName: string;
  APolygon: ILonLatPolygon;
  AProjectedPolygon: IProjectedPolygon;
  ASplitCount: TPoint;
  Atypemap, AHtypemap: TMapType;
  AusedReColor: Boolean;
  ARecolorConfig: IBitmapPostProcessingConfigStatic;
  AQuality: Integer
);
begin
  inherited Create(
    AViewConfig,
    AMarksImageProvider,
    ALocalConverterFactory,
    AMapCalibrationList,
    AFileName,
    APolygon,
    AProjectedPolygon,
    ASplitCount,
    Atypemap,
    AHtypemap,
    AusedReColor,
    ARecolorConfig
  );
  FQuality := AQuality;
end;

function TThreadMapCombineECW.ReadLine(ALine: cardinal; var LineR, LineG,
  LineB: PLineRGB): boolean;
var
  i, j, rarri, lrarri, p_x, p_y, Asx, Asy, Aex, Aey, starttile: integer;
  line: Integer;
  p: PColor32array;
  VConverter: ILocalCoordConverter;
begin
  Result := True;
  line := ALine;
  if line < (256 - sy) then begin
    starttile := sy + line;
  end else begin
    starttile := (line - (256 - sy)) mod 256;
  end;
  if (starttile = 0) or (line = 0) then begin
    FTilesProcessed := Line;
    ProgressFormUpdateOnProgress;
    p_y := (CurrentPieceRect.Top + line) - ((CurrentPieceRect.Top + line) mod 256);
    p_x := CurrentPieceRect.Left - (CurrentPieceRect.Left mod 256);
    lrarri := 0;
    rarri := 0;
    if line > (255 - sy) then begin
      Asy := 0;
    end else begin
      Asy := sy;
    end;
    if (p_y div 256) = (CurrentPieceRect.Bottom div 256) then begin
      Aey := ey;
    end else begin
      Aey := 255;
    end;
    Asx := sx;
    Aex := 255;
    while p_x <= CurrentPieceRect.Right do begin
      // запомнием координаты обрабатываемого тайла для случая если произойдет ошибка
      FLastTile := Point(p_x shr 8, p_y shr 8);
      if not (RgnAndRgn(@FPoly[0], Length(FPoly), p_x + 128, p_y + 128, false)) then begin
        btmm.Clear(BackGroundColor);
      end else begin
        FLastTile := Point(p_x shr 8, p_y shr 8);
        VConverter := CreateConverterForTileImage(FLastTile);
        PrepareTileBitmap(btmm, VConverter);
      end;
      if (p_x + 256) > CurrentPieceRect.Right then begin
        Aex := ex;
      end;
      for j := Asy to Aey do begin
        p := btmm.ScanLine[j];
        rarri := lrarri;
        for i := Asx to Aex do begin
          Rarr^[j]^[rarri] := (cardinal(p^[i]) shr 16);
          Garr^[j]^[rarri] := (cardinal(p^[i]) shr 8);
          Barr^[j]^[rarri] := (cardinal(p^[i]));
          inc(rarri);
        end;
      end;
      lrarri := rarri;
      Asx := 0;
      inc(p_x, 256);
    end;
  end;
  for i := 0 to (CurrentPieceRect.Right - CurrentPieceRect.Left) - 1 do begin
    LineR^[i] := Rarr^[starttile]^[i];
    LineG^[i] := Garr^[starttile]^[i];
    LineB^[i] := Barr^[starttile]^[i];
  end;
end;

procedure TThreadMapCombineECW.SaveRect;
var
  k: integer;
  Datum, Proj: string;
  Units: TCellSizeUnits;
  CellIncrementX, CellIncrementY, OriginX, OriginY: Double;
  errecw: integer;
  VECWWriter: TECWWrite;
begin
  sx := (CurrentPieceRect.Left mod 256);
  sy := (CurrentPieceRect.Top mod 256);
  ex := (CurrentPieceRect.Right mod 256);
  ey := (CurrentPieceRect.Bottom mod 256);
  VECWWriter := TECWWrite.Create;
  try
    btmm := TCustomBitmap32.Create;
    try
      btmm.Width := 256;
      btmm.Height := 256;
      getmem(Rarr, 256 * sizeof(PRow));
      for k := 0 to 255 do begin
        getmem(Rarr[k], (MapPieceSize.X + 1) * sizeof(byte));
      end;
      getmem(Garr, 256 * sizeof(PRow));
      for k := 0 to 255 do begin
        getmem(Garr[k], (MapPieceSize.X + 1) * sizeof(byte));
      end;
      getmem(Barr, 256 * sizeof(PRow));
      for k := 0 to 255 do begin
        getmem(Barr[k], (MapPieceSize.X + 1) * sizeof(byte));
      end;
      try
        Datum := 'EPSG:' + IntToStr(MainGeoConverter.Datum.EPSG);
        Proj := 'EPSG:' + IntToStr(MainGeoConverter.GetProjectionEPSG);
        Units := MainGeoConverter.GetCellSizeUnits;
        CalculateWFileParams(
          MainGeoConverter.PixelPos2LonLat(CurrentPieceRect.TopLeft, Zoom),
          MainGeoConverter.PixelPos2LonLat(CurrentPieceRect.BottomRight, Zoom),
          MapPieceSize.X, MapPieceSize.Y, MainGeoConverter,
          CellIncrementX, CellIncrementY, OriginX, OriginY
          );
        errecw :=
          VECWWriter.Encode(
            OperationID,
            CancelNotifier,
            CurrentFileName,
            MapPieceSize.X,
            MapPieceSize.Y,
            101 - FQuality,
            COMPRESS_HINT_BEST,
            ReadLine,
            Datum,
            Proj,
            Units,
            CellIncrementX,
            CellIncrementY,
            OriginX,
            OriginY
          );
        if (errecw > 0) and (errecw <> 52) then begin
          raise Exception.Create(SAS_ERR_Save + ' ' + SAS_ERR_Code + inttostr(errecw));
        end;
      finally
        for k := 0 to 255 do begin
          freemem(Rarr[k]);
        end;
        FreeMem(Rarr);
        for k := 0 to 255 do begin
          freemem(Garr[k]);
        end;
        FreeMem(Garr);
        for k := 0 to 255 do begin
          freemem(Barr[k]);
        end;
        FreeMem(Barr);
      end;
    finally
      btmm.Free;
    end;
  finally
    FreeAndNil(VECWWriter);
  end;
end;

end.
