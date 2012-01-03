unit u_ThreadMapCombineBase;

interface

uses
  Windows,
  Classes,
  Types,
  GR32,
  t_GeoTypes,
  i_GlobalViewMainConfig,
  i_BitmapLayerProvider,
  i_BitmapPostProcessingConfig,
  i_VectorItemLonLat,
  i_VectorItemProjected,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  u_MapType,
  u_ThreadRegionProcessAbstract;

type
  TBGR = packed record
    B: Byte;
    G: Byte;
    R: Byte;
  end;

  TABGR = packed record
    A: Byte;
    B: Byte;
    G: Byte;
    R: Byte;
  end;

  PArrayBGR = ^TArrayBGR;
  TArrayBGR = array [0..0] of TBGR;

  PArrayABGR = ^TArrayABGR;
  TArrayABGR = array [0..0] of TABGR;

  P256ArrayBGR = ^T256ArrayBGR;
  T256ArrayBGR = array[0..255] of PArrayBGR;

  P256ArrayABGR = ^T256ArrayABGR;
  T256ArrayABGR = array[0..255] of PArrayABGR;

  TThreadMapCombineBase = class(TThreadRegionProcessAbstract)
  private
    FUsedReColor: boolean;
    FRecolorConfig: IBitmapPostProcessingConfigStatic;
    FConverterFactory: ILocalCoordConverterFactorySimpe;
    FTempBitmap: TCustomBitmap32;
    FMainTypeMap: TMapType;
  protected
    FTypeMap: TMapType;
    FHTypeMap: TMapType;
    FZoom: byte;
    FPoly: TArrayOfPoint;
    FPolyProjected: IProjectedPolygonLine;
    FMapCalibrationList: IInterfaceList;
    FSplitCount: TPoint;

    FFileName: string;
    FFilePath: string;
    FFileExt: string;
    FCurrentFileName: string;
    FMapRect: TRect;
    FMapSize: TPoint;
    FMapPieceSize: TPoint;
    FCurrentPieceRect: TRect;
    FLastTile: TPoint;
    FMarksImageProvider: IBitmapLayerProvider;

    FNumImgs: integer;
    FNumImgsSaved: integer;

    FUsePrevZoomAtMap: Boolean;
    FUsePrevZoomAtLayer: Boolean;
    FBackGroundColor: TColor32;

    FArray256BGR: P256ArrayBGR;
    FArray256ABGR: P256ArrayABGR;
    sx, ex, sy, ey: integer;
    btmm: TCustomBitmap32;

    function CreateConverterForTileImage(ATile: TPoint): ILocalCoordConverter;
    procedure PrepareTileBitmap(
      ATargetBitmap: TCustomBitmap32;
      AConverter: ILocalCoordConverter
    );
    procedure ProgressFormUpdateOnProgress; virtual;

    procedure SaveRect; virtual; abstract;

    procedure ProcessRegion; override;
    procedure ProcessRecolor(Bitmap: TCustomBitmap32);

    function ReadLine(
      ALineNumber: Integer;
      APLine: Pointer;
      APArray256Buffer: Pointer;
      AReadWithAlphaChannel: Boolean = False
    ): Boolean; virtual;
  public
    constructor Create(
      AViewConfig: IGlobalViewMainConfig;
      AMarksImageProvider: IBitmapLayerProvider;
      ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      AMapCalibrationList: IInterfaceList;
      AFileName: string;
      APolygon: ILonLatPolygonLine;
      AProjectedPolygon: IProjectedPolygonLine;
      ASplitCount: TPoint;
      Azoom: byte;
      Atypemap: TMapType;
      AHtypemap: TMapType;
      AusedReColor: Boolean;
      ARecolorConfig: IBitmapPostProcessingConfigStatic
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  gnugettext,
  i_MapCalibration,
  u_ResStrings,
  u_GeoFun;

{ TMapCombineThreadBase }

constructor TThreadMapCombineBase.Create(
  AViewConfig: IGlobalViewMainConfig;
  AMarksImageProvider: IBitmapLayerProvider;
  ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  AMapCalibrationList: IInterfaceList;
  AFileName: string;
  APolygon: ILonLatPolygonLine;
  AProjectedPolygon: IProjectedPolygonLine;
  ASplitCount: TPoint;
  Azoom: byte;
  Atypemap: TMapType;
  AHtypemap: TMapType;
  AusedReColor: Boolean;
  ARecolorConfig: IBitmapPostProcessingConfigStatic
);
begin
  inherited Create(APolygon);
  FZoom := Azoom - 1;
  FSplitCount := ASplitCount;
  FFilePath := ExtractFilePath(AFileName);
  FFileExt := ExtractFileExt(AFileName);
  FFileName := ChangeFileExt(ExtractFileName(AFileName), '');
  FTypeMap := Atypemap;
  FHTypeMap := AHtypemap;
  if FTypeMap <> nil then begin
    FMainTypeMap := FTypeMap;
  end else begin
    FMainTypeMap := FHTypeMap;
  end;
  if FMainTypeMap = nil then begin
    raise Exception.Create( _('No one Map or Layer are selected!') );
  end;
  FUsedReColor := AusedReColor;
  FRecolorConfig := ARecolorConfig;
  FMarksImageProvider := AMarksImageProvider;
  FMapCalibrationList := AMapCalibrationList;
  FConverterFactory := ALocalConverterFactory;
  FTempBitmap := TCustomBitmap32.Create;
  FUsePrevZoomAtMap := AViewConfig.UsePrevZoomAtMap;
  FUsePrevZoomAtLayer := AViewConfig.UsePrevZoomAtLayer;
  FBackGroundColor := Color32(AViewConfig.BackGroundColor);
  FPolyProjected := AProjectedPolygon;
end;

procedure TThreadMapCombineBase.ProgressFormUpdateOnProgress;
var
  VProcessed: Integer;
begin
  VProcessed := round((FTilesProcessed / FTilesToProcess) * 100);
  ProgressFormUpdateProgressAndLine1(
    VProcessed,
    SAS_STR_Processed + ': ' + inttostr(VProcessed) + '%'
  );
end;


function TThreadMapCombineBase.CreateConverterForTileImage(
  ATile: TPoint): ILocalCoordConverter;
var
  VTileRect: TRect;
  VBitmapTileRect: TRect;
begin
  VTileRect := FMainTypeMap.GeoConvert.TilePos2PixelRect(ATile, FZoom);
  VBitmapTileRect.Left := 0;
  VBitmapTileRect.Top := 0;
  VBitmapTileRect.Right := VTileRect.Right - VTileRect.Left;
  VBitmapTileRect.Bottom := VTileRect.Bottom - VTileRect.Top;
  Result := FConverterFactory.CreateConverter(VBitmapTileRect, FZoom, FMainTypeMap.GeoConvert, DoublePoint(1, 1), DoublePoint(VTileRect.TopLeft));
end;

destructor TThreadMapCombineBase.Destroy;
begin
  FreeAndNil(FTempBitmap);
  inherited;
end;

procedure TThreadMapCombineBase.PrepareTileBitmap(
  ATargetBitmap: TCustomBitmap32;
  AConverter: ILocalCoordConverter
);
var
  VLoadResult: Boolean;
begin
  if FTypeMap <> nil then begin
    VLoadResult := FTypeMap.LoadBtimapUni(ATargetBitmap, AConverter.GetRectInMapPixel, AConverter.GetZoom, AConverter.GetGeoConverter, FUsePrevZoomAtMap, True, True);
    if not VLoadResult then begin
      ATargetBitmap.Clear(FBackGroundColor);
    end;
  end else begin
    ATargetBitmap.Clear(0);
  end;
  if FHTypeMap <> nil then begin
    VLoadResult := FHTypeMap.LoadBtimapUni(FTempBitmap, AConverter.GetRectInMapPixel, AConverter.GetZoom, AConverter.GetGeoConverter, FUsePrevZoomAtLayer, True, True);
    if VLoadResult then begin
      FTempBitmap.DrawMode := dmBlend;
      ATargetBitmap.Draw(0, 0, FTempBitmap);
    end;
  end;
  ProcessRecolor(ATargetBitmap);
  if FMarksImageProvider <> nil then begin
    FMarksImageProvider.GetBitmapRect(OperationID, CancelNotifier, ATargetBitmap, AConverter);
  end;
end;

procedure TThreadMapCombineBase.ProcessRecolor(Bitmap: TCustomBitmap32);
begin
  if FUsedReColor then begin
    FRecolorConfig.ProcessBitmap(Bitmap);
  end;
end;

procedure TThreadMapCombineBase.ProcessRegion;
var
  i, j, pti: integer;
  VProcessTiles: Int64;
  VLen: Integer;
begin
  inherited;
  VLen := FPolyProjected.Count;
  SetLength(FPoly, VLen);
  for i := 0 to VLen - 1 do begin
    FPoly[i] := Point(Trunc(FPolyProjected.Points[i].X), Trunc(FPolyProjected.Points[i].Y));
  end;

  VProcessTiles := GetDwnlNum(FMapRect.TopLeft, FMapRect.BottomRight, @FPoly[0], VLen, true);
  GetMinMax(FMapRect, @FPoly[0], VLen, false);

  FMapSize.X := FMapRect.Right - FMapRect.Left;
  FMapSize.Y := FMapRect.Bottom - FMapRect.Top;
  FMapPieceSize.X := FMapSize.X div FSplitCount.X;
  FMapPieceSize.Y := FMapSize.Y div FSplitCount.Y;

  FTilesToProcess := FMapPieceSize.Y;
  FTilesProcessed := 0;

  FNumImgs := FSplitCount.X * FSplitCount.Y;
  FNumImgsSaved := 0;

  ProgressFormUpdateCaption(
    Format(
      SAS_STR_MapCombineProgressLine0,
      [FMapSize.X div 256 + 1, FMapSize.Y div 256 + 1, VProcessTiles]
    ),
    Format(
      SAS_STR_MapCombineProgressCaption,
      [FMapSize.X, FMapSize.Y, FNumImgs]
    )
  );

  ProgressFormUpdateOnProgress;

  for i := 1 to FSplitCount.X do begin
    for j := 1 to FSplitCount.Y do begin
      FCurrentPieceRect.Left := FMapRect.Left + FMapPieceSize.X * (i - 1);
      FCurrentPieceRect.Right := FMapRect.Left + FMapPieceSize.X * i;
      FCurrentPieceRect.Top := FMapRect.Top + FMapPieceSize.Y * (j - 1);
      FCurrentPieceRect.Bottom := FMapRect.Top + FMapPieceSize.Y * j;

      if (FSplitCount.X > 1) or (FSplitCount.Y > 1) then begin
        FCurrentFileName := FFilePath + FFileName + '_' + inttostr(i) + '-' + inttostr(j) + FFileExt;
      end else begin
        FCurrentFileName := FFilePath + FFileName + FFileExt;
      end;

      for pti := 0 to FMapCalibrationList.Count - 1 do begin
        try
          (FMapCalibrationList.get(pti) as IMapCalibration).SaveCalibrationInfo(FCurrentFileName, FCurrentPieceRect.TopLeft, FCurrentPieceRect.BottomRight, FZoom, FTypeMap.GeoConvert);
        except
          //TODO: ƒобавить сюда нормальную обработку ошибок.
        end;
      end;
      saveRECT;
    end;
  end;
end;

function TThreadMapCombineBase.ReadLine(
  ALineNumber: Integer;
  APLine: Pointer;
  APArray256Buffer: Pointer;
  AReadWithAlphaChannel: Boolean = False
): Boolean;
var
  i, j, rarri, lrarri, p_x, p_y, Asx, Asy, Aex, Aey, starttile: integer;
  p: PColor32array;
  VBytesToRead: Byte;
  VConverter: ILocalCoordConverter;
  VBuf: Pointer;
begin
  if AReadWithAlphaChannel then begin
    VBytesToRead := 4;
  end else begin
    VBytesToRead := 3;
  end;
  if ALineNumber < (256 - sy) then begin
    starttile := sy + ALineNumber;
  end else begin
    starttile := (ALineNumber - (256 - sy)) mod 256;
  end;
  if (starttile = 0) or (ALineNumber = 0) then begin
    FTilesProcessed := ALineNumber;
    ProgressFormUpdateOnProgress;
    p_y := (FCurrentPieceRect.Top + ALineNumber) - ((FCurrentPieceRect.Top + ALineNumber) mod 256);
    p_x := FCurrentPieceRect.Left - (FCurrentPieceRect.Left mod 256);
    lrarri := 0;
    rarri := 0;
    if ALineNumber > (255 - sy) then begin
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
      if not (RgnAndRgn(@FPoly[0], Length(FPoly), p_x + 128, p_y + 128, false)) then begin
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
          if AReadWithAlphaChannel then begin
            VBuf := @P256ArrayABGR(APArray256Buffer)[j]^[rarri];
          end else begin
            VBuf := @P256ArrayBGR(APArray256Buffer)[j]^[rarri];
          end;
          CopyMemory(VBuf, Pointer(integer(p) + (i * 4)), VBytesToRead);
          inc(rarri);
        end;
      end;
      lrarri := rarri;
      Asx := 0;
      inc(p_x, 256);
    end;
  end;
  if AReadWithAlphaChannel then begin
    VBuf := P256ArrayABGR(APArray256Buffer)^[starttile];
  end else begin
    VBuf := P256ArrayBGR(APArray256Buffer)^[starttile];
  end;
  CopyMemory(APLine, VBuf, (FCurrentPieceRect.Right - FCurrentPieceRect.Left) * VBytesToRead);
  Result := True;
end;

end.
