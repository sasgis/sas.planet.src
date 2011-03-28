unit u_ThreadMapCombineBase;

interface

uses
  Classes,
  Types,
  GR32,
  t_GeoTypes,
  i_MarksSimple,
  i_BitmapLayerProvider,
  i_BitmapPostProcessingConfig,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  u_MapType,
  u_ThreadRegionProcessAbstract;

type
  TThreadMapCombineBase = class(TThreadRegionProcessAbstract)
  private
    FUsedReColor: boolean;
    FRecolorConfig: IBitmapPostProcessingConfigStatic;
    FConverterFactory: ILocalCoordConverterFactorySimpe;
    FTempBitmap: TCustomBitmap32;
  protected
    FTypeMap: TMapType;
    FHTypeMap: TMapType;
    FZoom: byte;
    FPoly: TArrayOfPoint;
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

    function CreateConverterForTileImage(ATile: TPoint): ILocalCoordConverter;
    procedure PrepareTileBitmap(ATargetBitmap: TCustomBitmap32; AConverter: ILocalCoordConverter);
    procedure ProgressFormUpdateOnProgress; virtual;

    procedure saveRECT; virtual; abstract;

    procedure ProcessRegion; override;
    procedure ProcessRecolor(Bitmap: TCustomBitmap32);
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
      AMarksSubset: IMarksSubset
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  i_MapCalibration,
  u_MapMarksBitmapLayerProviderByMarksSubset,
  u_LocalCoordConverterFactorySimpe,
  u_GlobalState,
  u_ResStrings,
  u_GeoFun;

{ TMapCombineThreadBase }

constructor TThreadMapCombineBase.Create(
  AMapCalibrationList: IInterfaceList;
  AFileName: string;
  APolygon: TArrayOfDoublePoint;
  ASplitCount: TPoint;
  Azoom: byte;
  Atypemap: TMapType;
  AHtypemap: TMapType;
  AusedReColor: Boolean;
  ARecolorConfig: IBitmapPostProcessingConfigStatic;
  AMarksSubset: IMarksSubset
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
  FUsedReColor := AusedReColor;
  FRecolorConfig := ARecolorConfig;
  if AMarksSubset <> nil then begin
    FMarksImageProvider := TMapMarksBitmapLayerProviderByMarksSubset.Create(AMarksSubset);
  end;
  FMapCalibrationList := AMapCalibrationList;
  FConverterFactory := TLocalCoordConverterFactorySimpe.Create;
  FTempBitmap := TCustomBitmap32.Create;
  FUsePrevZoomAtMap := GState.ViewConfig.UsePrevZoomAtMap;
  FUsePrevZoomAtLayer := GState.ViewConfig.UsePrevZoomAtLayer;
  FBackGroundColor := Color32(GState.ViewConfig.BackGroundColor);
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
begin
  VTileRect := FTypeMap.GeoConvert.TilePos2PixelRect(ATile, FZoom);
  Result := FConverterFactory.CreateConverter(VTileRect, FZoom, FTypeMap.GeoConvert, DoublePoint(1, 1), DoublePoint(0,0));
end;

destructor TThreadMapCombineBase.Destroy;
begin
  FreeAndNil(FTempBitmap);
  inherited;
end;

procedure TThreadMapCombineBase.PrepareTileBitmap(
  ATargetBitmap: TCustomBitmap32; AConverter: ILocalCoordConverter);
var
  VSize: TPoint;
begin
  VSize := AConverter.GetLocalRectSize;
  FTypeMap.LoadBtimapUni(ATargetBitmap, AConverter.GetRectInMapPixel, AConverter.GetZoom, False, AConverter.GetGeoConverter, FUsePrevZoomAtMap, True, True);
  if FHTypeMap <> nil then begin
    FHTypeMap.LoadBtimapUni(FTempBitmap, AConverter.GetRectInMapPixel, AConverter.GetZoom, False, AConverter.GetGeoConverter, FUsePrevZoomAtLayer, True, True);
    FTempBitmap.DrawMode := dmBlend;
    ATargetBitmap.Draw(0, 0, FTempBitmap);
  end;
  if FMarksImageProvider <> nil then begin
    FMarksImageProvider.GetBitmapRect(FTempBitmap, AConverter);
    FTempBitmap.DrawMode := dmBlend;
    ATargetBitmap.Draw(0, 0, FTempBitmap);
  end;
  ProcessRecolor(ATargetBitmap);
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
begin
  inherited;
  FPoly := FTypeMap.GeoConvert.LonLatArray2PixelArray(FPolygLL, FZoom);

  VProcessTiles := GetDwnlNum(FMapRect.TopLeft, FMapRect.BottomRight, FPoly, true);
  GetMinMax(FMapRect, FPoly, false);

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

end.
