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
  i_CoordConverter,
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
    FMainGeoConverter: ICoordConverter;
    FTypeMap: TMapType;
    FHTypeMap: TMapType;
    FPolyProjected: IProjectedPolygon;
    FLine: IProjectedPolygonLine;
    FMapCalibrationList: IInterfaceList;
    FSplitCount: TPoint;
    FFileName: string;
    FFilePath: string;
    FFileExt: string;
    FMapRect: TRect;
    FMapSize: TPoint;
    FMarksImageProvider: IBitmapLayerProvider;
    FUsePrevZoomAtMap: Boolean;
    FUsePrevZoomAtLayer: Boolean;
    FBackGroundColor: TColor32;
    FZoom: byte;
    FCurrentFileName: string;
    FMapPieceSize: TPoint;
    FCurrentPieceRect: TRect;
    FCurrentPieceConverter: ILocalCoordConverter;
  protected
    FLastTile: TPoint;
    property CurrentPieceConverter: ILocalCoordConverter read FCurrentPieceConverter;
    property CurrentPieceRect: TRect read FCurrentPieceRect;
    property MapPieceSize: TPoint read FMapPieceSize;
    property CurrentFileName: string read FCurrentFileName;
    property Zoom: byte read FZoom;
    property ConverterFactory: ILocalCoordConverterFactorySimpe read FConverterFactory;
    property MainGeoConverter: ICoordConverter read FMainGeoConverter;
    property Line: IProjectedPolygonLine read FLine;
    property BackGroundColor: TColor32 read FBackGroundColor;
    function CreateConverterForTileImage(ATile: TPoint): ILocalCoordConverter;
    function PrepareTileBitmap(
      ATargetBitmap: TCustomBitmap32;
      AConverter: ILocalCoordConverter
    ): Boolean;
    procedure ProgressFormUpdateOnProgress; virtual;

    procedure SaveRect; virtual; abstract;

    procedure ProcessRegion; override;
    procedure ProcessRecolor(Bitmap: TCustomBitmap32);

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
      ARecolorConfig: IBitmapPostProcessingConfigStatic
    );
    destructor Destroy; override;
  end;

  TThreadMapCombineBaseWithByLyne = class(TThreadMapCombineBase)
  protected
    FPoly: TArrayOfPoint;

    FArray256BGR: P256ArrayBGR;
    FArray256ABGR: P256ArrayABGR;
    sx, ex, sy, ey: integer;
    btmm: TCustomBitmap32;

    function ReadLine(
      ALineNumber: Integer;
      APLine: Pointer;
      APArray256Buffer: Pointer;
      AReadWithAlphaChannel: Boolean = False
    ): Boolean; virtual;
    procedure ProcessRegion; override;
  end;

implementation

uses
  SysUtils,
  gnugettext,
  i_EnumDoublePoint,
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
  APolygon: ILonLatPolygon;
  AProjectedPolygon: IProjectedPolygon;
  ASplitCount: TPoint;
  Atypemap: TMapType;
  AHtypemap: TMapType;
  AusedReColor: Boolean;
  ARecolorConfig: IBitmapPostProcessingConfigStatic
);
begin
  inherited Create(APolygon);
  FSplitCount := ASplitCount;
  FFilePath := ExtractFilePath(AFileName);
  FFileExt := ExtractFileExt(AFileName);
  FFileName := ChangeFileExt(ExtractFileName(AFileName), '');
  FTypeMap := Atypemap;
  FHTypeMap := AHtypemap;
  FMainGeoConverter := nil;

  if (FTypeMap = nil) and (FHTypeMap = nil) then begin
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
  FZoom := FPolyProjected.Projection.Zoom;
  FMainGeoConverter := FPolyProjected.Projection.GeoConverter;

  FLine := FPolyProjected.Item[0];
  FMapRect := FMainGeoConverter.PixelRectFloat2PixelRect(FLine.Bounds, FZoom);

  FMapSize.X := FMapRect.Right - FMapRect.Left;
  FMapSize.Y := FMapRect.Bottom - FMapRect.Top;
  FMapPieceSize.X := FMapSize.X div FSplitCount.X;
  FMapPieceSize.Y := FMapSize.Y div FSplitCount.Y;

  FTilesToProcess := FMapPieceSize.Y;
  FTilesProcessed := 0;
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
begin
  Result := FConverterFactory.CreateForTile(ATile, FZoom, FMainGeoConverter);
end;

destructor TThreadMapCombineBase.Destroy;
begin
  FreeAndNil(FTempBitmap);
  inherited;
end;

function TThreadMapCombineBase.PrepareTileBitmap(
  ATargetBitmap: TCustomBitmap32;
  AConverter: ILocalCoordConverter
): Boolean;
var
  VLoadResult: Boolean;
  VTileSize: TPoint;
begin
  VTileSize := AConverter.GetLocalRectSize;
  FTempBitmap.SetSize(VTileSize.X, VTileSize.Y);
  Result := False;
  if FLine.IsRectIntersectPolygon(AConverter.GetRectInMapPixelFloat) then begin
    VLoadResult := False;
    if FTypeMap <> nil then begin
      VLoadResult := FTypeMap.LoadBtimapUni(ATargetBitmap, AConverter.GetRectInMapPixel, AConverter.GetZoom, AConverter.GetGeoConverter, FUsePrevZoomAtMap, True, True);
    end;
    if not VLoadResult then begin
      ATargetBitmap.Clear(FBackGroundColor);
    end else begin
      Result := True;
    end;
    if FHTypeMap <> nil then begin
      VLoadResult := FHTypeMap.LoadBtimapUni(FTempBitmap, AConverter.GetRectInMapPixel, AConverter.GetZoom, AConverter.GetGeoConverter, FUsePrevZoomAtLayer, True, True);
      if VLoadResult then begin
        FTempBitmap.DrawMode := dmBlend;
        ATargetBitmap.Draw(0, 0, FTempBitmap);
        Result := True;
      end;
    end;
    if Result then begin
      ProcessRecolor(ATargetBitmap);
    end;
    if FMarksImageProvider <> nil then begin
      FMarksImageProvider.GetBitmapRect(OperationID, CancelNotifier, ATargetBitmap, AConverter);
      Result := True;
    end;
  end else begin
    ATargetBitmap.Clear(FBackGroundColor);
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
  VTileRect: TRect;
  VPath: string;
begin
  inherited;
  VTileRect := FMainGeoConverter.PixelRect2TileRect(FMapRect, FZoom);
  VProcessTiles := VTileRect.Right - VTileRect.Left;
  VProcessTiles := VProcessTiles * (VTileRect.Bottom - VTileRect.Top);

  ProgressFormUpdateCaption(
    Format(
      SAS_STR_MapCombineProgressLine0,
      [FMapSize.X div 256 + 1, FMapSize.Y div 256 + 1, VProcessTiles]
    ),
    Format(
      SAS_STR_MapCombineProgressCaption,
      [FMapSize.X, FMapSize.Y, FSplitCount.X * FSplitCount.Y]
    )
  );

  ProgressFormUpdateOnProgress;

  for i := 1 to FSplitCount.X do begin
    for j := 1 to FSplitCount.Y do begin
      FCurrentPieceRect.Left := FMapRect.Left + FMapPieceSize.X * (i - 1);
      FCurrentPieceRect.Right := FMapRect.Left + FMapPieceSize.X * i;
      FCurrentPieceRect.Top := FMapRect.Top + FMapPieceSize.Y * (j - 1);
      FCurrentPieceRect.Bottom := FMapRect.Top + FMapPieceSize.Y * j;
      FCurrentPieceConverter :=
        FConverterFactory.CreateConverter(
          Rect(0, 0, FMapPieceSize.X, FMapPieceSize.Y),
          FZoom,
          FMainGeoConverter,
          DoublePoint(1, 1),
          DoublePoint(FCurrentPieceRect.TopLeft)
        );
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
      try
        saveRECT;
      except
        on E: Exception do begin
          VPath := FTypeMap.GetTileShowName(FLastTile, FZoom);
          raise Exception.Create(
            E.message + #13#10 + VPath
          );
        end;
      end;
    end;
  end;
end;

{ TThreadMapCombineBaseWithByLyne }

procedure TThreadMapCombineBaseWithByLyne.ProcessRegion;
var
  VLen: Integer;
  VEnum: IEnumProjectedPoint;
  VPoint: TDoublePoint;
  i: Integer;
begin
  VLen := FLine.Count + 1;
  SetLength(FPoly, VLen);
  VEnum := FLine.GetEnum;
  i := 0;
  while VEnum.Next(VPoint) do begin
    FPoly[i] := Point(Trunc(VPoint.X), Trunc(VPoint.Y));
    Inc(i);
  end;
  inherited;
end;

function TThreadMapCombineBaseWithByLyne.ReadLine(
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
      FLastTile := Point(p_x shr 8, p_y shr 8);
      VConverter := CreateConverterForTileImage(FLastTile);
      PrepareTileBitmap(btmm, VConverter);
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
