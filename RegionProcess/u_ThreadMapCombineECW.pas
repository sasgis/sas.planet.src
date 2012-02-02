unit u_ThreadMapCombineECW;

interface

uses
  Types,
  SysUtils,
  Classes,
  GR32,
  i_GlobalViewMainConfig,
  i_BitmapLayerProvider,
  i_LocalCoordConverter,
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
  TTileRowBGRData = array of PArrayBGR;

  TThreadMapCombineECW = class(TThreadMapCombineBase)
  private
    FPreparedData: TTileRowBGRData;
    FQuality: Integer;

    FPreparedConverter: ILocalCoordConverter;
    FTempBitmap: TCustomBitmap32;

    function PrepareConverterForLocalLine(ALine: Integer): ILocalCoordConverter;
    function GetLocalLine(ALine: Integer): PArrayBGR;
    procedure AddTile(
      ATargetBitmap: TCustomBitmap32;
      AConverter: ILocalCoordConverter
    );
    procedure PrepareBufferMem(ARect: TRect);
    procedure ClearBuffer;
    procedure PrepareBufferData(AConverter: ILocalCoordConverter);
    function ReadLine(ALine: Integer; var LineR, LineG, LineB: PLineRGB): Boolean;
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
  i_CoordConverter;

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

procedure TThreadMapCombineECW.AddTile(
  ATargetBitmap: TCustomBitmap32;
  AConverter: ILocalCoordConverter
);
var
  i: Integer;
  j: Integer;
  VColor32Line: PColor32Array;
  VBitmapRect: TRect;
  VPreparedMapRect: TRect;
  VIntersectionAtPrepared: TRect;
  VIntersectionAtBitmap: TRect;
  VSource: PColor32Entry;
  VTarget: ^TBGR;
begin
  VBitmapRect := AConverter.GetRectInMapPixel;
  VPreparedMapRect := FPreparedConverter.GetRectInMapPixel;
  Assert(VBitmapRect.Top = VPreparedMapRect.Top);
  Assert(VBitmapRect.Bottom = VPreparedMapRect.Bottom);
  Assert(VBitmapRect.Right > VPreparedMapRect.Left);
  Assert(VBitmapRect.Left < VPreparedMapRect.Right);

  if VBitmapRect.Left < VPreparedMapRect.Left then begin
    VBitmapRect.Left := VPreparedMapRect.Left;
  end;
  if VBitmapRect.Right > VPreparedMapRect.Right then begin
    VBitmapRect.Right := VPreparedMapRect.Right;
  end;
  VIntersectionAtPrepared := FPreparedConverter.MapRect2LocalRect(VBitmapRect);
  VIntersectionAtBitmap := AConverter.MapRect2LocalRect(VBitmapRect);
  for i := 0 to (VBitmapRect.Bottom - VBitmapRect.Top - 1) do begin
    VColor32Line := ATargetBitmap.ScanLine[i];
    VSource := @VColor32Line[VIntersectionAtBitmap.Left];
    VTarget := @FPreparedData[i][VIntersectionAtPrepared.Left];
    for j := VBitmapRect.Left to  VBitmapRect.Right - 1 do begin
      VTarget.B := VSource.B;
      VTarget.G := VSource.G;
      VTarget.R := VSource.R;
      Inc(VSource);
      Inc(VTarget);
    end;
  end;
end;

procedure TThreadMapCombineECW.ClearBuffer;
var
  i: Integer;
begin
  for i := 0 to Length(FPreparedData) - 1 do begin
    if FPreparedData[i] <> nil then begin
      FreeMem(FPreparedData[i]);
      FPreparedData[i] := nil;
    end;
  end;
  FPreparedData := nil;
end;

function TThreadMapCombineECW.GetLocalLine(ALine: Integer): PArrayBGR;
var
  VPreparedLocalRect: TRect;
begin
  VPreparedLocalRect := FPreparedConverter.GetLocalRect;
  Assert(ALine >= VPreparedLocalRect.Top);
  Assert(ALine < VPreparedLocalRect.Bottom);
  Result := FPreparedData[ALine - VPreparedLocalRect.Top];
end;

procedure TThreadMapCombineECW.PrepareBufferData(AConverter: ILocalCoordConverter);
var
  VTile: TPoint;
  i, j: Integer;
  VTileConverter: ILocalCoordConverter;
  VTileSize: TPoint;
  VRectOfTile: TRect;
begin
  PrepareBufferMem(AConverter.GetLocalRect);
  VRectOfTile := MainGeoConverter.PixelRect2TileRect(AConverter.GetRectInMapPixel, Zoom);
  for i := VRectOfTile.Top to VRectOfTile.Bottom - 1 do begin
    VTile.Y := i;
    for j := VRectOfTile.Left to VRectOfTile.Right - 1 do begin
      VTile.X := j;
      VTileConverter := CreateConverterForTileImage(VTile);
      VTileSize := VTileConverter.GetLocalRectSize;
      PrepareTileBitmap(FTempBitmap, VTileConverter);
      AddTile(FTempBitmap, VTileConverter);
    end;
  end;
end;

procedure TThreadMapCombineECW.PrepareBufferMem(ARect: TRect);
var
  VLinesExists: Integer;
  VLinesNeed: Integer;
  VWidth: Integer;
  i: Integer;
begin
  VWidth := ARect.Right - ARect.Left;
  VLinesNeed :=  ARect.Bottom - ARect.Top;
  VLinesExists := Length(FPreparedData);
  if VLinesExists < VLinesNeed then begin
    SetLength(FPreparedData, VLinesNeed);
    for i := VLinesExists to VLinesNeed - 1 do begin
      GetMem(FPreparedData[i], (VWidth + 1) * sizeof(TBGR));
    end;
  end;
end;

function TThreadMapCombineECW.PrepareConverterForLocalLine(
  ALine: Integer
): ILocalCoordConverter;
var
  VPixel: TPoint;
  VTile: TPoint;
  VPreparedMapRect: TRect;
  VCurrentPieceRect: TRect;
  VCurrentPieceMapRect: TRect;
  VPreparedLocalRect: TRect;
  VPixelRect: TRect;
begin
  VCurrentPieceRect := CurrentPieceConverter.GetLocalRect;
  VPixel :=
    CurrentPieceConverter.LocalPixel2MapPixel(
      Point(
        VCurrentPieceRect.Left,
        ALine
      )
    );
  VTile := MainGeoConverter.PixelPos2TilePos(VPixel, Zoom);
  VPixelRect := MainGeoConverter.TilePos2PixelRect(VTile, Zoom);
  VCurrentPieceMapRect := CurrentPieceConverter.GetRectInMapPixel;
  VPreparedMapRect :=
    Rect(
      VCurrentPieceMapRect.Left,
      VPixelRect.Top,
      VCurrentPieceMapRect.Right,
      VPixelRect.Bottom
    );

  VPreparedLocalRect := CurrentPieceConverter.MapRect2LocalRect(VPreparedMapRect);
  Result :=
    ConverterFactory.CreateConverter(
      VPreparedLocalRect,
      Zoom,
      MainGeoConverter,
      DoublePoint(1, 1),
      DoublePoint(VCurrentPieceMapRect.TopLeft)
    );
end;

function TThreadMapCombineECW.ReadLine(ALine: Integer; var LineR, LineG,
  LineB: PLineRGB): Boolean;
var
  VRect: TRect;
  VRGB: PArrayBGR;
  i: Integer;
begin
  if (FPreparedConverter <> nil) then begin
    VRect := FPreparedConverter.GetLocalRect;
    if (ALine < VRect.Top) or (ALine >= VRect.Bottom) then begin
      FPreparedConverter := nil;
    end;
  end;

  if FPreparedConverter =  nil then begin
    FTilesProcessed := ALine;
    ProgressFormUpdateOnProgress;
    FPreparedConverter := PrepareConverterForLocalLine(ALine);
    PrepareBufferData(FPreparedConverter);
  end;
  VRGB := GetLocalLine(ALine);
  for i := 0 to (CurrentPieceRect.Right - CurrentPieceRect.Left) - 1 do begin
    LineR[i] := VRGB[i].R;
    LineG[i] := VRGB[i].G;
    LineB[i] := VRGB[i].B;
  end;
  Result := True;
end;

procedure TThreadMapCombineECW.SaveRect;
var
  Datum, Proj: string;
  Units: TCellSizeUnits;
  CellIncrementX, CellIncrementY, OriginX, OriginY: Double;
  errecw: integer;
  VECWWriter: TECWWrite;
begin
  VECWWriter := TECWWrite.Create;
  try
    FTempBitmap := TCustomBitmap32.Create;
    try
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
        ClearBuffer;
      end;
    finally
      FTempBitmap.Free;
    end;
  finally
    FreeAndNil(VECWWriter);
  end;
end;

end.
