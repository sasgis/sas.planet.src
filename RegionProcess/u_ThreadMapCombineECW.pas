unit u_ThreadMapCombineECW;

interface

uses
  SysUtils,
  Classes,
  GR32,
  i_NotifierOperation,
  i_BitmapLayerProvider,
  i_RegionProcessProgressInfo,
  i_LocalCoordConverter,
  i_VectorItemLonLat,
  i_ImageLineProvider,
  i_MapCalibration,
  i_LocalCoordConverterFactorySimpe,
  u_ECWWrite,
  u_GeoFun,
  t_GeoTypes,
  u_ThreadMapCombineBase;

type
  TThreadMapCombineECW = class(TThreadMapCombineBase)
  private
    FImageLineProvider: IImageLineProvider;
    FLinesCount: Integer;
    FQuality: Integer;
    FBgColor: TColor32;
    function ReadLine(
      ALine: Integer;
      var LineR, LineG, LineB: PLineRGB
    ): Boolean;
  protected
    procedure SaveRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AFileName: string;
      const AImageProvider: IBitmapLayerProvider;
      const ALocalConverter: ILocalCoordConverter;
      const AConverterFactory: ILocalCoordConverterFactorySimpe
    ); override;
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const APolygon: IGeometryLonLatMultiPolygon;
      const ATargetConverter: ILocalCoordConverter;
      const AImageProvider: IBitmapLayerProvider;
      const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      const AMapCalibrationList: IMapCalibrationList;
      const AFileName: string;
      const ASplitCount: TPoint;
      ABgColor: TColor32;
      AQuality: Integer
    );
  end;

implementation

uses
  LibECW,
  i_CoordConverter,
  u_ImageLineProvider,
  u_ResStrings;

constructor TThreadMapCombineECW.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const APolygon: IGeometryLonLatMultiPolygon;
  const ATargetConverter: ILocalCoordConverter;
  const AImageProvider: IBitmapLayerProvider;
  const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  const AMapCalibrationList: IMapCalibrationList;
  const AFileName: string;
  const ASplitCount: TPoint;
  ABgColor: TColor32;
  AQuality: Integer
);
begin
  inherited Create(
    AProgressInfo,
    APolygon,
    ATargetConverter,
    AImageProvider,
    ALocalConverterFactory,
    AMapCalibrationList,
    AFileName,
    ASplitCount,
    Self.ClassName
  );
  FBgColor := ABgColor;
  FQuality := AQuality;
end;

function TThreadMapCombineECW.ReadLine(
  ALine: Integer;
  var LineR, LineG,
  LineB: PLineRGB
): Boolean;
type
  TBGR = packed record
    B: Byte;
    G: Byte;
    R: Byte;
  end;
  PArrayBGR = ^TArrayBGR;
  TArrayBGR = array [0..0] of TBGR;
var
  VRGB: PArrayBGR;
  i: Integer;
  VWidth: Integer;
begin
  VWidth := FImageLineProvider.LocalConverter.GetLocalRectSize.X;
  VRGB := FImageLineProvider.GetLine(OperationID, CancelNotifier, ALine);
  for i := 0 to VWidth - 1 do begin
    LineR[i] := VRGB[i].R;
    LineG[i] := VRGB[i].G;
    LineB[i] := VRGB[i].B;
  end;
  if ALine mod 256 = 0 then begin
    ProgressFormUpdateOnProgress(ALine / FLinesCount);
  end;
  Result := True;
end;

procedure TThreadMapCombineECW.SaveRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AFileName: string;
  const AImageProvider: IBitmapLayerProvider;
  const ALocalConverter: ILocalCoordConverter;
  const AConverterFactory: ILocalCoordConverterFactorySimpe
);
var
  Datum, Proj: string;
  Units: TCellSizeUnits;
  CellIncrementX, CellIncrementY, OriginX, OriginY: Double;
  errecw: integer;
  VECWWriter: TECWWrite;
  VCurrentPieceRect: TRect;
  VGeoConverter: ICoordConverter;
  VMapPieceSize: TPoint;
begin
  VECWWriter := TECWWrite.Create;
  try
    FImageLineProvider :=
      TImageLineProviderBGR.Create(
        AImageProvider,
        ALocalConverter,
        AConverterFactory,
        FBgColor
      );
    VGeoConverter := ALocalConverter.GeoConverter;
    VCurrentPieceRect := ALocalConverter.GetRectInMapPixel;
    VMapPieceSize := ALocalConverter.GetLocalRectSize;
    FLinesCount := VMapPieceSize.Y;
    Datum := 'EPSG:' + IntToStr(VGeoConverter.Datum.EPSG);
    Proj := 'EPSG:' + IntToStr(VGeoConverter.GetProjectionEPSG);
    Units := VGeoConverter.GetCellSizeUnits;
    CalculateWFileParams(
      ALocalConverter.GeoConverter.PixelPos2LonLat(VCurrentPieceRect.TopLeft, ALocalConverter.Zoom),
      ALocalConverter.GeoConverter.PixelPos2LonLat(VCurrentPieceRect.BottomRight, ALocalConverter.Zoom),
      VMapPieceSize.X, VMapPieceSize.Y, VGeoConverter,
      CellIncrementX, CellIncrementY, OriginX, OriginY
    );
    errecw :=
      VECWWriter.Encode(
        OperationID,
        CancelNotifier,
        AFileName,
        VMapPieceSize.X,
        VMapPieceSize.Y,
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
    FreeAndNil(VECWWriter);
  end;
end;

end.
