unit u_ThreadMapCombineJPG;

interface

uses
  Types,
  SysUtils,
  Classes,
  LibJpegWrite,
  GR32,
  i_NotifierOperation,
  i_RegionProcessProgressInfo,
  i_BitmapLayerProvider,
  i_MapCalibration,
  i_VectorItemLonLat,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_ImageLineProvider,
  u_ResStrings,
  u_ThreadMapCombineBase;

type
  TThreadMapCombineJPG = class(TThreadMapCombineBase)
  private
    FWidth: Integer;
    FHeight: Integer;
    FQuality: Integer;
    FBgColor: TColor32;
    FLineProvider: IImageLineProvider;
    function GetLine(
      Sender: TObject;
      ALineNumber: Integer;
      ALineSize: Cardinal;
      out Abort: Boolean
    ): PByte;
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
      const APolygon: ILonLatPolygon;
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
  Exif,
  t_GeoTypes,
  i_CoordConverter,
  u_ImageLineProvider;

{ TThreadMapCombineJPG }

constructor TThreadMapCombineJPG.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const APolygon: ILonLatPolygon;
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
    AnsiString(Self.ClassName)
  );
  FBgColor := ABgColor;
  FQuality := AQuality;
end;

procedure TThreadMapCombineJPG.SaveRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AFileName: string;
  const AImageProvider: IBitmapLayerProvider;
  const ALocalConverter: ILocalCoordConverter;
  const AConverterFactory: ILocalCoordConverterFactorySimpe
);
const
  JPG_MAX_HEIGHT = 65536;
  JPG_MAX_WIDTH = 65536;
var
  VJpegWriter: TJpegWriter;
  VStream: TFileStream;
  VCurrentPieceRect: TRect;
  VGeoConverter: ICoordConverter;
  VMapPieceSize: TPoint;
  VExif: TExifSimple;
  VCenterLonLat: TDoublePoint;
  VUseBGRAColorSpace: Boolean;
begin
  VGeoConverter := ALocalConverter.GeoConverter;
  VCurrentPieceRect := ALocalConverter.GetRectInMapPixel;
  VMapPieceSize := ALocalConverter.GetLocalRectSize;
  VCenterLonLat := ALocalConverter.GetCenterLonLat;

  VUseBGRAColorSpace := True; // Available for libjpeg-turbo only

  if VUseBGRAColorSpace then begin
    FLineProvider :=
      TImageLineProviderBGRA.Create(
        AImageProvider,
        ALocalConverter,
        AConverterFactory,
        FBgColor
      );
  end else begin
    FLineProvider :=
      TImageLineProviderRGB.Create(
        AImageProvider,
        ALocalConverter,
        AConverterFactory,
        FBgColor
      );
  end;

  FWidth := VMapPieceSize.X;
  FHeight := VMapPieceSize.Y;
  if (FWidth >= JPG_MAX_WIDTH) or (FHeight >= JPG_MAX_HEIGHT) then begin
    raise Exception.CreateFmt(SAS_ERR_ImageIsTooBig, ['JPG', FWidth, JPG_MAX_WIDTH, FHeight, JPG_MAX_HEIGHT, 'JPG']);
  end;
  VStream := TFileStream.Create(AFileName, fmCreate);
  try
    VJpegWriter := TJpegWriter.Create(VStream, VUseBGRAColorSpace);
    try
      VJpegWriter.Width := FWidth;
      VJpegWriter.Height := FHeight;
      VJpegWriter.Quality := FQuality;
      VJpegWriter.AddCommentMarker('Created with SAS.Planet' + #0);
      VExif := TExifSimple.Create(VCenterLonLat.Y, VCenterLonLat.X);
      try
        VJpegWriter.AddExifMarker(VExif.Stream);
      finally
        VExif.Free;
      end;
      VJpegWriter.Compress(Self.GetLine);
    finally
      VJpegWriter.Free;
    end;
  finally
    VStream.Free;
  end;
end;

function TThreadMapCombineJPG.GetLine(
  Sender: TObject;
  ALineNumber: Integer;
  ALineSize: Cardinal;
  out Abort: Boolean
): PByte;
begin
  if ALineNumber mod 256 = 0 then begin
    ProgressFormUpdateOnProgress(ALineNumber / FHeight);
  end;
  Result := FLineProvider.GetLine(OperationID, CancelNotifier, ALineNumber);
  Abort := (Result = nil) or CancelNotifier.IsOperationCanceled(OperationID);
end;

end.
