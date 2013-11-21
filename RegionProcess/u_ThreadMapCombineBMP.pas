unit u_ThreadMapCombineBMP;

interface

uses
  SysUtils,
  Classes,
  GR32,
  i_NotifierOperation,
  i_BitmapLayerProvider,
  i_RegionProcessProgressInfo,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_VectorItemLonLat,
  i_MapCalibration,
  u_ThreadMapCombineBase,
  LibBMP;

type
  TThreadMapCombineBMP = class(TThreadMapCombineBase)
  private
    FBgColor: TColor32;
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
      ABgColor: TColor32
    );
  end;

implementation

uses
  gnugettext,
  i_ImageLineProvider,
  u_ImageLineProvider,
  u_ResStrings;

constructor TThreadMapCombineBMP.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const APolygon: ILonLatPolygon;
  const ATargetConverter: ILocalCoordConverter;
  const AImageProvider: IBitmapLayerProvider;
  const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  const AMapCalibrationList: IMapCalibrationList;
  const AFileName: string;
  const ASplitCount: TPoint;
  ABgColor: TColor32
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
end;

procedure TThreadMapCombineBMP.SaveRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AFileName: string;
  const AImageProvider: IBitmapLayerProvider;
  const ALocalConverter: ILocalCoordConverter;
  const AConverterFactory: ILocalCoordConverterFactorySimpe
);
const
  BMP_MAX_WIDTH = 32768;
  BMP_MAX_HEIGHT = 32768;
var
  i: Integer;
  VBMP: TBitmapFile;
  VLineBGR: Pointer;
  VSize: TPoint;
  VLineProvider: IImageLineProvider;
begin
  VSize := ALocalConverter.GetLocalRectSize;

  if (VSize.X >= BMP_MAX_WIDTH) or (VSize.Y >= BMP_MAX_HEIGHT) then begin
    raise Exception.CreateFmt(SAS_ERR_ImageIsTooBig, ['BMP', VSize.X, BMP_MAX_WIDTH, VSize.Y, BMP_MAX_HEIGHT, 'BMP']);
  end;

  VBMP := TBitmapFile.Create(AFileName, VSize.X, VSize.Y);
  try
    VLineProvider :=
      TImageLineProviderBGR.Create(
        AImageProvider,
        ALocalConverter,
        AConverterFactory,
        FBgColor
      );
    for i := 0 to VSize.Y - 1 do begin
      VLineBGR := VLineProvider.GetLine(AOperationID, ACancelNotifier, i);
      if VLineBGR <> nil then begin
        if not VBMP.WriteLine(i, VLineBGR) then begin
          raise Exception.Create(_('BMP: Line write failure!'));
        end;
      end else begin
        raise Exception.Create(_('BMP: Fill line failure!'));
      end;

      if CancelNotifier.IsOperationCanceled(OperationID) then begin
        Break;
      end;
      if i mod 256 = 0 then begin
        ProgressFormUpdateOnProgress(i / VSize.Y);
      end;
    end;
  finally
    VBMP.Free;
  end;
end;

end.
