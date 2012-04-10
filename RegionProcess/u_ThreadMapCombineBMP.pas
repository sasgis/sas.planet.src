unit u_ThreadMapCombineBMP;

interface

uses
  SysUtils,
  Classes,
  GR32,
  i_OperationNotifier,
  i_BitmapLayerProvider,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  u_ResStrings,
  u_ThreadMapCombineBase,
  LibBMP;

type
  TThreadMapCombineBMP = class(TThreadMapCombineBase)
  protected
    procedure SaveRect(
      AOperationID: Integer;
      const ACancelNotifier: IOperationNotifier;
      const AFileName: string;
      const AImageProvider: IBitmapLayerProvider;
      const ALocalConverter: ILocalCoordConverter;
      const AConverterFactory: ILocalCoordConverterFactorySimpe
    ); override;
  end;

implementation

uses
  gnugettext,
  i_ImageLineProvider,
  u_ImageLineProvider;

procedure TThreadMapCombineBMP.SaveRect(
  AOperationID: Integer;
  const ACancelNotifier: IOperationNotifier;
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
        AConverterFactory
      );
    for i := 0 to VSize.Y - 1 do begin
      VLineBGR := VLineProvider.GetLine(AOperationID, ACancelNotifier, i);
      if VLineBGR <> nil then begin
        if not VBMP.WriteLine(i, VLineBGR) then begin
          raise Exception.Create( _('BMP: Line write failure!') );
        end;
      end else begin
        raise Exception.Create( _('BMP: Fill line failure!') );
      end;

      if CancelNotifier.IsOperationCanceled(OperationID) then begin
        Break;
      end;
      if i mod 256 = 0 then begin
        ProgressFormUpdateOnProgress(i/VSize.Y);
      end;
    end;
  finally
    VBMP.Free;
  end;
end;

end.
