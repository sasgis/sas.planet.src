unit i_IBitmapLayerProvider;

interface

uses
  Types,
  GR32,
  i_ICoordConverter,
  i_ILocalCoordConverter;

type
  IBitmapLayerProviderNew = interface
    ['{A4E2AEE1-1747-46F1-9836-173AFB62CCF9}']
    procedure GetBitmapRect(
      ATargetBmp: TCustomBitmap32;
      ALocalConverter: ILocalCoordConverter
    );
  end;

  IBitmapLayerProvider = interface
    ['{A4E2AEE1-1747-46F1-9836-173AFB62CCF9}']
    procedure GetBitmapRect(
      ATargetBmp: TCustomBitmap32;
      AConverter: ICoordConverter;
      ATargetRect: TRect;
      ATargetZoom: Byte
    );
  end;

implementation

end.
