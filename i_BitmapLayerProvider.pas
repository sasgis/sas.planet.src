unit i_BitmapLayerProvider;

interface

uses
  Types,
  GR32,
  i_LocalCoordConverter;

type
  IBitmapLayerProvider = interface
    ['{A4E2AEE1-1747-46F1-9836-173AFB62CCF9}']
    procedure GetBitmapRect(
      ATargetBmp: TCustomBitmap32;
      ALocalConverter: ILocalCoordConverter
    );
  end;

implementation

end.
