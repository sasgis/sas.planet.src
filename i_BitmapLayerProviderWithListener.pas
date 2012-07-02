unit i_BitmapLayerProviderWithListener;

interface

uses
  i_JclNotify,
  i_LocalCoordConverter,
  i_BitmapLayerProvider;

type
  IBitmapLayerProviderWithListener = interface(IBitmapLayerProvider)
    ['{95B7E0FF-1FD5-4239-BBDA-1535BF03A965}']
    procedure SetListener(
      const AListener: IListener;
      const ALocalConverter: ILocalCoordConverter
    );
    procedure RemoveListener;
  end;

implementation

end.

