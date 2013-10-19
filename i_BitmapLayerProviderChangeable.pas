unit i_BitmapLayerProviderChangeable;

interface

uses
  i_Changeable,
  i_BitmapLayerProvider;

type
  IBitmapLayerProviderChangeable = interface(IChangeable)
    ['{92FA4B43-19A9-4B94-BF01-8365EA389248}']
    function GetStatic: IBitmapLayerProvider;
  end;

implementation

end.
