unit i_ARGBToPaletteConverter;

interface

uses
  ImagingTypes;

type
  IARGBToPaletteConverter = interface
    procedure Convert(var AImage: TImageData);
  end;

implementation

end.
