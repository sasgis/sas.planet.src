unit i_Bitmap8Static;

interface

uses
  GR32;

type
  IBitmap8Static = interface
    ['{BCBD7AF8-3785-4FFD-8473-FE3F45F7A372}']
    function GetSize: TPoint;
    property Size: TPoint read GetSize;

    function GetData: PByte;
    property Data: PByte read GetData;

    function GetPalette: PColor32Array;
    property Palette: PColor32Array read GetPalette;

    function GetPaletteSize: Integer;
    property PaletteSize: Integer read GetPaletteSize;
  end;

implementation

end.
