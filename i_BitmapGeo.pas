unit i_BitmapGeo;

interface

uses
  GR32,
  i_LocalCoordConverter;

type
  IBitmapGeo = interface
    ['{26047AE0-8E5C-4232-BF16-891E2CE340ED}']
    function GetBitmap: TCustomBitmap32;
    property Bitmap: TCustomBitmap32 read GetBitmap;

    function GetConverter: ILocalCoordConverter;
    property Converter: ILocalCoordConverter read GetConverter;
  end;

  IBitmapGeoTile = interface(IBitmapGeo)
    ['{3DC296FC-3411-483B-AD8C-A46E1D714171}']
    function GetTile: TPoint;
    property Tile: TPoint read GetTile;
  end;

implementation

end.
