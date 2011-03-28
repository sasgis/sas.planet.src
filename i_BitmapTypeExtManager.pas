unit i_BitmapTypeExtManager;

interface

uses
  i_BitmapTileSaveLoad;

type
  IBitmapTypeExtManager = interface
    ['{946CE320-DA28-4EC4-934E-CD4A671A9D06}']
    function GetIsBitmapType(AType: String): Boolean;
    function GetIsBitmapExt(AExt: String): Boolean;
    function GetExtForType(AType: String): string;
    function GetBitmapLoaderForExt(AExt: String): IBitmapTileLoader;
    function GetBitmapSaverForExt(AExt: String): IBitmapTileSaver;
    function GetBitmapLoaderForType(AType: String): IBitmapTileLoader;
    function GetBitmapSaverForType(AType: String): IBitmapTileSaver;
  end;

implementation

end.
 