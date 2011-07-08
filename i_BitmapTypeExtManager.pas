unit i_BitmapTypeExtManager;

interface

uses
  i_BitmapTileSaveLoad;

type
  IBitmapTypeExtManager = interface
    ['{946CE320-DA28-4EC4-934E-CD4A671A9D06}']
    function GetIsBitmapType(const AType: String): Boolean;
    function GetIsBitmapExt(const AExt: String): Boolean;
    function GetExtForType(const AType: String): string;
    function GetBitmapLoaderForExt(const AExt: String): IBitmapTileLoader;
    function GetBitmapSaverForExt(const AExt: String): IBitmapTileSaver;
    function GetBitmapLoaderForType(const AType: String): IBitmapTileLoader;
    function GetBitmapSaverForType(const AType: String): IBitmapTileSaver;
  end;

implementation

end.
 