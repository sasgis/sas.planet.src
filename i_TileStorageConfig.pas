unit i_TileStorageConfig;

interface

uses
  i_ConfigDataElement,
  i_TileStorageType,
  i_TileStorage;

type
  ITileStorageConfig = interface(IConfigDataElement)
    ['{068F95DE-9816-4A6F-86BC-A472D255BE18}']
    function GetCachePath: string;
    procedure SetCachePath(AValue: string);
    property CachePath: string read GetCachePath write SetCachePath;

    function GetTileStorageType: ITileStorageType;
    procedure SetTileStorageType(AValue: ITileStorageType);
    property TileStorageType: ITileStorageType read GetTileStorageType write SetTileStorageType;

    function BuildStorage: ITileStorage;
  end;

implementation

end.
