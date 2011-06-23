unit i_TileStorageTypeListItem;

interface

uses
  i_TileStorageType;

type
  ITileStorageTypeListItem = interface
    ['{103AA969-E2D0-4E66-8B8D-F78E6D442E7D}']
    function GetGUID: TGUID;
    property GUID: TGUID read GetGUID;

    function GetStorageType: ITileStorageType;
    property StorageType: ITileStorageType read GetStorageType;

    function GetCanUseAsDefault: Boolean;
    property CanUseAsDefault: Boolean read GetCanUseAsDefault;
  end;


implementation

end.
