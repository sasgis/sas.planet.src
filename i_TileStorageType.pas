unit i_TileStorageType;

interface

uses
  i_TileStorageTypeInfo,
  i_TileStorageTypeConfig,
  i_TileStorage;

type
  ITileStorageType = interface
    ['{EBB122FB-5382-49CA-A265-3BEA89694B0E}']
    function GetGUID: TGUID;
    property GUID: TGUID read GetGUID;

    function GetInfo: ITileStorageTypeInfo;
    property Info: ITileStorageTypeInfo read GetInfo;

    function GetConfig: ITileStorageTypeConfig;
    property Config: ITileStorageTypeConfig read GetConfig;

    function GetCaption: string;
    property Caption: string read GetCaption;

    function BuildStorage(APath: string): ITileStorage;
  end;

implementation

end.
