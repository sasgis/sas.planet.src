unit i_TileStorageType;

interface

uses
  i_ConfigDataElement,
  i_TileStorageTypeInfo,
  i_TileStorage;

type
  ITileStorageType = interface
    ['{EBB122FB-5382-49CA-A265-3BEA89694B0E}']
    function GetInfo: ITileStorageTypeInfo;
    property Info: ITileStorageTypeInfo read GetInfo;

    function GetCaption: string;
    property Caption: string read GetCaption;

    function BuildStorage(APath: string): ITileStorage;
  end;

implementation

end.
