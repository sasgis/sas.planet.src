unit i_TileStorageType;

interface

uses
  i_ConfigDataElement,
  i_TileStorageTypeInfo,
  i_TileStorage;

type
  ITileStorageType = interface(IConfigDataElement)
    ['{EBB122FB-5382-49CA-A265-3BEA89694B0E}']
    function GetInfo: ITileStorageTypeInfo;
    function BuildStorage(APath: string): ITileStorage;
    function GetCaption: string;
  end;

implementation

end.
