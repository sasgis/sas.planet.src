unit i_TileStorageTypeInfo;

interface

type
  ITileStorageTypeInfo = interface
    ['{EBB122FB-5382-49CA-A265-3BEA89694B0E}']
    function GetIsFileCache: Boolean;
    function GetIsReadOnly: boolean;
    function GetAllowDelete: boolean;
    function GetAllowSave: boolean;
    function GetAllowMultiWrite: Boolean;
    function GetAllowMultiRead: Boolean;
  end;
  
implementation

end.

