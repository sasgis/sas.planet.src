unit i_ITileStorageType;

interface

uses
  i_JclNotify,
  i_ITileStorageTypeInfo,
  i_ITileStorage;

type
  ITileStorageType = interface
    ['{EBB122FB-5382-49CA-A265-3BEA89694B0E}']
    function GetInfo: ITileStorageTypeInfo;
    function GetBasePath: string;
    procedure SetBasePath(AValue: string);
    function BuildStorage(AName: string): ITileStorage;
    function GetBasePathChangeNotifier: IJclNotifier;
    function GetCaption: string;
  end;

implementation

end.
