unit i_IConfigDataElement;

interface

uses
  i_JclNotify,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider;

type
  IConfigDataElement = interface
    ['{AAD224E2-F566-43CC-BBAF-EF9C175009E7}']
    procedure LockRead;
    procedure LockWrite;
    procedure UnlockRead;
    procedure UnlockWrite;
    procedure ReadConfig(AConfigData: IConfigDataProvider);
    procedure WriteConfig(AConfigData: IConfigDataWriteProvider);
    procedure StopNotify;
    procedure StartNotify;
    function GetChangeNotifier: IJclNotifier;
  end;

implementation

end.
