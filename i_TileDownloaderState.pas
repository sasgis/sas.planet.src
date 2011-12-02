unit i_TileDownloaderState;

interface

uses
  i_JclNotify;

type
  ITileDownloaderStateStatic = interface
    ['{A0A51F5A-7F35-48A2-870D-203739666B64}']
    function GetEnabled: Boolean;
    property Enabled: Boolean read GetEnabled;

    function GetDisableReason: string;
    property DisableReason: string read GetDisableReason;
  end;

  ITileDownloaderStateChangeble = interface
    ['{48AAA474-723C-443E-940E-2D4332ED41F7}']
    function GetStatic: ITileDownloaderStateStatic;

    function GetBeforeChangeNotifier: IJclNotifier;
    property BeforeChangeNotifier: IJclNotifier read GetBeforeChangeNotifier;

    function GetChangeNotifier: IJclNotifier;
    property ChangeNotifier: IJclNotifier read GetChangeNotifier;

    function GetAfterChangeNotifier: IJclNotifier;
    property AfterChangeNotifier: IJclNotifier read GetAfterChangeNotifier;
  end;

implementation

end.
