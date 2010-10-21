unit i_IGPSModuleByCOMPortSettings;

interface

uses
  i_JclNotify;

type
  IGPSModuleByCOMPortSettings = interface
    ['{75AC2DE2-4C88-4A0C-A1D1-D99E51995C78}']
    function GetPort: Integer; safecall;
    function GetBaudRate: Integer; safecall;
    function GetConnectionTimeout: Integer; safecall;
    function GetDelay: Integer; safecall;
    function GetNMEALog: Boolean; safecall;
    function GetLogPath: WideString; safecall;

    function GetConfigChangeNotifier: IJclNotifier; safecall;

    property Port: Integer read GetPort;
    property BaudRate: Integer read GetBaudRate;
    property ConnectionTimeout: Integer read GetConnectionTimeout;
    property Delay: Integer read GetDelay;
    property NMEALog: Boolean read GetNMEALog;
    property LogPath: WideString read GetLogPath;
    property ConfigChangeNotifier: IJclNotifier read GetConfigChangeNotifier;
  end;

implementation

end.
