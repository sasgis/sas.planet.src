unit i_GPSModuleByCOMPortSettings;

interface

type
  IGPSModuleByCOMPortSettings = interface
    ['{1E9AF59D-8988-4747-9952-5D17A0B0DB33}']
    function GetPort: Integer; safecall;
    property Port: Integer read GetPort;

    function GetBaudRate: Integer; safecall;
    property BaudRate: Integer read GetBaudRate;

    function GetConnectionTimeout: Integer; safecall;
    property ConnectionTimeout: Integer read GetConnectionTimeout;

    function GetDelay: Integer; safecall;
    property Delay: Integer read GetDelay;

    function GetNMEALog: Boolean; safecall;
    property NMEALog: Boolean read GetNMEALog;

    function GetLogPath: WideString; safecall;
    property LogPath: WideString read GetLogPath;
  end;

implementation

end.
