unit i_GPSModuleByCOMPortConfig;

interface

uses
  i_GPSModuleByCOMPortSettings,
  i_ConfigDataElement;

type
  IGPSModuleByCOMPortConfig = interface(IConfigDataElement)
    ['{75AC2DE2-4C88-4A0C-A1D1-D99E51995C78}']
    function GetPort: Integer;
    procedure SetPort(const AValue: Integer);
    property Port: Integer read GetPort write SetPort;

    function GetBaudRate: Integer;
    procedure SetBaudRate(const AValue: Integer);
    property BaudRate: Integer read GetBaudRate write SetBaudRate;

    function GetConnectionTimeout: Integer;
    procedure SetConnectionTimeout(const AValue: Integer);
    property ConnectionTimeout: Integer read GetConnectionTimeout write SetConnectionTimeout;

    function GetDelay: Integer;
    procedure SetDelay(const AValue: Integer);
    property Delay: Integer read GetDelay write SetDelay;

    function GetNMEALog: Boolean;
    procedure SetNMEALog(const AValue: Boolean);
    property NMEALog: Boolean read GetNMEALog write SetNMEALog;

    function GetLogPath: WideString;
    property LogPath: WideString read GetLogPath;

    function GetStatic: IGPSModuleByCOMPortSettings;
  end;

implementation

end.
