unit i_IGPSModuleByCOMPortConfig;

interface

uses
  i_IGPSModuleByCOMPortSettings,
  i_IConfigDataElement;

type
  IGPSModuleByCOMPortConfig = interface(IConfigDataElement)
    ['{75AC2DE2-4C88-4A0C-A1D1-D99E51995C78}']
    function GetPort: Integer;
    procedure SetPort(AValue: Integer);
    property Port: Integer read GetPort write SetPort;

    function GetBaudRate: Integer;
    procedure SetBaudRate(AValue: Integer);
    property BaudRate: Integer read GetBaudRate write SetBaudRate;

    function GetConnectionTimeout: Integer;
    procedure SetConnectionTimeout(AValue: Integer);
    property ConnectionTimeout: Integer read GetConnectionTimeout write SetConnectionTimeout;

    function GetDelay: Integer;
    procedure SetDelay(AValue: Integer);
    property Delay: Integer read GetDelay write SetDelay;

    function GetNMEALog: Boolean;
    procedure SetNMEALog(AValue: Boolean);
    property NMEALog: Boolean read GetNMEALog write SetNMEALog;

    function GetLogPath: WideString;
    property LogPath: WideString read GetLogPath;

    function GetStatic: IGPSModuleByCOMPortSettings;
  end;

implementation

end.
