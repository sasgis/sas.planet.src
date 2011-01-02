unit i_IGPSModuleByCOMPortSettings;

interface

uses
  i_JclNotify,
  i_IConfigDataElement;

type
  IGPSModuleByCOMPortConfigSatic = interface
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

    function GetStatic: IGPSModuleByCOMPortConfigSatic;
  end;

implementation

end.
