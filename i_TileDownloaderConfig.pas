unit i_TileDownloaderConfig;

interface

uses
  i_ConfigDataElement,
  i_ProxySettings;

type
  ITileDownloaderConfigStatic = interface
    ['{FECD40CF-A0AF-479A-8CCC-E3363037773E}']
    function GetProxyConfigStatic: IProxyConfigStatic;
    property ProxyConfigStatic: IProxyConfigStatic read GetProxyConfigStatic;

    function GetTimeOut: Cardinal;
    property TimeOut: Cardinal read GetTimeOut;

    function GetWaitInterval: Cardinal;
    property WaitInterval: Cardinal read GetWaitInterval;

    function GetSleepOnResetConnection: Cardinal;
    property SleepOnResetConnection: Cardinal read GetSleepOnResetConnection;

    function GetDownloadTryCount: Integer;
    property DownloadTryCount: Integer read GetDownloadTryCount;

    function GetIgnoreMIMEType: Boolean;
    property IgnoreMIMEType: Boolean read GetIgnoreMIMEType;

    function GetExpectedMIMETypes: string;
    property ExpectedMIMETypes: string read GetExpectedMIMETypes;

    function GetDefaultMIMEType: string;
    property DefaultMIMEType: string read GetDefaultMIMEType;
  end;

  ITileDownloaderConfig = interface(IInetConfig)
    ['{FECD40CF-A0AF-479A-8CCC-E3363037773E}']
    function GetWaitInterval: Cardinal;
    procedure SetWaitInterval(AValue: Cardinal);
    property WaitInterval: Cardinal read GetWaitInterval write SetWaitInterval;

    function GetIgnoreMIMEType: Boolean;
    procedure SetIgnoreMIMEType(AValue: Boolean);
    property IgnoreMIMEType: Boolean read GetIgnoreMIMEType write SetIgnoreMIMEType;

    function GetExpectedMIMETypes: string;
    procedure SetExpectedMIMETypes(AValue: string);
    property ExpectedMIMETypes: string read GetExpectedMIMETypes write SetExpectedMIMETypes;

    function GetDefaultMIMEType: string;
    procedure SetDefaultMIMEType(AValue: string);
    property DefaultMIMEType: string read GetDefaultMIMEType write SetDefaultMIMEType;

    function GetStatic: ITileDownloaderConfigStatic;
  end;

implementation

end.
