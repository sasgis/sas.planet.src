unit i_TileDownloaderConfig;

interface

uses
  i_ConfigDataElement,
  i_InetConfig;

type
  ITileDownloaderConfigStatic = interface
    ['{FECD40CF-A0AF-479A-8CCC-E3363037773E}']
    function GetInetConfigStatic: IInetConfigStatic;
    property InetConfigStatic: IInetConfigStatic read GetInetConfigStatic;

    function GetWaitInterval: Cardinal;
    property WaitInterval: Cardinal read GetWaitInterval;

    function GetIgnoreMIMEType: Boolean;
    property IgnoreMIMEType: Boolean read GetIgnoreMIMEType;

    function GetExpectedMIMETypes: string;
    property ExpectedMIMETypes: string read GetExpectedMIMETypes;

    function GetDefaultMIMEType: string;
    property DefaultMIMEType: string read GetDefaultMIMEType;
  end;

  ITileDownloaderConfig = interface(IConfigDataElement)
    ['{FECD40CF-A0AF-479A-8CCC-E3363037773E}']
    function GetInetConfig: IInetConfig;
    property InetConfig: IInetConfig read GetInetConfig;

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
