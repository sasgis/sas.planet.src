unit i_GSMGeoCodeConfig;

interface

uses
  i_ConfigDataElement;
  
type
  IGSMGeoCodeConfig = interface(IConfigDataElement)
    ['{E60F1967-FE2D-4C3C-81D1-D99B50A0F21F}']
    function GetUseGSMByCOM: Boolean;
    procedure SetUseGSMByCOM(const AValue: Boolean);
    property UseGSMByCOM: Boolean read GetUseGSMByCOM write SetUseGSMByCOM;

    function GetPortName: string;
    procedure SetPortName(const AValue: string);
    property PortName: string read GetPortName write SetPortName;

    function GetBaudRate: Cardinal;
    procedure SetBaudRate(const AValue: Cardinal);
    property BaudRate: Cardinal read GetBaudRate write SetBaudRate;

    function GetWaitTime: Cardinal;
    procedure SetWaitTime(const AValue: Cardinal);
    property WaitTime: Cardinal read GetWaitTime write SetWaitTime;
  end;

implementation

end.
