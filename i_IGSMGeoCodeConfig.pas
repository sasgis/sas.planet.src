unit i_IGSMGeoCodeConfig;

interface

uses
  i_ConfigDataElement;
  
type
  IGSMGeoCodeConfig = interface(IConfigDataElement)
    ['{E60F1967-FE2D-4C3C-81D1-D99B50A0F21F}']
    function GetUseGSMByCOM: Boolean;
    function GetPortName: string;
    function GetBaudRate: Cardinal;
    function GetWaitTime: Cardinal;
    procedure SetUseGSMByCOM(AValue: Boolean);
    procedure SetPortName(AValue: string);
    procedure SetBaudRate(AValue: Cardinal);
    procedure SetWaitTime(AValue: Cardinal);
  end;

implementation

end.
