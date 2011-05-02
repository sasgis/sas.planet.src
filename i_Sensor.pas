unit i_Sensor;

interface

uses
  GR32,
  i_JclNotify,
  i_ConfigDataElement;

type
  ISensor = interface(IConfigDataElement)
    ['{EFD30054-5F65-49DF-8EB9-A4EF816D05D2}']
    function GetGUID: TGUID;
    function GetCaption: string;
    function GetDescription: string;
    function GetMenuItemName: string;
    function CanReset: Boolean;
    procedure Reset;
    function GetSensorTypeIID: TGUID;
    function GetDataUpdateNotifier: IJclNotifier;
  end;

  ISensorText = interface(ISensor)
  ['{9FBEF687-7C1E-4BA6-85D7-ECD16E2F1A7A}']
    function GetText: string;
  end;

  ISensorBitmap = interface(ISensor)
  ['{6A1BB26A-13DE-4533-BA3F-188769BF71D6}']
    procedure GetBitmap(ATarget: TCustomBitmap32);
  end;

  ISensorViewConfig = interface(IConfigDataElement)
    ['{ABA124E3-376F-495E-982C-F3D27F48F610}']
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;
  end;

  ISensorView = interface
    ['{3D7823AF-17D9-495E-901C-BF6435E5C0E1}']
    function GetConfig: ISensorViewConfig;
    function GetSensor: ISensor;
  end;

implementation

end.
