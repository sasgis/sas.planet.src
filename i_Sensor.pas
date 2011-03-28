unit i_Sensor;

interface

uses
  GR32,
  i_ConfigDataElement,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider;

type
  ISensorInfo = interface
    ['{EFD30054-5F65-49DF-8EB9-A4EF816D05D2}']
    function GetGUID: TGUID;
    function GetCaption: string;
    function GetDescription: string;
    function CanReset: Boolean;
    function GetMenuItemName: string;
    function GetSensorViewIID: TGUID;
  end;

  ISensorViewConfig = interface(IConfigDataElement)
    ['{ABA124E3-376F-495E-982C-F3D27F48F610}']
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;

    function GetFontSize: Integer;
    procedure SetFontSize(AValue: Integer);
    property FontSize: Integer read GetFontSize write SetFontSize;
  end;

  ISensorView = interface
    ['{3D7823AF-17D9-495E-901C-BF6435E5C0E1}']
    function GetConfig: ISensorViewConfig;
  end;

  ISensorViewText = interface(ISensorView)
    ['{8345C7D4-BEAD-4242-9ACC-81FBEB571ADE}']
    procedure SetText(AValue: string);
  end;

  ISensorViewBitmap = interface(ISensorView)
    ['{B92A9CE8-82D0-4045-B931-4E85FA77070C}']
    procedure SetBitmap(AValue: TCustomBitmap32);
  end;

  ISensor = interface
    ['{F106BDDC-E596-47EE-99FC-C9A61C7868F4}']
    function GetInfo: ISensorInfo;
    procedure SetView(AView: ISensorView);
  end;

implementation

end.
