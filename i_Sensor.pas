unit i_Sensor;

interface

uses
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
  end;

  ISensorConfig = interface(IConfigDataElement)
    ['{ABA124E3-376F-495E-982C-F3D27F48F610}']
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;

    function GetFontSize: Integer;
    procedure SetFontSize(AValue: Integer);
    property FontSize: Integer read GetFontSize write SetFontSize;
  end;

  ISensor = interface
    ['{F106BDDC-E596-47EE-99FC-C9A61C7868F4}']
    procedure Show;
    procedure Hide;
    procedure Reset;
    function GetVisible: Boolean;
    procedure LoadConfig(AConfigProvider: IConfigDataProvider);
    procedure SaveConfig(AConfigProvider: IConfigDataWriteProvider);
  end;

implementation

end.
