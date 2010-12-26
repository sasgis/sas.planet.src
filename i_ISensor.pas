unit i_ISensor;

interface

uses
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider;

type
  ISensor = interface
    ['{F106BDDC-E596-47EE-99FC-C9A61C7868F4}']
    function GetCaption: string;
    function GetDescription: string;
    function GetMenuItemName: string;
    procedure Show;
    procedure Hide;
    function CanReset: Boolean;
    procedure Reset;
    function GetVisible: Boolean;
    procedure LoadConfig(AConfigProvider: IConfigDataProvider);
    procedure SaveConfig(AConfigProvider: IConfigDataWriteProvider);
  end;

implementation

end.
