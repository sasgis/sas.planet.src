unit i_PointCaptionsLayerConfig;

interface

uses
  GR32,
  i_ConfigDataElement;

type
  IPointCaptionsLayerConfigStatic = interface
    ['{E826C149-C1AC-490E-BA46-63E74289B7DF}']
    function GetVisible: Boolean;
    property Visible: Boolean read GetVisible;

    function GetShowAzimuth: Boolean;
    property ShowAzimuth: Boolean read GetShowAzimuth;

    function GetShowLastPointOnly: Boolean;
    property ShowLastPointOnly: Boolean read GetShowLastPointOnly;

    function GetFontSize: Integer;
    property FontSize: Integer read GetFontSize;

    function GetLastPointFontSize: Integer;
    property LastPointFontSize: Integer read GetLastPointFontSize;

    function GetTextColor: TColor32;
    property TextColor: TColor32 read GetTextColor;

    function GetTextBGColor: TColor32;
    property TextBGColor: TColor32 read GetTextBGColor;
  end;

  IPointCaptionsLayerConfig = interface(IConfigDataElement)
    ['{7B3B1D25-519A-43AE-9FFA-B27982DA37D7}']
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;

    function GetShowAzimuth: Boolean;
    procedure SetShowAzimuth(AValue: Boolean);
    property ShowAzimuth: Boolean read GetShowAzimuth write SetShowAzimuth;

    function GetShowLastPointOnly: Boolean;
    procedure SetShowLastPointOnly(const AValue: Boolean);
    property ShowLastPointOnly: Boolean read GetShowLastPointOnly write SetShowLastPointOnly;

    function GetFontSize: Integer;
    procedure SetFontSize(AValue: Integer);
    property FontSize: Integer read GetFontSize write SetFontSize;

    function GetLastPointFontSize: Integer;
    procedure SetLastPointFontSize(AValue: Integer);
    property LastPointFontSize: Integer read GetLastPointFontSize write SetLastPointFontSize;

    function GetTextColor: TColor32;
    procedure SetTextColor(const AValue: TColor32);
    property TextColor: TColor32 read GetTextColor write SetTextColor;

    function GetTextBGColor: TColor32;
    procedure SetTextBGColor(const AValue: TColor32);
    property TextBGColor: TColor32 read GetTextBGColor write SetTextBGColor;

    function GetStatic: IPointCaptionsLayerConfigStatic;
  end;

implementation

end.
