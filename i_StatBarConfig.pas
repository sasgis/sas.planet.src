unit i_StatBarConfig;

interface

uses
  GR32,
  i_ConfigDataElement;

type
  IStatBarConfig = interface(IConfigDataElement)
    ['{473782BB-AD89-4745-8CBA-93B38EA851E6}']
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;

    function GetHeight: Integer;
    procedure SetHeight(AValue: Integer);
    property Height: Integer read GetHeight write SetHeight;

    function GetMinUpdateTickCount: Cardinal;
    procedure SetMinUpdateTickCount(AValue: Cardinal);
    property MinUpdateTickCount: Cardinal read GetMinUpdateTickCount write SetMinUpdateTickCount;

    function GetBgColor: TColor32;
    procedure SetBgColor(AValue: TColor32);
    property BgColor: TColor32 read GetBgColor write SetBgColor;

    function GetTextColor: TColor32;
    procedure SetTextColor(AValue: TColor32);
    property TextColor: TColor32 read GetTextColor write SetTextColor;

    function GetFontName: string;
    procedure SetFontName(AValue: string);
    property FontName: string read GetFontName write SetFontName;

    function GetFontSize: Integer;
    procedure SetFontSize(AValue: Integer);
    property FontSize: Integer read GetFontSize write SetFontSize;
  end;
  
implementation

end.
