unit i_ICalcLineLayerConfig;

interface

uses
  GR32,
  i_IConfigDataElement;

type
  ICalcLineLayerConfig = interface(IConfigDataElement)
    ['{D3B5B8D5-B389-4406-9881-9704030CDD1E}']
    function GetLenShow: Boolean;
    procedure SetLenShow(AValue: Boolean);
    property LenShow: Boolean read GetLenShow write SetLenShow;

    function GetLineColor: TColor32;
    procedure SetLineColor(AValue: TColor32);
    property LineColor: TColor32 read GetLineColor write SetLineColor;

    function GetLineWidth: integer;
    procedure SetLineWidth(AValue: integer);
    property LineWidth: integer read GetLineWidth write SetLineWidth;

    function GetPointFillColor: TColor32;
    procedure SetPointFillColor(AValue: TColor32);
    property PointFillColor: TColor32 read GetPointFillColor write SetPointFillColor;

    function GetPointRectColor: TColor32;
    procedure SetPointRectColor(AValue: TColor32);
    property PointRectColor: TColor32 read GetPointRectColor write SetPointRectColor;

    function GetPointFirstColor: TColor32;
    procedure SetPointFirstColor(AValue: TColor32);
    property PointFirstColor: TColor32 read GetPointFirstColor write SetPointFirstColor;

    function GetPointActiveColor: TColor32;
    procedure SetPointActiveColor(AValue: TColor32);
    property PointActiveColor: TColor32 read GetPointActiveColor write SetPointActiveColor;

    function GetPointSize: integer;
    procedure SetPointSize(AValue: integer);
    property PointSize: integer read GetPointSize write SetPointSize;

    function GetTextColor: TColor32;
    procedure SetTextColor(AValue: TColor32);
    property TextColor: TColor32 read GetTextColor write SetTextColor;

    function GetTextBGColor: TColor32;
    procedure SetTextBGColor(AValue: TColor32);
    property TextBGColor: TColor32 read GetTextBGColor write SetTextBGColor;
 end;

implementation

end.
