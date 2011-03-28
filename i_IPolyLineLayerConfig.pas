unit i_IPolyLineLayerConfig;

interface

uses
  GR32,
  i_ConfigDataElement;

type
  IPolyLineLayerConfig = interface(IConfigDataElement)
    ['{5B334D74-C1B7-4C5D-96C2-9EA4D02698EF}']
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
 end;

implementation

end.
