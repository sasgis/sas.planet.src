unit i_IMapLayerNavToPointMarkerConfig;

interface

uses
  GR32,
  i_IConfigDataElement;

type
  IMapLayerNavToPointMarkerConfig = interface(IConfigDataElement)
    ['{7477526C-A086-41F6-9853-6992035DD10E}']
    function GetCrossDistInPixels: Double;
    procedure SetCrossDistInPixels(AValue: Double);
    property CrossDistInPixels: Double read GetCrossDistInPixels write SetCrossDistInPixels;

    function GetMarkerArrowSize: Integer;
    procedure SetMarkerArrowSize(AValue: Integer);
    property MarkerArrowSize: Integer read GetMarkerArrowSize write SetMarkerArrowSize;

    function GetMarkerArrowColor: TColor32;
    procedure SetMarkerArrowColor(AValue: TColor32);
    property MarkerArrowColor: TColor32 read GetMarkerArrowColor write SetMarkerArrowColor;

    function GetMarkerCrossSize: Integer;
    procedure SetMarkerCrossSize(AValue: Integer);
    property MarkerCrossSize: Integer read GetMarkerCrossSize write SetMarkerCrossSize;

    function GetMarkerCrossColor: TColor32;
    procedure SetMarkerCrossColor(AValue: TColor32);
    property MarkerCrossColor: TColor32 read GetMarkerCrossColor write SetMarkerCrossColor;

    function GetMarkerArrow: TCustomBitmap32;
    function GetMarkerCross: TCustomBitmap32;
  end;

implementation

end.
