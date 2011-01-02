unit i_IMapLayerGPSMarkerConfig;

interface

uses
  GR32,
  i_IConfigDataElement;

type
  IMapLayerGPSMarkerConfig = interface
    ['{A8E08D39-7805-4A4C-AD78-F8CAD66919C7}']
    function GetMinMoveSpeed: Double;
    procedure SetMinMoveSpeed(AValue: Double);
    property MinMoveSpeed: Double read GetMinMoveSpeed write SetMinMoveSpeed;

    function GetMarkerMovedSize: Integer;
    procedure SetMarkerMovedSize(AValue: Integer);
    property MarkerMovedSize: Integer read GetMarkerMovedSize write SetMarkerMovedSize;

    function GetMarkerMovedColor: TColor32;
    procedure SetMarkerMovedColor(AValue: TColor32);
    property MarkerMovedColor: TColor32 read GetMarkerMovedColor write SetMarkerMovedColor;

    function GetMarkerStopedSize: Integer;
    procedure SetMarkerStopedSize(AValue: Integer);
    property MarkerStopedSize: Integer read GetMarkerStopedSize write SetMarkerStopedSize;

    function GetMarkerStopedColor: TColor32;
    procedure SetMarkerStopedColor(AValue: TColor32);
    property MarkerStopedColor: TColor32 read GetMarkerStopedColor write SetMarkerStopedColor;

    function GetMarkerMoved: TCustomBitmap32;
    function GetMarkerStoped: TCustomBitmap32;
  end;

implementation

end.
