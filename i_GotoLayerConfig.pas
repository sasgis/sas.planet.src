unit i_GotoLayerConfig;

interface

uses
  GR32,
  i_ConfigDataElement;

type
  IGotoLayerConfig = interface(IConfigDataElement)
    ['{95096F2B-D3BF-46CC-9826-1D1D973EEC1F}']
    function GetMarker: TCustomBitmap32;
    procedure SetMarker(AValue: TCustomBitmap32);
    property Marker: TCustomBitmap32 read GetMarker write SetMarker;

    function GetMarkerFixedPoint: TPoint;
    procedure SetMarkerFixedPoint(AValue: TPoint);
    property MarkerFixedPoint: TPoint read GetMarkerFixedPoint write SetMarkerFixedPoint;

    function GetShowTickCount: Cardinal;
    procedure SetShowTickCount(AValue: Cardinal);
    property ShowTickCount: Cardinal read GetShowTickCount write SetShowTickCount;
 end;

implementation

end.
