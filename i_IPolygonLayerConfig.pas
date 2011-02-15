unit i_IPolygonLayerConfig;

interface

uses
  GR32,
  i_IPolyLineLayerConfig;

type
  IPolygonLayerConfig = interface(IPolyLineLayerConfig)
    ['{3A63D380-CEE4-4A55-8706-4B38F379A2B9}']
    function GetFillColor: TColor32;
    procedure SetFillColor(AValue: TColor32);
    property FillColor: TColor32 read GetFillColor write SetFillColor;
 end;

implementation

end.
