unit i_IMarkPolygonLayerConfig;

interface

uses
  GR32,
  i_IPolyLineLayerConfig;

type
  IMarkPolygonLayerConfig = interface(IPolyLineLayerConfig)
    ['{B6E75FAB-86BB-4CE9-B2FD-F3A014FA0B12}']
    function GetFillColor: TColor32;
    procedure SetFillColor(AValue: TColor32);
    property FillColor: TColor32 read GetFillColor write SetFillColor;
 end;

implementation

end.
