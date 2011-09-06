unit i_SelectionPolylineLayerConfig;

interface

uses
  GR32,
  i_PolylineLayerConfig;

type
  ISelectionPolylineLayerConfig = interface(IPolylineLayerConfig)
    ['{9E4CE106-9322-4A88-915B-CE5AECED03D2}']
    function GetRadius: Double;
    procedure SetRadius(AValue: Double);

    function GetShadowPolygonColor: TColor32;
    procedure SetShadowPolygonColor(AValue: TColor32);
 end;

implementation

end.
