unit i_MapLayerGPSMarkerConfig;

interface

uses
  GR32,
  i_ConfigDataElement,
  i_BitmapMarkerProviderSimpleConfig;

type
  IMapLayerGPSMarkerConfig = interface(IConfigDataElement)
    ['{A8E08D39-7805-4A4C-AD78-F8CAD66919C7}']
    function GetMinMoveSpeed: Double;
    procedure SetMinMoveSpeed(AValue: Double);
    property MinMoveSpeed: Double read GetMinMoveSpeed write SetMinMoveSpeed;

    function GetMovedMarkerConfig: IBitmapMarkerProviderSimpleConfig;
    property MovedMarkerConfig: IBitmapMarkerProviderSimpleConfig read GetMovedMarkerConfig;

    function GetStopedMarkerConfig: IBitmapMarkerProviderSimpleConfig;
    property StopedMarkerConfig: IBitmapMarkerProviderSimpleConfig read GetStopedMarkerConfig;
  end;

implementation

end.
