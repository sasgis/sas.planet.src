unit i_MapLayerNavToPointMarkerConfig;

interface

uses
  GR32,
  i_ConfigDataElement,
  i_BitmapMarkerProviderSimpleConfig;

type
  IMapLayerNavToPointMarkerConfig = interface(IConfigDataElement)
    ['{7477526C-A086-41F6-9853-6992035DD10E}']
    function GetCrossDistInPixels: Double;
    procedure SetCrossDistInPixels(AValue: Double);
    property CrossDistInPixels: Double read GetCrossDistInPixels write SetCrossDistInPixels;

    function GetArrowMarkerConfig: IBitmapMarkerProviderSimpleConfig;
    property ArrowMarkerConfig: IBitmapMarkerProviderSimpleConfig read GetArrowMarkerConfig;

    function GetReachedMarkerConfig: IBitmapMarkerProviderSimpleConfig;
    property ReachedMarkerConfig: IBitmapMarkerProviderSimpleConfig read GetReachedMarkerConfig;
  end;

implementation

end.
