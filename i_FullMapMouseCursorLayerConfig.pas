unit i_FullMapMouseCursorLayerConfig;

interface

uses
  i_ConfigDataElement;

type
  IFullMapMouseCursorLayerConfig = interface(IConfigDataElement)
    ['{B0B3C241-966E-47C7-9B8F-C80434B7868D}']
    function GetEnabled: Boolean;
    procedure SetEnabled(AValue: Boolean);
    property Enabled: Boolean read GetEnabled write SetEnabled;

    function GetShowAlways: Boolean;
    procedure SetShowAlways(AValue: Boolean);
    property ShowAlways: Boolean read GetShowAlways write SetShowAlways;
  end;

implementation

end.
