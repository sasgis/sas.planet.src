unit i_GlobalViewMainConfig;

interface

uses
  Graphics,
  i_ConfigDataElement;

type
  IGlobalViewMainConfig = interface(IConfigDataElement)
    ['{9174C624-07F7-41B7-8533-C77D1E05B270}']
    function GetBackGroundColor: TColor;
    procedure SetBackGroundColor(const AValue: TColor);
    property BackGroundColor: TColor read GetBackGroundColor write SetBackGroundColor;

    function GetUsePrevZoomAtMap: Boolean;
    procedure SetUsePrevZoomAtMap(const AValue: Boolean);
    property UsePrevZoomAtMap: Boolean read GetUsePrevZoomAtMap write SetUsePrevZoomAtMap;

    function GetUsePrevZoomAtLayer: Boolean;
    procedure SetUsePrevZoomAtLayer(const AValue: Boolean);
    property UsePrevZoomAtLayer: Boolean read GetUsePrevZoomAtLayer write SetUsePrevZoomAtLayer;
  end;

implementation

end.
