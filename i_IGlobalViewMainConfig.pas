unit i_IGlobalViewMainConfig;

interface

uses
  Graphics,
  i_IConfigDataElement;

type
  IGlobalViewMainConfig = interface(IConfigDataElement)
    ['{9174C624-07F7-41B7-8533-C77D1E05B270}']
    function GetBackGroundColor: TColor;
    procedure SetBackGroundColor(AValue: TColor);
    property BackGroundColor: TColor read GetBackGroundColor write SetBackGroundColor;

    function GetUsePrevZoomAtMap: Boolean;
    procedure SetUsePrevZoomAtMap(AValue: Boolean);
    property UsePrevZoomAtMap: Boolean read GetUsePrevZoomAtMap write SetUsePrevZoomAtMap;

    function GetUsePrevZoomAtLayer: Boolean;
    procedure SetUsePrevZoomAtLayer(AValue: Boolean);
    property UsePrevZoomAtLayer: Boolean read GetUsePrevZoomAtLayer write SetUsePrevZoomAtLayer;
  end;

implementation

end.
