unit i_IGlobalViewMainConfig;

interface

uses
  GR32,
  i_IConfigDataElement;

type
  IGlobalViewMainConfig = interface(IConfigDataElement)
    ['{9174C624-07F7-41B7-8533-C77D1E05B270}']
    function GetBackGroundColor: TColor32;
    procedure SetBackGroundColor(AValue: TColor32);
    property BackGroundColor: TColor32 read GetBackGroundColor write SetBackGroundColor;

    function GetUsePrevZoomAtMap: Boolean;
    procedure SetUsePrevZoomAtMap(AValue: Boolean);
    property UsePrevZoomAtMap: Boolean read GetUsePrevZoomAtMap write SetUsePrevZoomAtMap;

    function GetUsePrevZoomAtLayer: Boolean;
    procedure SetUsePrevZoomAtLayer(AValue: Boolean);
    property UsePrevZoomAtLayer: Boolean read GetUsePrevZoomAtLayer write SetUsePrevZoomAtLayer;
  end;

implementation

end.
