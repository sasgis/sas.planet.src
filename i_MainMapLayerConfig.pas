unit i_MainMapLayerConfig;

interface

uses
  i_ThreadConfig,
  i_ConfigDataElement;

type
  IMainMapLayerConfig = interface(IConfigDataElement)
    ['{CE0C51B0-801C-4539-9E53-31A49B3989D5}']
    function GetUsePrevZoomAtMap: Boolean;
    procedure SetUsePrevZoomAtMap(const AValue: Boolean);
    property UsePrevZoomAtMap: Boolean read GetUsePrevZoomAtMap write SetUsePrevZoomAtMap;

    function GetUsePrevZoomAtLayer: Boolean;
    procedure SetUsePrevZoomAtLayer(const AValue: Boolean);
    property UsePrevZoomAtLayer: Boolean read GetUsePrevZoomAtLayer write SetUsePrevZoomAtLayer;

    function GetThreadConfig: IThreadConfig;
    property ThreadConfig: IThreadConfig read GetThreadConfig;
  end;

implementation

end.
