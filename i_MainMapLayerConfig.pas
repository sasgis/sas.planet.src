unit i_MainMapLayerConfig;

interface

uses
  i_ThreadConfig,
  i_UseTilePrevZoomConfig,
  i_ConfigDataElement;

type
  IMainMapLayerConfig = interface(IConfigDataElement)
    ['{CE0C51B0-801C-4539-9E53-31A49B3989D5}']
    function GetUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
    property UseTilePrevZoomConfig: IUseTilePrevZoomConfig read GetUseTilePrevZoomConfig;

    function GetThreadConfig: IThreadConfig;
    property ThreadConfig: IThreadConfig read GetThreadConfig;
  end;

implementation

end.
