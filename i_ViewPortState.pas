unit i_ViewPortState;

interface

uses
  Types,
  i_JclNotify,
  t_GeoTypes,
  i_CoordConverter,
  i_LocalCoordConverter,
  i_ConfigDataElement;

type
  IViewPortState = interface(IConfigDataElement)
    ['{F2F2E282-AA3B-48BC-BC09-73FE9C07B723}']
    function GetMainCoordConverter: ICoordConverter;
    procedure SetMainCoordConverter(AValue: ICoordConverter);
    property MainCoordConverter: ICoordConverter read GetMainCoordConverter write SetMainCoordConverter;

    function GetCurrentCoordConverter: ICoordConverter;
    function GetCurrentZoom: Byte;

    function GetVisualCoordConverter: ILocalCoordConverter;


    procedure ChangeViewSize(ANewSize: TPoint);
    procedure ChangeMapPixelByDelta(ADelta: TDoublePoint);
    procedure ChangeMapPixelToVisualPoint(AVisualPoint: TPoint);
    procedure ChangeZoomWithFreezeAtVisualPoint(AZoom: Byte; AFreezePoint: TPoint);
    procedure ChangeZoomWithFreezeAtCenter(AZoom: Byte);

    procedure ChangeLonLat(ALonLat: TDoublePoint);

    procedure MoveTo(Pnt: TPoint);
    procedure ScaleTo(AScale: Double; ACenterPoint: TPoint); overload;
    procedure ScaleTo(AScale: Double); overload;

    function GetScaleChangeNotifier: IJclNotifier;
    property ScaleChangeNotifier: IJclNotifier read GetScaleChangeNotifier;
  end;

implementation

end.
