unit i_BitmapMarker;

interface

uses
  GR32,
  i_JclNotify,
  t_GeoTypes;

type
  IBitmapMarker = interface
    ['{03AB4233-EEEA-4AD6-A194-EFD32345056D}']
    function GetBitmapSize: TPoint;
    property BitmapSize: TPoint read GetBitmapSize;

    function GetBitmap: TCustomBitmap32;
    property Bitmap: TCustomBitmap32 read GetBitmap;

    function GetAnchorPoint: TDoublePoint;
    property AnchorPoint: TDoublePoint read GetAnchorPoint;

    function GetUseDirection: Boolean;
    property UseDirection: Boolean read GetUseDirection;

    function GetDefaultDirection: Double;
    property DefaultDirection: Double read GetDefaultDirection;
  end;

  IBitmapMarkerProvider = interface
    ['{A186F046-0CFB-456A-A6C3-271046CB2CA0}']
    function GetUseDirection: Boolean;
    property UseDirection: Boolean read GetUseDirection;

    function GetMarker: IBitmapMarker;
    function GetMarkerBySize(ASize: Integer): IBitmapMarker;
    function GetMarkerWithRotation(AAngle: Double): IBitmapMarker;
    function GetMarkerWithRotationBySize(AAngle: Double;  ASize: Integer): IBitmapMarker;

    function GetChangeNotifier: IJclNotifier;
  end;

implementation

end.
