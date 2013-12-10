unit i_LonLatRect;

interface

uses
  t_GeoTypes;

type
  ILonLatRect = interface
    ['{B93137DE-5BA8-4A3E-885E-0976CE70199E}']
    function GetLeft: Double;
    property Left: Double read GetLeft;
    function GetTop: Double;
    property Top: Double read GetTop;
    function GetRight: Double;
    property Right: Double read GetRight;
    function GetBottom: Double;
    property Bottom: Double read GetBottom;

    function GetTopLeft: TDoublePoint;
    property TopLeft: TDoublePoint read GetTopLeft;
    function GetBottomRight: TDoublePoint;
    property BottomRight: TDoublePoint read GetBottomRight;

    function GetRect: TDoubleRect;
    property Rect: TDoubleRect read GetRect;

    function CalcRectCenter: TDoublePoint;

    function IsEqual(const ARect: TDoubleRect): Boolean; overload;
    function IsEqual(const ARect: ILonLatRect): Boolean; overload;
    function IsPointInRect(const APoint: TDoublePoint): Boolean;
    function UnionWithRect(const ARect: TDoubleRect): TDoubleRect; overload;
    function UnionWithRect(const ARect: ILonLatRect): TDoubleRect; overload;
    function IntersecWithRect(
      out AResultRect: TDoubleRect;
      const ARect: TDoubleRect
    ): Boolean; overload;
    function IntersecWithRect(
      out AResultRect: TDoubleRect;
      const ARect: ILonLatRect
    ): Boolean; overload;
    function IsIntersecWithRect(const ARect: TDoubleRect): Boolean; overload;
    function IsIntersecWithRect(const ARect: ILonLatRect): Boolean; overload;
  end;


implementation

end.
