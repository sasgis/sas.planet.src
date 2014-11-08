unit i_TileRect;

interface

uses
  Types;

type
  ITileRect = interface
    ['{EF5843E4-504A-45E9-9605-980EF7C04E6B}']
    function GetLeft: Integer;
    property Left: Integer read GetLeft;
    function GetTop: Integer;
    property Top: Integer read GetTop;
    function GetRight: Integer;
    property Right: Integer read GetRight;
    function GetBottom: Integer;
    property Bottom: Integer read GetBottom;

    function GetTopLeft: TPoint;
    property TopLeft: TPoint read GetTopLeft;
    function GetBottomRight: TPoint;
    property BottomRight: TPoint read GetBottomRight;

    function GetRect: TRect;
    property Rect: TRect read GetRect;

    function GetZoom: Byte;
    property Zoom: Byte read GetZoom;

    function IsEqual(const ARect: TRect): Boolean; overload;
    function IsEqual(const ARect: ITileRect): Boolean; overload;
    function IsPointInRect(const APoint: TPoint): Boolean;
    function UnionWithRect(const ARect: TRect): TRect; overload;
    function UnionWithRect(const ARect: ITileRect): TRect; overload;
    function IntersecWithRect(
      out AResultRect: TRect;
      const ARect: TRect
    ): Boolean; overload;
    function IntersecWithRect(
      out AResultRect: TRect;
      const ARect: ITileRect
    ): Boolean; overload;
    function IsIntersecWithRect(const ARect: TRect): Boolean; overload;
    function IsIntersecWithRect(const ARect: ITileRect): Boolean; overload;
  end;

implementation

end.
