unit i_Bitmap32Surface;

interface

uses
  Types,
  t_Bitmap32,
  i_Bitmap32Static;

type
 IBitmap32Surface = interface
    ['{90C9BDF0-E2CC-4369-BA0B-96C46A70A19C}']
    function GetSize: TPoint;
    property Size: TPoint read GetSize;

    function GetData: PColor32Array;
    property Data: PColor32Array read GetData;

    function IsEmpty: Boolean;

    procedure Clear;
    procedure FullFill(const AFillColor: TColor32);

    procedure FillRect(const ARect: TRect; const AValue: TColor32);
    procedure FrameRect(const ARect: TRect; const AValue: TColor32);
    procedure Line(const APoint1, APoint2: TPoint; const AValue: TColor32);
    procedure SetPixel(const APoint: TPoint; const AValue: TColor32);

    procedure DrawBitmapStatic(const ASource: IBitmap32Static);
    procedure DrawBitmapStaticAt(const APosition: TPoint; const ASource: IBitmap32Static);
    procedure DrawBitmapData(const ASize: TPoint; const AData: PColor32Array);
    procedure DrawBitmapDataAt(const APosition: TPoint; const ASize: TPoint; const AData: PColor32Array);

    function MakeAndClear: IBitmap32Static;
  end;

  IBitmap32SurfaceFactory = interface
    ['{91C9BDF0-E2CC-4369-BA0B-96C46A70A19C}']
    function BuildEmpty(const ASize: TPoint; const AFillColor: TColor32): IBitmap32Surface;
    function BuildFillColor(const ASize: TPoint; const AFillColor: TColor32): IBitmap32Surface;
    function BuildByData(const ASize: TPoint; const AData: PColor32Array): IBitmap32Surface;
    function BuildByBitmap32Static(const ASource: IBitmap32Static): IBitmap32Surface;
  end;

implementation

end.
