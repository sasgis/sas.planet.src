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

    function GetIsInited: Boolean;
    property IsInited: Boolean read GetIsInited;

    procedure Clear;
    procedure FullFill(const AFillColor: TColor32);

    procedure FillRect(
      const ARect: TRect;
      const AValue: TColor32
    );
    procedure FrameRect(
      const ARect: TRect;
      const AValue: TColor32
    );
    procedure Line(
      const APoint1, APoint2: TPoint;
      const AValue: TColor32
    );
    procedure SetPixel(
      const APoint: TPoint;
      const AValue: TColor32
    );

    procedure DrawBitmapStatic(const ASource: IBitmap32Static);
    procedure DrawBitmapStaticAt(
      const APosition: TPoint;
      const ASource: IBitmap32Static
    );
    procedure DrawBitmapData(
      const ASize: TPoint;
      const AData: PColor32Array
    );
    procedure DrawBitmapDataAt(
      const APosition: TPoint;
      const ASize: TPoint;
      const AData: PColor32Array
    );
  end;

  IBitmap32StaticBuilder = interface(IBitmap32Surface)
    ['{DD01C179-C0AE-487B-9EE4-809D217559F9}']
    function MakeStaticAndClear: IBitmap32Static;
    function MakeStaticCopy: IBitmap32Static;
  end;

  IBitmap32StaticBuilderFactory = interface
    ['{91C9BDF0-E2CC-4369-BA0B-96C46A70A19C}']
    function BuildEmpty(const ASize: TPoint): IBitmap32StaticBuilder;
    function BuildFillColor(
      const ASize: TPoint;
      const AFillColor: TColor32
    ): IBitmap32StaticBuilder;
    function BuildByData(
      const ASize: TPoint;
      const AData: PColor32Array
    ): IBitmap32StaticBuilder;
    function BuildByBitmap32Static(const ASource: IBitmap32Static): IBitmap32StaticBuilder;
  end;

implementation

end.
