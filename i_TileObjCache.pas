unit i_TileObjCache;

interface

uses
  GR32,
  u_KmlInfoSimple;

type
  ITileObjCache = interface
    ['{B52B38D1-C57C-424C-B85B-AC623A54E7B5}']
    procedure Clear;
    procedure DeleteTileFromCache(AXY: TPoint; AZoom: Byte);
    procedure AddTileToCache(AObj: TCustomBitmap32; AXY: TPoint; AZoom: Byte); overload;
    procedure AddTileToCache(AObj: TKmlInfoSimple; AXY: TPoint; AZoom: Byte); overload;
    function TryLoadTileFromCache(AObj: TCustomBitmap32; AXY: TPoint; AZoom: Byte): boolean; overload;
    function TryLoadTileFromCache(AObj: TKmlInfoSimple; AXY: TPoint; AZoom: Byte): boolean; overload;
  end;


implementation

end.
 