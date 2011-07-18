unit u_TileCacheSimpleGlobal;

interface

uses
  GR32,
  i_VectorDataItemSimple,
  i_MemObjCache,
  i_TileObjCache,
  u_MapType;

type
  TTileCacheSimpleGlobal = class(TInterfacedObject, ITileObjCache)
  private
    FGUID: TGUID;
    FGUIDString: String;
    FMemCache: IMemObjCache;
    function GetMemCacheKey(AXY: TPoint; Azoom: byte): string;
  protected
    procedure Clear;
    procedure DeleteTileFromCache(AXY: TPoint; AZoom: Byte);
    procedure AddTileToCache(AObj: TCustomBitmap32; AXY: TPoint; AZoom: Byte); overload;
    procedure AddTileToCache(AObj: IVectorDataItemList; AXY: TPoint; AZoom: Byte); overload;
    function TryLoadTileFromCache(AObj: TCustomBitmap32; AXY: TPoint; AZoom: Byte): boolean; overload;
    function TryLoadTileFromCache(var AObj: IVectorDataItemList; AXY: TPoint; AZoom: Byte): boolean; overload;
  public
    constructor Create(AMapType: TMapType; AMemCache: IMemObjCache);
  end;


implementation

uses
  SysUtils,
  ComObj;

{ TTileCacheSimpleGlobal }

procedure TTileCacheSimpleGlobal.AddTileToCache(AObj: TCustomBitmap32;
  AXY: TPoint; AZoom: Byte);
begin
  FMemCache.AddTileToCache(AObj, GetMemCacheKey(AXY, AZoom));
end;

procedure TTileCacheSimpleGlobal.AddTileToCache(AObj: IVectorDataItemList;
  AXY: TPoint; AZoom: Byte);
begin
  FMemCache.AddTileToCache(AObj, GetMemCacheKey(AXY, AZoom));
end;

procedure TTileCacheSimpleGlobal.Clear;
begin
  FMemCache.Clear;
end;

constructor TTileCacheSimpleGlobal.Create(AMapType: TMapType; AMemCache: IMemObjCache);
begin
  FGUID := AMapType.Zmp.GUID;
  FGUIDString := GUIDToString(FGUID);
  FMemCache := AMemCache;
end;

procedure TTileCacheSimpleGlobal.DeleteTileFromCache(AXY: TPoint;
  AZoom: Byte);
begin
  FMemCache.DeleteFileFromCache(GetMemCacheKey(AXY, AZoom));
end;

function TTileCacheSimpleGlobal.GetMemCacheKey(AXY: TPoint;
  Azoom: byte): string;
begin
  Result := inttostr(Azoom) + '-' + inttostr(AXY.X) + '-' + inttostr(AXY.Y) + '-' + FGUIDString;
end;

function TTileCacheSimpleGlobal.TryLoadTileFromCache(var AObj: IVectorDataItemList;
  AXY: TPoint; AZoom: Byte): boolean;
begin
  Result := FMemCache.TryLoadFileFromCache(AObj, GetMemCacheKey(AXY, AZoom));
end;

function TTileCacheSimpleGlobal.TryLoadTileFromCache(AObj: TCustomBitmap32;
  AXY: TPoint; AZoom: Byte): boolean;
begin
  Result := FMemCache.TryLoadFileFromCache(AObj, GetMemCacheKey(AXY, AZoom));
end;

end.
