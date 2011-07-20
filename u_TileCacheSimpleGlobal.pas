unit u_TileCacheSimpleGlobal;

interface

uses
  GR32,
  i_VectorDataItemSimple,
  i_MemObjCache,
  i_TileObjCache,
  u_MapType;

type
  TTileCacheSimpleGlobalVector = class(TInterfacedObject, ITileObjCacheVector)
  private
    FGUID: TGUID;
    FGUIDString: String;
    FMemCache: IMemObjCacheVector;
    function GetMemCacheKey(AXY: TPoint; Azoom: byte): string;
  protected
    procedure Clear;
    procedure DeleteTileFromCache(AXY: TPoint; AZoom: Byte);
    procedure AddTileToCache(AObj: IVectorDataItemList; AXY: TPoint; AZoom: Byte);
    function TryLoadTileFromCache(var AObj: IVectorDataItemList; AXY: TPoint; AZoom: Byte): boolean;
  public
    constructor Create(AMapType: TMapType; AMemCache: IMemObjCacheVector);
  end;

  TTileCacheSimpleGlobalBitmap = class(TInterfacedObject, ITileObjCacheBitmap)
  private
    FGUID: TGUID;
    FGUIDString: String;
    FMemCache: IMemObjCacheBitmap;
    function GetMemCacheKey(AXY: TPoint; Azoom: byte): string;
    function GetMemCachePreKey(AXY: TPoint; Azoom: byte): string;
  protected
    procedure Clear;
    procedure DeleteTileFromCache(AXY: TPoint; AZoom: Byte);
    procedure AddTileToCache(AObj: TCustomBitmap32; AXY: TPoint; AZoom: Byte);
    function TryLoadTileFromCache(AObj: TCustomBitmap32; AXY: TPoint; AZoom: Byte): boolean;
    procedure AddTilePreToCache(AObj: TCustomBitmap32; AXY: TPoint; AZoom: Byte);
    function TryLoadTilePreFromCache(AObj: TCustomBitmap32; AXY: TPoint; AZoom: Byte): boolean;
  public
    constructor Create(AMapType: TMapType; AMemCache: IMemObjCacheBitmap);
  end;


implementation

uses
  SysUtils,
  ComObj;

{ TTileCacheSimpleGlobal }

procedure TTileCacheSimpleGlobalVector.AddTileToCache(AObj: IVectorDataItemList;
  AXY: TPoint; AZoom: Byte);
begin
  FMemCache.AddTileToCache(AObj, GetMemCacheKey(AXY, AZoom));
end;

procedure TTileCacheSimpleGlobalVector.Clear;
begin
  FMemCache.Clear;
end;

constructor TTileCacheSimpleGlobalVector.Create(AMapType: TMapType; AMemCache: IMemObjCacheVector);
begin
  FGUID := AMapType.Zmp.GUID;
  FGUIDString := GUIDToString(FGUID);
  FMemCache := AMemCache;
end;

procedure TTileCacheSimpleGlobalVector.DeleteTileFromCache(AXY: TPoint;
  AZoom: Byte);
begin
  FMemCache.DeleteFileFromCache(GetMemCacheKey(AXY, AZoom));
end;

function TTileCacheSimpleGlobalVector.GetMemCacheKey(AXY: TPoint;
  Azoom: byte): string;
begin
  Result := inttostr(Azoom) + '-' + inttostr(AXY.X) + '-' + inttostr(AXY.Y) + '-' + FGUIDString;
end;

function TTileCacheSimpleGlobalVector.TryLoadTileFromCache(var AObj: IVectorDataItemList;
  AXY: TPoint; AZoom: Byte): boolean;
begin
  Result := FMemCache.TryLoadFileFromCache(AObj, GetMemCacheKey(AXY, AZoom));
end;

{ TTileCacheSimpleGlobalBitmap }

procedure TTileCacheSimpleGlobalBitmap.AddTilePreToCache(AObj: TCustomBitmap32;
  AXY: TPoint; AZoom: Byte);
begin
  FMemCache.AddTileToCache(AObj, GetMemCachePreKey(AXY, AZoom));
end;

procedure TTileCacheSimpleGlobalBitmap.AddTileToCache(AObj: TCustomBitmap32;
  AXY: TPoint; AZoom: Byte);
begin
  FMemCache.AddTileToCache(AObj, GetMemCacheKey(AXY, AZoom));
end;

procedure TTileCacheSimpleGlobalBitmap.Clear;
begin
  FMemCache.Clear;
end;

constructor TTileCacheSimpleGlobalBitmap.Create(AMapType: TMapType;
  AMemCache: IMemObjCacheBitmap);
begin
  FGUID := AMapType.Zmp.GUID;
  FGUIDString := GUIDToString(FGUID);
  FMemCache := AMemCache;
end;

procedure TTileCacheSimpleGlobalBitmap.DeleteTileFromCache(AXY: TPoint;
  AZoom: Byte);
begin
  FMemCache.DeleteFileFromCache(GetMemCacheKey(AXY, AZoom));
end;

function TTileCacheSimpleGlobalBitmap.GetMemCacheKey(AXY: TPoint;
  Azoom: byte): string;
begin
  Result := inttostr(Azoom) + '-' + inttostr(AXY.X) + '-' + inttostr(AXY.Y) + '-' + FGUIDString;
end;

function TTileCacheSimpleGlobalBitmap.GetMemCachePreKey(AXY: TPoint;
  Azoom: byte): string;
begin
  Result := inttostr(Azoom) + '-' + inttostr(AXY.X) + '-' + inttostr(AXY.Y) + '-Pre-' + FGUIDString;
end;

function TTileCacheSimpleGlobalBitmap.TryLoadTileFromCache(
  AObj: TCustomBitmap32; AXY: TPoint; AZoom: Byte): boolean;
begin
  Result := FMemCache.TryLoadFileFromCache(AObj, GetMemCacheKey(AXY, AZoom));
end;

function TTileCacheSimpleGlobalBitmap.TryLoadTilePreFromCache(
  AObj: TCustomBitmap32; AXY: TPoint; AZoom: Byte): boolean;
begin
  Result := FMemCache.TryLoadFileFromCache(AObj, GetMemCachePreKey(AXY, AZoom));
end;

end.
