unit u_TileCacheSimpleGlobal;

interface

uses
  GR32,
  u_KmlInfoSimple,
  i_IMemObjCache,
  i_ITileObjCache,
  UMapType;

type
  TTileCacheSimpleGlobal = class(TInterfacedObject, ITileObjCache)
  private
    FGUID: TGUID;
    FGUIDString: String;
    FMemCache: IMemObjCache;
    procedure Clear;
    procedure DeleteTileFromCache(AXY: TPoint; AZoom: Byte);
    procedure AddTileToCache(AObj: TCustomBitmap32; AXY: TPoint; AZoom: Byte); overload;
    procedure AddTileToCache(AObj: TKmlInfoSimple; AXY: TPoint; AZoom: Byte); overload;
    function TryLoadTileFromCache(AObj: TCustomBitmap32; AXY: TPoint; AZoom: Byte): boolean; overload;
    function TryLoadTileFromCache(AObj: TKmlInfoSimple; AXY: TPoint; AZoom: Byte): boolean; overload;
    function GetMemCacheKey(AXY: TPoint; Azoom: byte): string;
  public
    constructor Create(AMapType: TMapType);
  end;


implementation

uses
  SysUtils,
  ComObj,
  u_GlobalState;

{ TTileCacheSimpleGlobal }

procedure TTileCacheSimpleGlobal.AddTileToCache(AObj: TCustomBitmap32;
  AXY: TPoint; AZoom: Byte);
begin
  FMemCache.AddTileToCache(AObj, GetMemCacheKey(AXY, AZoom));
end;

procedure TTileCacheSimpleGlobal.AddTileToCache(AObj: TKmlInfoSimple;
  AXY: TPoint; AZoom: Byte);
begin
  FMemCache.AddTileToCache(AObj, GetMemCacheKey(AXY, AZoom));
end;

procedure TTileCacheSimpleGlobal.Clear;
begin
  FMemCache.Clear;
end;

constructor TTileCacheSimpleGlobal.Create(AMapType: TMapType);
begin
  FGUID := AMapType.GUID;
  FGUIDString := GUIDToString(FGUID);
  FMemCache := GState.MainFileCache;
end;

procedure TTileCacheSimpleGlobal.DeleteTileFromCache(AXY: TPoint;
  AZoom: Byte);
begin
  FMemCache.DeleteFileFromCache(GetMemCacheKey(AXY, AZoom));
end;

function TTileCacheSimpleGlobal.GetMemCacheKey(AXY: TPoint;
  Azoom: byte): string;
begin
  Result := inttostr(Azoom)+'-'+inttostr(AXY.X)+'-'+inttostr(AXY.Y) +'-'+FGUIDString;
end;

function TTileCacheSimpleGlobal.TryLoadTileFromCache(AObj: TKmlInfoSimple;
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
