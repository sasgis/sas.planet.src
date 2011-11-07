unit u_MemTileCache;

interface

uses
  Windows,
  Classes,
  SysUtils,
  GR32,
  i_JclNotify,
  i_MainMemCacheConfig,
  i_VectorDataItemSimple,
  i_ObjectWithTTL,
  i_TileObjCache;

type
  TMemTileCacheBase = class(TInterfacedObject, IObjectWithTTL)
  private
    FConfig: IMainMemCacheConfig;
    FConfigListener: IJclListener;

    FCacheList: TStringList;
    FSync: TMultiReadExclusiveWriteSynchronizer;
    FUpdateCounter: Integer;
    FLastUseTime: Cardinal;
    FTTL: Cardinal;
    FCheckInterval: Cardinal;
    procedure OnChangeConfig(Sender: TObject);
    function GetMemCacheKey(AXY: TPoint; Azoom: byte): string;
  protected
    procedure ItemFree(AIndex: Integer); virtual; abstract;
    procedure IncUpdateCounter;
  protected
    function CheckTTLAndGetNextCheckTime(ANow: Cardinal): Cardinal;
  protected
    procedure Clear;
    procedure DeleteTileFromCache(AXY: TPoint; AZoom: Byte);
  public
    constructor Create(
      AConfig: IMainMemCacheConfig
    );
    destructor Destroy; override;
  end;

  TMemTileCacheVector = class(TMemTileCacheBase, ITileObjCacheVector)
  protected
    procedure ItemFree(AIndex: Integer); override;
  protected
    procedure AddTileToCache(AObj: IVectorDataItemList; AXY: TPoint; AZoom: Byte);
    function TryLoadTileFromCache(var AObj: IVectorDataItemList; AXY: TPoint; AZoom: Byte): boolean;
  end;

  TMemTileCacheBitmap = class(TMemTileCacheBase, ITileObjCacheBitmap)
  protected
    procedure ItemFree(AIndex: Integer); override;
  protected
    procedure AddTileToCache(AObj: TCustomBitmap32; AXY: TPoint; AZoom: Byte);
    function TryLoadTileFromCache(AObj: TCustomBitmap32; AXY: TPoint; AZoom: Byte): boolean;
  end;

implementation

uses
  u_NotifyEventListener;

{ TTileCacheBase }

constructor TMemTileCacheBase.Create(
  AConfig: IMainMemCacheConfig
);
begin
  FConfig := AConfig;
  FConfigListener := TNotifyEventListener.Create(Self.OnChangeConfig);
  FConfig.GetChangeNotifier.Add(FConfigListener);

  FCacheList := TStringList.Create;
  FCacheList.Capacity := FConfig.MaxSize;
  FSync := TMultiReadExclusiveWriteSynchronizer.Create;
  FTTL := 40000;
  FCheckInterval := 1000;
  FUpdateCounter := 0;
end;

destructor TMemTileCacheBase.Destroy;
begin
  FConfig.GetChangeNotifier.Remove(FConfigListener);
  FConfigListener := nil;
  FConfig := nil;

  Clear;
  FreeAndNil(FSync);
  FreeAndNil(FCacheList);
  inherited;
end;

function TMemTileCacheBase.CheckTTLAndGetNextCheckTime(ANow: Cardinal): Cardinal;
var
  VCleanTime: Cardinal;
  VCounter: Integer;
begin
  VCounter := InterlockedExchange(FUpdateCounter, 0);
  if VCounter > 0 then begin
    FLastUseTime := ANow;
  end else begin
    VCleanTime := FLastUseTime + FTTL;
    if (VCleanTime <= ANow) or ((ANow < 1 shl 29) and (VCleanTime > 1 shl 30)) then begin
      Clear;
    end;
  end;
  Result := ANow + FCheckInterval;
end;

procedure TMemTileCacheBase.Clear;
var
  i: integer;
begin
  FSync.BeginWrite;
  try
    for i := FCacheList.Count - 1 downto 0 do begin
      ItemFree(i);
      FCacheList.Delete(i);
    end;
  finally
    FSync.EndWrite;
  end;
end;

procedure TMemTileCacheBase.DeleteTileFromCache(AXY: TPoint; AZoom: Byte);
var
  i: Integer;
  VKey: string;
begin
  VKey := GetMemCacheKey(AXY, AZoom);
  FSync.BeginWrite;
  try
    i := FCacheList.IndexOf(VKey);
    if i >= 0 then begin
      ItemFree(i);
      FCacheList.Delete(i);
    end;
  finally
    FSync.EndWrite;
  end;
end;

function TMemTileCacheBase.GetMemCacheKey(AXY: TPoint; Azoom: byte): string;
begin
  Result := inttostr(Azoom) + '-' + inttostr(AXY.X) + '-' + inttostr(AXY.Y);
end;

procedure TMemTileCacheBase.IncUpdateCounter;
begin
  InterlockedIncrement(FUpdateCounter);
end;

procedure TMemTileCacheBase.OnChangeConfig(Sender: TObject);
var
  VNewSize: Integer;
  i: Integer;
begin
  VNewSize := FConfig.MaxSize;
  FSync.BeginWrite;
  try
    if VNewSize <> FCacheList.Capacity then begin
      if VNewSize < FCacheList.Count then begin
        for i := 0 to (FCacheList.Count - VNewSize) - 1 do begin
          ItemFree(0);
          FCacheList.Delete(0);
        end;
      end;
      FCacheList.Capacity := VNewSize;
    end;
  finally
    FSync.EndWrite;
  end;
end;

{ TMemTileCacheVector }

procedure TMemTileCacheVector.AddTileToCache(AObj: IVectorDataItemList;
  AXY: TPoint; AZoom: Byte);
var
  VKey: string;
  i: integer;
begin
  VKey := GetMemCacheKey(AXY, AZoom);
  FSync.BeginWrite;
  try
    i := FCacheList.IndexOf(VKey);
    if (i < 0)and(FCacheList.Capacity>0) then begin
      if (FCacheList.Count >= FCacheList.Capacity)and(FCacheList.Count>0) then begin
        ItemFree(0);
        FCacheList.Delete(0);
      end;
      AObj._AddRef;
      FCacheList.AddObject(VKey, Pointer(AObj));
    end;
  finally
    FSync.EndWrite;
  end;
end;

procedure TMemTileCacheVector.ItemFree(AIndex: Integer);
begin
  IInterface(Pointer(FCacheList.Objects[AIndex]))._Release;
end;

function TMemTileCacheVector.TryLoadTileFromCache(var AObj: IVectorDataItemList;
  AXY: TPoint; AZoom: Byte): boolean;
var
  i: integer;
  VKey: string;
begin
  Result := false;
  VKey := GetMemCacheKey(AXY, AZoom);
  FSync.BeginRead;
  try
    i := FCacheList.IndexOf(VKey);
    if i >= 0 then begin
      AObj := IVectorDataItemList(Pointer(FCacheList.Objects[i]));
      Result := true;
    end;
  finally
    FSync.EndRead;
  end;
  if Result then begin
    IncUpdateCounter;
  end;
end;

{ TMemTileCacheBitmap }

procedure TMemTileCacheBitmap.AddTileToCache(AObj: TCustomBitmap32; AXY: TPoint;
  AZoom: Byte);
var
  btmcache: TCustomBitmap32;
  VKey: string;
  i: integer;
begin
  VKey := GetMemCacheKey(AXY, AZoom);
  FSync.BeginWrite;
  try
    i := FCacheList.IndexOf(VKey);
    if (i < 0)and(FCacheList.Capacity>0) then begin
      if (FCacheList.Count >= FCacheList.Capacity)and(FCacheList.Count>0) then begin
        ItemFree(0);
        FCacheList.Delete(0);
      end;
      btmcache := TCustomBitmap32.Create;
      btmcache.Assign(AObj);
      FCacheList.AddObject(VKey, btmcache);
    end;
  finally
    FSync.EndWrite;
  end;
end;

procedure TMemTileCacheBitmap.ItemFree(AIndex: Integer);
begin
  FCacheList.Objects[AIndex].Free;
end;

function TMemTileCacheBitmap.TryLoadTileFromCache(AObj: TCustomBitmap32;
  AXY: TPoint; AZoom: Byte): boolean;
var
  i: integer;
  VKey: string;
begin
  Result := false;
  VKey := GetMemCacheKey(AXY, AZoom);
  FSync.BeginRead;
  try
    i := FCacheList.IndexOf(VKey);
    if i >= 0 then begin
      AObj.Assign(TCustomBitmap32(FCacheList.Objects[i]));
      result := true;
    end;
  finally
    FSync.EndRead;
  end;
  if Result then begin
    IncUpdateCounter;
  end;
end;

end.
