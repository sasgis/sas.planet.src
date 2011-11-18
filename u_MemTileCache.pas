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
  i_TTLCheckListener,
  i_TTLCheckNotifier,
  i_TileObjCache,
  i_CoordConverter,
  u_TileStorageAbstract;

type
  TMemTileCacheBase = class(TInterfacedObject)
  private
    FConfig: IMainMemCacheConfig;
    FGCList: ITTLCheckNotifier;
    FConfigListener: IJclListener;
    FTileStorage: TTileStorageAbstract;
    FCoordConverter: ICoordConverter;
    FStorageChangeListener: IJclListener;
    FTTLListener: ITTLCheckListener;

    FCacheList: TStringList;
    FSync: TMultiReadExclusiveWriteSynchronizer;
    procedure OnChangeConfig(Sender: TObject);
    procedure OnTileStorageChange(Sender: TObject);
    function GetMemCacheKey(AXY: TPoint; Azoom: byte): string;
    procedure OnTTLTrim(Sender: TObject);
  protected
    procedure ItemFree(AIndex: Integer); virtual; abstract;
    procedure IncUpdateCounter;
  protected
    procedure Clear;
    procedure DeleteTileFromCache(AXY: TPoint; AZoom: Byte);
  public
    constructor Create(
      AGCList: ITTLCheckNotifier;
      ATileStorage: TTileStorageAbstract;
      ACoordConverter: ICoordConverter;
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
  i_TileRectUpdateNotifier,
  u_TTLCheckListener,
  u_NotifyEventListener;

{ TTileCacheBase }

constructor TMemTileCacheBase.Create(
  AGCList: ITTLCheckNotifier;
  ATileStorage: TTileStorageAbstract;
  ACoordConverter: ICoordConverter;
  AConfig: IMainMemCacheConfig
);
var
  i: Integer;
  VNotifier: ITileRectUpdateNotifier;
begin
  FConfig := AConfig;
  FGCList := AGCList;
  FConfigListener := TNotifyEventListener.Create(Self.OnChangeConfig);
  FConfig.GetChangeNotifier.Add(FConfigListener);

  if ATileStorage <> nil then begin
    FTileStorage := ATileStorage;
    FCoordConverter := ACoordConverter;
    FStorageChangeListener := TNotifyEventListener.Create(Self.OnTileStorageChange);
    for i := FCoordConverter.MinZoom to FCoordConverter.MaxZoom do begin
      VNotifier := FTileStorage.NotifierByZoom[i];
      if VNotifier <> nil then begin
        VNotifier.Add(FStorageChangeListener, FCoordConverter.TileRectAtZoom(i));
      end;
    end;
  end;

  FCacheList := TStringList.Create;
  FCacheList.Capacity := FConfig.MaxSize;
  FSync := TMultiReadExclusiveWriteSynchronizer.Create;
  FTTLListener := TTTLCheckListener.Create(Self.OnTTLTrim, 40000, 1000);
  FGCList.Add(FTTLListener);
end;

destructor TMemTileCacheBase.Destroy;
var
  i: Integer;
  VNotifier: ITileRectUpdateNotifier;
begin
  FConfig.GetChangeNotifier.Remove(FConfigListener);
  FConfigListener := nil;
  FConfig := nil;

  FGCList.Remove(FTTLListener);
  FTTLListener := nil;
  FGCList := nil;

  for i := FCoordConverter.MinZoom to FCoordConverter.MaxZoom do begin
    VNotifier := FTileStorage.NotifierByZoom[i];
    if VNotifier <> nil then begin
      VNotifier.Remove(FStorageChangeListener);
    end;
  end;
  FStorageChangeListener := nil;
  FCoordConverter := nil;
  FTileStorage := nil;

  Clear;
  FreeAndNil(FSync);
  FreeAndNil(FCacheList);
  inherited;
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
  FTTLListener.UpdateUseTime;
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

procedure TMemTileCacheBase.OnTileStorageChange(Sender: TObject);
begin
  Clear;
end;

procedure TMemTileCacheBase.OnTTLTrim(Sender: TObject);
begin
  Clear;
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
