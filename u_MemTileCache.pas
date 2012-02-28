unit u_MemTileCache;

interface

uses
  Types,
  Classes,
  SysUtils,
  i_JclNotify,
  i_MainMemCacheConfig,
  i_Bitmap32Static,
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
    procedure OnChangeConfig;
    procedure OnTileStorageChange;
    function GetMemCacheKey(AXY: TPoint; Azoom: byte): string;
    procedure OnTTLTrim(Sender: TObject);
  protected
    procedure ItemFree(AIndex: Integer);
    procedure IncUpdateCounter;
  protected
    procedure Clear;
    procedure DeleteTileFromCache(AXY: TPoint; AZoom: Byte);
    procedure AddObjectToCache(AObj: IInterface; AXY: TPoint; AZoom: Byte);
    function TryLoadObjectFromCache(AXY: TPoint; AZoom: Byte): IInterface;
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
    procedure AddTileToCache(AObj: IVectorDataItemList; AXY: TPoint; AZoom: Byte);
    function TryLoadTileFromCache(AXY: TPoint; AZoom: Byte): IVectorDataItemList;
  end;

  TMemTileCacheBitmap = class(TMemTileCacheBase, ITileObjCacheBitmap)
  protected
    procedure AddTileToCache(AObj: IBitmap32Static; AXY: TPoint; AZoom: Byte);
    function TryLoadTileFromCache(AXY: TPoint; AZoom: Byte): IBitmap32Static;
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
  FConfigListener := TNotifyNoMmgEventListener.Create(Self.OnChangeConfig);
  FConfig.GetChangeNotifier.Add(FConfigListener);

  if ATileStorage <> nil then begin
    FTileStorage := ATileStorage;
    FCoordConverter := ACoordConverter;
    FStorageChangeListener := TNotifyNoMmgEventListener.Create(Self.OnTileStorageChange);
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

procedure TMemTileCacheBase.AddObjectToCache(
  AObj: IInterface;
  AXY: TPoint;
  AZoom: Byte
);
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

procedure TMemTileCacheBase.ItemFree(AIndex: Integer);
begin
  IInterface(Pointer(FCacheList.Objects[AIndex]))._Release;
end;

procedure TMemTileCacheBase.OnChangeConfig;
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

procedure TMemTileCacheBase.OnTileStorageChange;
begin
  Clear;
end;

procedure TMemTileCacheBase.OnTTLTrim(Sender: TObject);
begin
  Clear;
end;

function TMemTileCacheBase.TryLoadObjectFromCache(
  AXY: TPoint;
  AZoom: Byte
): IInterface;
var
  i: integer;
  VKey: string;
begin
  Result := nil;
  VKey := GetMemCacheKey(AXY, AZoom);
  FSync.BeginRead;
  try
    i := FCacheList.IndexOf(VKey);
    if i >= 0 then begin
      Result := IVectorDataItemList(Pointer(FCacheList.Objects[i]));
    end;
  finally
    FSync.EndRead;
  end;
  if Result <> nil then begin
    IncUpdateCounter;
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

function TMemTileCacheVector.TryLoadTileFromCache(
  AXY: TPoint;
  AZoom: Byte
): IVectorDataItemList;
var
  VObj: IInterface;
begin
  VObj := TryLoadObjectFromCache(AXY, AZoom);
  if not Supports(VObj, IVectorDataItemList, Result) then begin
    Result := nil;
  end;
end;

{ TMemTileCacheBitmap }

procedure TMemTileCacheBitmap.AddTileToCache(
  AObj: IBitmap32Static;
  AXY: TPoint;
  AZoom: Byte
);
begin
  AddObjectToCache(AObj, AXY, AZoom);
end;

function TMemTileCacheBitmap.TryLoadTileFromCache(
  AXY: TPoint;
  AZoom: Byte
): IBitmap32Static;
var
  VObj: IInterface;
begin
  VObj := TryLoadObjectFromCache(AXY, AZoom);
  if not Supports(VObj, IBitmap32Static, Result) then begin
    Result := nil;
  end;
end;

end.
