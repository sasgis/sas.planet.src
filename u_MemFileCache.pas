unit u_MemFileCache;

interface

uses
  Windows,
  SysUtils,
  Classes,
  GR32,
  i_JclNotify,
  i_InternalPerformanceCounter,
  i_MemObjCache,
  i_MainMemCacheConfig,
  i_VectorDataItemSimple;

type
  TMemFileCacheVector = class(TInterfacedObject, IMemObjCacheVector)
  private
    FConfig: IMainMemCacheConfig;
    FConfigListener: IJclListener;
    FCacheList: TStringList;
    FSync: TMultiReadExclusiveWriteSynchronizer;
    procedure OnChangeConfig(Sender: TObject);
  public
    constructor Create(AConfig: IMainMemCacheConfig);
    destructor Destroy; override;
    procedure Clear;
    procedure DeleteFileFromCache(path: string);
    procedure AddTileToCache(btm: IVectorDataItemList; APath: string);
    function TryLoadFileFromCache(var btm: IVectorDataItemList; APath: string): boolean;
  end;

  TMemFileCacheBitmap = class(TInterfacedObject, IMemObjCacheBitmap)
  private
    FConfig: IMainMemCacheConfig;
    FConfigListener: IJclListener;

    FCacheList: TStringList;
    FSync: TMultiReadExclusiveWriteSynchronizer;
    procedure OnChangeConfig(Sender: TObject);
  public
    constructor Create(AConfig: IMainMemCacheConfig);
    destructor Destroy; override;
    procedure Clear;
    procedure DeleteFileFromCache(path: string);
    procedure AddTileToCache(btm: TCustomBitmap32; APath: string); overload;
    function TryLoadFileFromCache(btm: TCustomBitmap32; APath: string): boolean; overload;
  end;


implementation

uses
  u_NotifyEventListener;

{ TMemFileCache }

constructor TMemFileCacheVector.Create(AConfig: IMainMemCacheConfig);
begin
  FConfig := AConfig;
  FConfigListener := TNotifyEventListener.Create(Self.OnChangeConfig);
  FConfig.GetChangeNotifier.Add(FConfigListener);

  FCacheList := TStringList.Create;
  FCacheList.Capacity := FConfig.MaxSize;
  FSync := TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TMemFileCacheVector.Destroy;
begin
  FConfig.GetChangeNotifier.Remove(FConfigListener);
  FConfigListener := nil;
  FConfig := nil;

  Clear;
  FreeAndNil(FSync);
  FreeAndNil(FCacheList);
  inherited;
end;

procedure TMemFileCacheVector.OnChangeConfig(Sender: TObject);
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
          IInterface(Pointer(FCacheList.Objects[0]))._Release;
          FCacheList.Delete(0);
        end;
      end;
      FCacheList.Capacity := VNewSize;
    end;
  finally
    FSync.EndWrite;
  end;
end;

procedure TMemFileCacheVector.Clear;
var
  i: integer;
begin
  FSync.BeginWrite;
  try
    for i := 0 to FCacheList.Count - 1 do begin
      IInterface(Pointer(FCacheList.Objects[i]))._Release;
    end;
    FCacheList.Clear;
  finally
    FSync.EndWrite;
  end;
end;

procedure TMemFileCacheVector.DeleteFileFromCache(path: string);
var
  i: Integer;
begin
  FSync.BeginWrite;
  try
    i := FCacheList.IndexOf(AnsiUpperCase(Path));
    if i >= 0 then begin
      IInterface(Pointer(FCacheList.Objects[i]))._Release;
      FCacheList.Delete(i);
    end;
  finally
    FSync.EndWrite;
  end;
end;

procedure TMemFileCacheVector.AddTileToCache(btm: IVectorDataItemList; APath: string);
var
  VPath: string;
  i: integer;
begin
  VPath := AnsiUpperCase(APath);
  FSync.BeginWrite;
  try
    i := FCacheList.IndexOf(APath);
    if (i < 0)and(FCacheList.Capacity>0) then begin
      if (FCacheList.Count >= FCacheList.Capacity)and(FCacheList.Count>0) then begin
        IInterface(Pointer(FCacheList.Objects[0]))._Release;
        FCacheList.Delete(0);
      end;
      btm._AddRef;
      FCacheList.AddObject(APath, Pointer(btm));
    end;
  finally
    FSync.EndWrite;
  end;
end;

function TMemFileCacheVector.TryLoadFileFromCache(var btm: IVectorDataItemList;
  APath: string): boolean;
var
  i: integer;
  VPath: string;
begin
  Result := false;
  VPath := AnsiUpperCase(APath);
  FSync.BeginRead;
  try
    i := FCacheList.IndexOf(VPath);
    if i >= 0 then begin
      btm := IVectorDataItemList(Pointer(FCacheList.Objects[i]));
      result := true;
    end;
  finally
    FSync.EndRead;
  end;
end;

{ TMemFileCacheBitmap }

constructor TMemFileCacheBitmap.Create(AConfig: IMainMemCacheConfig);
begin
  FConfig := AConfig;
  FConfigListener := TNotifyEventListener.Create(Self.OnChangeConfig);
  FConfig.GetChangeNotifier.Add(FConfigListener);

  FCacheList := TStringList.Create;
  FCacheList.Capacity := FConfig.MaxSize;
  FSync := TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TMemFileCacheBitmap.Destroy;
begin
  FConfig.GetChangeNotifier.Remove(FConfigListener);
  FConfigListener := nil;
  FConfig := nil;

  Clear;
  FreeAndNil(FSync);
  FreeAndNil(FCacheList);
  inherited;
end;

procedure TMemFileCacheBitmap.AddTileToCache(btm: TCustomBitmap32;
  APath: string);
var
  btmcache: TCustomBitmap32;
  VPath: string;
  i: integer;
begin
  VPath := AnsiUpperCase(APath);
  FSync.BeginWrite;
  try
    i := FCacheList.IndexOf(APath);
    if (i < 0)and(FCacheList.Capacity>0) then begin
      if (FCacheList.Count >= FCacheList.Capacity)and(FCacheList.Count>0) then begin
        FCacheList.Objects[0].Free;
        FCacheList.Delete(0);
      end;
      btmcache := TCustomBitmap32.Create;
      btmcache.Assign(btm);
      FCacheList.AddObject(APath, btmcache);
    end;
  finally
    FSync.EndWrite;
  end;
end;

procedure TMemFileCacheBitmap.Clear;
var
  i: integer;
begin
  FSync.BeginWrite;
  try
    for i := 0 to FCacheList.Count - 1 do begin
      FCacheList.Objects[i].Free;
    end;
    FCacheList.Clear;
  finally
    FSync.EndWrite;
  end;
end;

procedure TMemFileCacheBitmap.DeleteFileFromCache(path: string);
var
  i: Integer;
begin
  FSync.BeginWrite;
  try
    i := FCacheList.IndexOf(AnsiUpperCase(Path));
    if i >= 0 then begin
      FCacheList.Objects[i].Free;
      FCacheList.Delete(i);
    end;
  finally
    FSync.EndWrite;
  end;
end;

procedure TMemFileCacheBitmap.OnChangeConfig(Sender: TObject);
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
          FCacheList.Objects[0].Free;
          FCacheList.Delete(0);
        end;
      end;
      FCacheList.Capacity := VNewSize;
    end;
  finally
    FSync.EndWrite;
  end;
end;

function TMemFileCacheBitmap.TryLoadFileFromCache(btm: TCustomBitmap32;
  APath: string): boolean;
var
  i: integer;
  VPath: string;
begin
  Result := false;
  VPath := AnsiUpperCase(APath);
  FSync.BeginRead;
  try
    i := FCacheList.IndexOf(VPath);
    if i >= 0 then begin
      btm.Assign(TCustomBitmap32(FCacheList.Objects[i]));
      result := true;
    end;
  finally
    FSync.EndRead;
  end;
end;

end.
