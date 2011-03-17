unit u_MemFileCache;

interface

uses
  Windows,
  SysUtils,
  Classes,
  GR32,
  i_JclNotify,
  i_IMemObjCache,
  i_IMainMemCacheConfig,
  u_KmlInfoSimple;

type
  TMemFileCache = class(TInterfacedObject, IMemObjCache)
  private
    FConfig: IMainMemCacheConfig;
    FConfigListener: IJclListener;

    FCacheList: TStringList;
    FSync: TMultiReadExclusiveWriteSynchronizer;
    procedure AddToCache(btm: TObject; APath: string);
    procedure OnChangeConfig(Sender: TObject);
  public
    constructor Create(AConfig: IMainMemCacheConfig);
    destructor Destroy; override;
    procedure Clear;
    procedure DeleteFileFromCache(path: string);
    procedure AddTileToCache(btm: TCustomBitmap32; APath: string); overload;
    procedure AddTileToCache(btm: TKmlInfoSimple; APath: string); overload;
    function TryLoadFileFromCache(btm: TCustomBitmap32; APath: string): boolean; overload;
    function TryLoadFileFromCache(btm: TKmlInfoSimple; APath: string): boolean; overload;
  end;

implementation

uses
  u_NotifyEventListener;

{ TMemFileCache }

constructor TMemFileCache.Create(AConfig: IMainMemCacheConfig);
begin
  FConfig := AConfig;
  FConfigListener := TNotifyEventListener.Create(Self.OnChangeConfig);
  FConfig.GetChangeNotifier.Add(FConfigListener);

  FCacheList := TStringList.Create;
  FCacheList.Capacity := FConfig.MaxSize;
  FSync := TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TMemFileCache.Destroy;
begin
  FConfig.GetChangeNotifier.Remove(FConfigListener);
  FConfigListener := nil;
  FConfig := nil;

  Clear;
  FreeAndNil(FSync);
  FreeAndNil(FCacheList);
  inherited;
end;

procedure TMemFileCache.OnChangeConfig(Sender: TObject);
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

procedure TMemFileCache.Clear;
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

procedure TMemFileCache.DeleteFileFromCache(path: string);
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

procedure TMemFileCache.AddToCache(btm: TObject; APath: string);
var
  i: integer;
begin
  FSync.BeginWrite;
  try
    i := FCacheList.IndexOf(APath);
    if (i < 0)and(FCacheList.Capacity>0) then begin
      if (FCacheList.Count >= FCacheList.Capacity)and(FCacheList.Count>0) then begin
        FCacheList.Objects[0].Free;
        FCacheList.Delete(0);
      end;
      FCacheList.AddObject(APath, btm);
    end else begin
      FreeAndNil(btm);
    end;
  finally
    FSync.EndWrite;
  end;
end;


procedure TMemFileCache.AddTileToCache(btm: TCustomBitmap32; APath: string);
var
  btmcache: TCustomBitmap32;
  VPath: string;
begin
  VPath := AnsiUpperCase(APath);
  btmcache := TCustomBitmap32.Create;
  btmcache.Assign(btm);
  AddToCache(btmcache, VPath);
end;

procedure TMemFileCache.AddTileToCache(btm: TKmlInfoSimple; APath: string);
var
  btmcache: TKmlInfoSimple;
  VPath: string;
begin
  VPath := AnsiUpperCase(APath);
  btmcache := TKmlInfoSimple.Create;
  btmcache.Assign(btm);
  AddToCache(btmcache, VPath);
end;

function TMemFileCache.TryLoadFileFromCache(btm: TCustomBitmap32;
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

function TMemFileCache.TryLoadFileFromCache(btm: TKmlInfoSimple;
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
      btm.Assign(TKmlInfoSimple(FCacheList.Objects[i]));
      result := true;
    end;
  finally
    FSync.EndRead;
  end;
end;

end.
