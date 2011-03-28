unit u_MapTypeCacheConfig;

interface

uses
  Types,
  i_JclNotify,
  i_ConfigDataProvider,
  i_TileFileNameGenerator;

type
  TMapTypeCacheConfigAbstract = class
  protected
    FTileFileExt: string;

    FDefCachetype: byte;
    FCacheType: byte;
    FEffectiveCacheType: Byte;

    FDefNameInCache: string;
    FNameInCache: string;

    FBasePath: String;
    FFileNameGenerator: ITileFileNameGenerator;

    FConfigChangeNotifier: IJclNotifier;

    procedure SetCacheType(const Value: byte); virtual; abstract;
    procedure SetNameInCache(const Value: string); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function GetTileFileName(AXY: TPoint; Azoom: byte): string;

    property DefCachetype: byte read FDefCachetype;
    property CacheType: byte read FCacheType write SetCacheType;

    property EffectiveCacheType: byte read FEffectiveCacheType;

    property DefNameInCache: string read FDefNameInCache;
    property NameInCache: string read FNameInCache write SetNameInCache;

    property BasePath: string read FBasePath;
    property ConfigChangeNotifier: IJclNotifier read FConfigChangeNotifier;
  end;

  TMapTypeCacheConfig = class(TMapTypeCacheConfigAbstract)
  private
    FGlobalSettingsListener: IJclListener;
    procedure OnSettingsEdit(Sender: TObject);
  protected
    procedure SetCacheType(const Value: byte); override;
    procedure SetNameInCache(const Value: string); override;
  public
    constructor Create(AConfig: IConfigDataProvider);
    destructor Destroy; override;
  end;

  TMapTypeCacheConfigGE = class(TMapTypeCacheConfigAbstract)
  protected
    FGlobalSettingsListener: IJclListener;
    procedure OnSettingsEdit(Sender: TObject);
    procedure SetCacheType(const Value: byte); override;
    procedure SetNameInCache(const Value: string); override;
  public
    constructor Create(AConfig: IConfigDataProvider);
    function GetIndexFileName: string;
    function GetDataFileName: string;
  end;


implementation

uses
  SysUtils,
  u_JclNotify,
  u_NotifyEventListener,
  u_GlobalState;

{ TMapTypeCacheConfigAbstract }

constructor TMapTypeCacheConfigAbstract.Create;
begin
  FConfigChangeNotifier := TJclBaseNotifier.Create;
end;

destructor TMapTypeCacheConfigAbstract.Destroy;
begin
  FConfigChangeNotifier := nil;
  inherited;
end;

function TMapTypeCacheConfigAbstract.GetTileFileName(AXY: TPoint; Azoom: byte): string;
begin
  Result := FBasePath + FFileNameGenerator.GetTileFileName(AXY, Azoom) + FTileFileExt;
end;

procedure TMapTypeCacheConfigAbstract.SetNameInCache(const Value: string);
begin
  if FNameInCache <> Value then begin
    FNameInCache := Value;
  end;
end;

{ TMapTypeCacheConfig }

constructor TMapTypeCacheConfig.Create(AConfig: IConfigDataProvider);
var
  VParams: IConfigDataProvider;
begin
  inherited Create;
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');

  FGlobalSettingsListener := TNotifyEventListener.Create(Self.OnSettingsEdit);
  GState.CacheConfig.CacheChangeNotifier.Add(FGlobalSettingsListener);

  FTileFileExt := LowerCase(VParams.ReadString('Ext', '.jpg'));
  FCacheType := VParams.ReadInteger('CacheType', 0);
  FDefCacheType := FCacheType;
  FNameInCache := VParams.ReadString('NameInCache', 'Sat');
  FDefNameInCache := FNameInCache;
  OnSettingsEdit(nil);
end;

destructor TMapTypeCacheConfig.Destroy;
begin
  GState.CacheConfig.CacheChangeNotifier.Remove(FGlobalSettingsListener);
  FGlobalSettingsListener := nil;
  inherited;
end;

procedure TMapTypeCacheConfig.OnSettingsEdit(Sender: TObject);
var
  VCacheType: Byte;
  VBasePath: string;
begin
  VCacheType := FCacheType;
  if VCacheType = 0 then begin
    VCacheType := GState.CacheConfig.DefCache;
  end;
  FEffectiveCacheType := VCacheType;
  FFileNameGenerator := GState.TileNameGenerator.GetGenerator(FEffectiveCacheType);

  VBasePath := FNameInCache;
  //TODO: — этим бардаком нужно что-то будет сделать
  if (length(VBasePath) < 2) or ((VBasePath[2] <> '\') and (system.pos(':', VBasePath) = 0)) then begin
    case FEffectiveCacheType of
      1: begin
        VBasePath:=IncludeTrailingPathDelimiter(GState.CacheConfig.OldCpath) + VBasePath;
      end;
      2: begin
        VBasePath:=IncludeTrailingPathDelimiter(GState.CacheConfig.NewCpath)+VBasePath;
      end;
      3: begin
        VBasePath:=IncludeTrailingPathDelimiter(GState.CacheConfig.ESCpath)+VBasePath;
      end;
      4,41: begin
        VBasePath:=IncludeTrailingPathDelimiter(GState.CacheConfig.GMTilespath)+VBasePath;
      end;
      5: begin
        VBasePath:=IncludeTrailingPathDelimiter(GState.CacheConfig.GECachepath)+VBasePath;
      end;
    end;
  end;
  //TODO: — этим бардаком нужно что-то будет сделать
  if (length(VBasePath) < 2) or ((VBasePath[2] <> '\') and (system.pos(':', VBasePath) = 0)) then begin
    VBasePath := IncludeTrailingPathDelimiter(GState.ProgramPath) + VBasePath;
  end;
  VBasePath := IncludeTrailingPathDelimiter(VBasePath);
  FBasePath := VBasePath;
end;

procedure TMapTypeCacheConfig.SetCacheType(const Value: byte);
begin
  if FCacheType <> Value then begin
    FCacheType := Value;
    OnSettingsEdit(nil);
  end;
end;

procedure TMapTypeCacheConfig.SetNameInCache(const Value: string);
begin
  if FNameInCache <> Value then begin
    FNameInCache := Value;
    OnSettingsEdit(nil);
  end;
end;

{ TMapTypeCacheConfigGE }

constructor TMapTypeCacheConfigGE.Create(AConfig: IConfigDataProvider);
var
  VParams: IConfigDataProvider;
begin
  inherited Create;
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
  FTileFileExt := '';
  FCacheType := 5;
  FEffectiveCacheType := 5;
  FDefCacheType := FCacheType;
  FNameInCache := VParams.ReadString('NameInCache', '');
  FDefNameInCache := FNameInCache;
  FGlobalSettingsListener := TNotifyEventListener.Create(Self.OnSettingsEdit);
  GState.CacheConfig.CacheChangeNotifier.Add(FGlobalSettingsListener);
end;

procedure TMapTypeCacheConfigGE.OnSettingsEdit(Sender: TObject);
var
  VBasePath: string;
begin
  VBasePath := FNameInCache;
  //TODO: — этим бардаком нужно что-то будет сделать
  if (length(VBasePath) < 2) or ((VBasePath[2] <> '\') and (system.pos(':', VBasePath) = 0)) then begin
    VBasePath:=IncludeTrailingPathDelimiter(GState.CacheConfig.GECachepath)+VBasePath;
  end;
  //TODO: — этим бардаком нужно что-то будет сделать
  if (length(VBasePath) < 2) or ((VBasePath[2] <> '\') and (system.pos(':', VBasePath) = 0)) then begin
    VBasePath := IncludeTrailingPathDelimiter(GState.ProgramPath) + VBasePath;
  end;
  VBasePath := IncludeTrailingPathDelimiter(VBasePath);
  FBasePath := VBasePath;
end;

procedure TMapTypeCacheConfigGE.SetCacheType(const Value: byte);
begin
end;

procedure TMapTypeCacheConfigGE.SetNameInCache(const Value: string);
begin
  if FNameInCache <> Value then begin
    FNameInCache := Value;
    OnSettingsEdit(nil);
  end;
end;
 
function TMapTypeCacheConfigGE.GetDataFileName: string;
begin
  Result := FBasePath + 'dbCache.dat';
end;

function TMapTypeCacheConfigGE.GetIndexFileName: string;
begin
  Result := FBasePath + 'dbCache.dat.index';
end;

end.
