unit u_MapTypeCacheConfig;

interface

uses
  Types,
  i_JclNotify,
  i_ConfigDataProvider,
  u_GlobalCahceConfig,
  i_TileFileNameGenerator;

type
  TMapTypeCacheConfigAbstract = class
  private
    FGlobalCacheConfig: TGlobalCahceConfig;
    FGlobalSettingsListener: IJclListener;
    procedure OnSettingsEdit(Sender: TObject); virtual; abstract;
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
    constructor Create(AGlobalCacheConfig: TGlobalCahceConfig);
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
    procedure OnSettingsEdit(Sender: TObject); override;
  protected
    procedure SetCacheType(const Value: byte); override;
    procedure SetNameInCache(const Value: string); override;
  public
    constructor Create(AGlobalCacheConfig: TGlobalCahceConfig; AConfig: IConfigDataProvider);
  end;

  TMapTypeCacheConfigGE = class(TMapTypeCacheConfigAbstract)
  protected
    procedure OnSettingsEdit(Sender: TObject); override;
    procedure SetCacheType(const Value: byte); override;
    procedure SetNameInCache(const Value: string); override;
  public
    constructor Create(AGlobalCacheConfig: TGlobalCahceConfig; AConfig: IConfigDataProvider);
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

constructor TMapTypeCacheConfigAbstract.Create(AGlobalCacheConfig: TGlobalCahceConfig);
begin
  FGlobalCacheConfig := AGlobalCacheConfig;
  FConfigChangeNotifier := TJclBaseNotifier.Create;

  FGlobalSettingsListener := TNotifyEventListener.Create(Self.OnSettingsEdit);
  FGlobalCacheConfig.CacheChangeNotifier.Add(FGlobalSettingsListener);
end;

destructor TMapTypeCacheConfigAbstract.Destroy;
begin
  FGlobalCacheConfig.CacheChangeNotifier.Remove(FGlobalSettingsListener);
  FGlobalSettingsListener := nil;

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

constructor TMapTypeCacheConfig.Create(AGlobalCacheConfig: TGlobalCahceConfig; AConfig: IConfigDataProvider);
var
  VParams: IConfigDataProvider;
begin
  inherited Create(AGlobalCacheConfig);
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');

  FTileFileExt := LowerCase(VParams.ReadString('Ext', '.jpg'));
  FCacheType := VParams.ReadInteger('CacheType', 0);
  FDefCacheType := VParams.ReadInteger('MAIN:CacheType', 0);
  FNameInCache := VParams.ReadString('NameInCache', 'Sat');
  FDefNameInCache := VParams.ReadString('MAIN:NameInCache', 'Sat');;
  OnSettingsEdit(nil);
end;

procedure TMapTypeCacheConfig.OnSettingsEdit(Sender: TObject);
var
  VCacheType: Byte;
  VBasePath: string;
begin
  VCacheType := FCacheType;
  if VCacheType = 0 then begin
    VCacheType := FGlobalCacheConfig.DefCache;
  end;
  FEffectiveCacheType := VCacheType;
  FFileNameGenerator := GState.TileNameGenerator.GetGenerator(FEffectiveCacheType);

  VBasePath := FNameInCache;
  //TODO: — этим бардаком нужно что-то будет сделать
  if (length(VBasePath) < 2) or ((VBasePath[2] <> '\') and (system.pos(':', VBasePath) = 0)) then begin
    case FEffectiveCacheType of
      1: begin
        VBasePath:=IncludeTrailingPathDelimiter(FGlobalCacheConfig.OldCpath) + VBasePath;
      end;
      2: begin
        VBasePath:=IncludeTrailingPathDelimiter(FGlobalCacheConfig.NewCpath)+VBasePath;
      end;
      3: begin
        VBasePath:=IncludeTrailingPathDelimiter(FGlobalCacheConfig.ESCpath)+VBasePath;
      end;
      4,41: begin
        VBasePath:=IncludeTrailingPathDelimiter(FGlobalCacheConfig.GMTilespath)+VBasePath;
      end;
      5: begin
        VBasePath:=IncludeTrailingPathDelimiter(FGlobalCacheConfig.GECachepath)+VBasePath;
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

constructor TMapTypeCacheConfigGE.Create(AGlobalCacheConfig: TGlobalCahceConfig; AConfig: IConfigDataProvider);
var
  VParams: IConfigDataProvider;
begin
  inherited Create(AGlobalCacheConfig);
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
  FTileFileExt := '';
  FCacheType := 5;
  FEffectiveCacheType := 5;
  FDefCacheType := FCacheType;
  FNameInCache := VParams.ReadString('NameInCache', '');
  FDefNameInCache := FNameInCache;
  OnSettingsEdit(nil);
end;

procedure TMapTypeCacheConfigGE.OnSettingsEdit(Sender: TObject);
var
  VBasePath: string;
begin
  VBasePath := FNameInCache;
  //TODO: — этим бардаком нужно что-то будет сделать
  if (length(VBasePath) < 2) or ((VBasePath[2] <> '\') and (system.pos(':', VBasePath) = 0)) then begin
    VBasePath:=IncludeTrailingPathDelimiter(FGlobalCacheConfig.GECachepath)+VBasePath;
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
