unit u_MapTypeCacheConfig;

interface

uses
  Types,
  i_JclNotify,
  i_IConfigDataProvider,
  i_ITileFileNameGenerator;

type
  TMapTypeCacheConfig = class
  private
    FTileFileExt: string;

    FDefCachetype: byte;
    FCacheType: byte;
    FEffectiveCacheType: Byte;

    FDefNameInCache: string;
    FNameInCache: string;

    FBasePath: String;
    FFileNameGenerator: ITileFileNameGenerator;
    FGlobalSettingsListener: IJclListener;
    procedure OnSettingsEdit;
    procedure SetCacheType(const Value: byte);
    procedure SetNameInCache(const Value: string);
  public
    constructor Create(AConfig: IConfigDataProvider);
    destructor Destroy; override;

    function GetTileFileName(AXY: TPoint; Azoom: byte): string;

    property DefCachetype: byte read FDefCachetype;
    property CacheType: byte read FCacheType write SetCacheType;

    property EffectiveCacheType: byte read FEffectiveCacheType;

    property DefNameInCache: string read FDefNameInCache;
    property NameInCache: string read FNameInCache write SetNameInCache;

    property BasePath: string read FBasePath;
  end;

implementation

uses
  SysUtils,
  u_JclNotify,
  u_GlobalState;

{ TListenerOfTMapCacheConfig }

type
  TListenerOfTMapCacheConfig = class(TJclBaseListener)
  protected
    FConfig: TMapTypeCacheConfig;
  public
    constructor Create(AConfig: TMapTypeCacheConfig);
    procedure Notification(msg: IJclNotificationMessage); override;
  end;

constructor TListenerOfTMapCacheConfig.Create(AConfig: TMapTypeCacheConfig);
begin
  FConfig := AConfig;
end;

procedure TListenerOfTMapCacheConfig.Notification(
  msg: IJclNotificationMessage);
begin
  inherited;
  FConfig.OnSettingsEdit;
end;

{ TMapTypeCacheConfig }

constructor TMapTypeCacheConfig.Create(AConfig: IConfigDataProvider);
var
  VParams: IConfigDataProvider;
begin
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');

  FGlobalSettingsListener := TListenerOfTMapCacheConfig.Create(Self);
  GState.CacheConfig.CacheChangeNotifier.Add(FGlobalSettingsListener);

  FTileFileExt:=LowerCase(VParams.ReadString('Ext','.jpg'));
  FCacheType:=VParams.ReadInteger('CacheType',0);
  FDefCacheType:=FCacheType;
  FNameInCache:=VParams.ReadString('NameInCache','Sat');
  FDefNameInCache:=FNameInCache;
end;

destructor TMapTypeCacheConfig.Destroy;
begin
  GState.CacheConfig.CacheChangeNotifier.Remove(FGlobalSettingsListener);
  FGlobalSettingsListener := nil;
  inherited;
end;

function TMapTypeCacheConfig.GetTileFileName(AXY: TPoint; Azoom: byte): string;
begin
  Result := FBasePath + FFileNameGenerator.GetTileFileName(AXY, Azoom) + FTileFileExt;
end;

procedure TMapTypeCacheConfig.OnSettingsEdit;
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
  if (length(VBasePath)<2)or((VBasePath[2]<>'\')and(system.pos(':',VBasePath)=0)) then begin
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
  if (length(VBasePath)<2)or((VBasePath[2]<>'\')and(system.pos(':',VBasePath)=0))then begin
    VBasePath:=IncludeTrailingPathDelimiter(GState.ProgramPath)+VBasePath;
  end;
  VBasePath := IncludeTrailingPathDelimiter(VBasePath);
  FBasePath := VBasePath;
end;

procedure TMapTypeCacheConfig.SetCacheType(const Value: byte);
begin
  if FCacheType <> Value then begin
    FCacheType := Value;
    OnSettingsEdit;
  end;
end;

procedure TMapTypeCacheConfig.SetNameInCache(const Value: string);
begin
  if FNameInCache <> Value then begin
    FNameInCache := Value;
    OnSettingsEdit;
  end;
end;

end.
