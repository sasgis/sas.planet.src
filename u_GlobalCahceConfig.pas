unit u_GlobalCahceConfig;

interface

uses
  i_JclNotify;

type
  TGlobalCahceConfig = class
  private
    //Способ храения кеша по-умолчанию.
    FDefCache: byte;

    //Пути к кешам разных типов
    FNewCPath: string;
    FOldCPath: string;
    FESCpath: string;
    FGMTilespath: string;
    FGECachepath: string;

    FCacheChangeNotifier: IJclNotifier;
    procedure SetDefCache(const Value: byte);
    procedure SetESCpath(const Value: string);
    procedure SetGECachepath(const Value: string);
    procedure SetGMTilespath(const Value: string);
    procedure SetNewCPath(const Value: string);
    procedure SetOldCPath(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    //Способ храения кеша по-умолчанию.
    property DefCache: byte read FDefCache write SetDefCache;

    //Пути к кешам разных типов
    property NewCPath: string read FNewCPath write SetNewCPath;
    property OldCPath: string read FOldCPath write SetOldCPath;
    property ESCpath: string read FESCpath write SetESCpath;
    property GMTilespath: string read FGMTilespath write SetGMTilespath;
    property GECachepath: string read FGECachepath write SetGECachepath;

    property CacheChangeNotifier: IJclNotifier read FCacheChangeNotifier;
  end;

implementation

uses
  u_JclNotify;

{ TGlobalCahceConfig }

constructor TGlobalCahceConfig.Create;
begin
  FDefCache := 2;
  FCacheChangeNotifier := TJclBaseNotifier.Create;
end;

destructor TGlobalCahceConfig.Destroy;
begin
  FCacheChangeNotifier := nil;
  inherited;
end;

procedure TGlobalCahceConfig.SetDefCache(const Value: byte);
begin
  if FDefCache <> Value then begin
    FDefCache := Value;
    FCacheChangeNotifier.Notify(nil);
  end;
end;

procedure TGlobalCahceConfig.SetESCpath(const Value: string);
begin
  if FESCpath <> Value then begin
    FESCpath := Value;
    FCacheChangeNotifier.Notify(nil);
  end;
end;

procedure TGlobalCahceConfig.SetGECachepath(const Value: string);
begin
  if FGECachepath <> Value then begin
    FGECachepath := Value;
    FCacheChangeNotifier.Notify(nil);
  end;
end;

procedure TGlobalCahceConfig.SetGMTilespath(const Value: string);
begin
  if FGMTilespath <> Value then begin
    FGMTilespath := Value;
    FCacheChangeNotifier.Notify(nil);
  end;
end;

procedure TGlobalCahceConfig.SetNewCPath(const Value: string);
begin
  if FNewCPath <> Value then begin
    FNewCPath := Value;
    FCacheChangeNotifier.Notify(nil);
  end;
end;

procedure TGlobalCahceConfig.SetOldCPath(const Value: string);
begin
  if FOldCPath <> Value then begin
    FOldCPath := Value;
    FCacheChangeNotifier.Notify(nil);
  end;
end;

end.
