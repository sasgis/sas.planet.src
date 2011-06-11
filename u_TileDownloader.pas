unit u_TileDownloader;

interface

uses
  SysUtils,
  SyncObjs,
  i_ConfigDataProvider,
  i_RequestBuilderScript,
  i_TileDownloader,
  i_TileDownloaderConfig,
  u_TileDownloaderConfig;

type
  TTileDownloader = class(TInterfacedObject, ITileDownloader)
  protected
    FEnabled: Boolean;
    FMapName: string;
    FZmpFileName: string;
    FMaxConnectToServerCount: Cardinal;
    FRequestBuilderScript: IRequestBuilderScript;
    FTileDownloaderConfig: ITileDownloaderConfig;
    FCS: TCriticalSection;
    procedure Lock;
    procedure UnLock;
    function GetRequestBuilderScript: IRequestBuilderScript;
    function GetTileDownloaderConfig: ITileDownloaderConfig;
    function GetIsEnabled: Boolean;
  public
    constructor Create(AConfig: IConfigDataProvider; AZmpFileName: string);
    destructor Destroy; override;
    procedure Download(AEvent: ITileDownloaderEvent); virtual;
    property RequestBuilderScript: IRequestBuilderScript read GetRequestBuilderScript;
    property TileDownloaderConfig: ITileDownloaderConfig read GetTileDownloaderConfig;
    property Enabled: Boolean read GetIsEnabled;
  end;

const
  DefConnectToServerCount = 32;
  MaxConnectToServerCount = 64;

implementation

uses
  u_GlobalState;

{ TTileDownloader }

constructor TTileDownloader.Create(AConfig: IConfigDataProvider; AZmpFileName: string);
var
  VParams: IConfigDataProvider;
begin
  inherited Create;
  FEnabled := False;
  FTileDownloaderConfig := TTileDownloaderConfig.Create(GState.InetConfig);
  FRequestBuilderScript := nil;
  FZmpFileName := AZmpFileName;
  FCS := TCriticalSection.Create;
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
  FMapName := VParams.ReadString('name', '');
  FMapName := VParams.ReadString('name_'+GState.LanguageManager.GetCurrentLanguageCode, FMapName);
  FMaxConnectToServerCount := VParams.ReadInteger('MaxConnectToServerCount', DefConnectToServerCount);
  if FMaxConnectToServerCount > MaxConnectToServerCount then
    FMaxConnectToServerCount := MaxConnectToServerCount;
end;

destructor TTileDownloader.Destroy;
begin
  FreeAndNil(FCS);
  inherited Destroy;
end;

procedure TTileDownloader.Download(AEvent: ITileDownloaderEvent);
begin
  // virtual
end;

function TTileDownloader.GetRequestBuilderScript: IRequestBuilderScript;
begin
  Lock;
  try
    Result := FRequestBuilderScript;
  finally
    Unlock;
  end;
end;

function TTileDownloader.GetTileDownloaderConfig: ITileDownloaderConfig;
begin
  Lock;
  try
    Result := FTileDownloaderConfig;
  finally
    Unlock;
  end;
end;

function TTileDownloader.GetIsEnabled: Boolean;
begin
  Lock;
  try
    Result := FEnabled;
  finally
    Unlock;
  end;
end;

procedure TTileDownloader.Lock;
begin
  FCS.Acquire;
end;

procedure TTileDownloader.UnLock;
begin
  FCS.Release;
end;

end.
