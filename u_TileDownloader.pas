unit u_TileDownloader;

interface

uses
  Windows,
  SysUtils,
  SyncObjs,
  i_ConfigDataProvider,
  i_InetConfig,
  i_TileRequestBuilder,
  i_TileRequestBuilderConfig,
  i_TileDownloader,
  i_TileDownloaderConfig,
  i_ZmpInfo;

type
  TTileDownloader = class(TInterfacedObject, ITileDownloader)
  protected
    FEnabled: Boolean;
    FZmp: IZmpInfo;
    FMaxConnectToServerCount: Cardinal;
    FTileRequestBuilder: ITileRequestBuilder;
    FTileRequestBuilderConfig: ITileRequestBuilderConfig;
    FTileDownloaderConfig: ITileDownloaderConfig;
    FInetConfig: IInetConfig;
    FCS: TCriticalSection;
    procedure Lock;
    procedure UnLock;
    function GetTileRequestBuilderConfig: ITileRequestBuilderConfig;
    function GetTileDownloaderConfig: ITileDownloaderConfig;
    function GetIsEnabled: Boolean;
  public
    constructor Create(
      AConfig: IConfigDataProvider;
      AInetConfig: IInetConfig;
      AZmp: IZmpInfo
    );
    destructor Destroy; override;
    function GetTileUrl(ATileXY: TPoint; AZoom: Byte): string; virtual; abstract;
    procedure Download(AEvent: ITileDownloaderEvent); virtual; abstract;
    property TileRequestBuilderConfig: ITileRequestBuilderConfig read GetTileRequestBuilderConfig;
    property TileDownloaderConfig: ITileDownloaderConfig read GetTileDownloaderConfig;
    property Enabled: Boolean read GetIsEnabled;
  end;

implementation

uses
  u_TileRequestBuilderConfig,
  u_TileDownloaderConfig;

{ TTileDownloader }

constructor TTileDownloader.Create(
  AConfig: IConfigDataProvider;
  AInetConfig: IInetConfig;
  AZmp: IZmpInfo
);
var
  VParams: IConfigDataProvider;
begin
  inherited Create;
  FInetConfig := AInetConfig;
  FZmp := AZmp;
  FEnabled := False;
  FCS := TCriticalSection.Create;
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
  FTileRequestBuilderConfig := TTileRequestBuilderConfig.Create(FZmp.TileRequestBuilderConfig);
  FTileRequestBuilderConfig.ReadConfig(VParams);
  FTileDownloaderConfig := TTileDownloaderConfig.Create(FInetConfig, FZmp.TileDownloaderConfig);
  FTileDownloaderConfig.ReadConfig(VParams);
  FMaxConnectToServerCount := FTileDownloaderConfig.MaxConnectToServerCount;
end;

destructor TTileDownloader.Destroy;
begin
  FreeAndNil(FCS);
  inherited Destroy;
end;

function TTileDownloader.GetTileRequestBuilderConfig: ITileRequestBuilderConfig;
begin
  Lock;
  try
    Result := FTileRequestBuilderConfig;
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
