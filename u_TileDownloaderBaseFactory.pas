unit u_TileDownloaderBaseFactory;

interface

uses
  i_ConfigDataProvider,
  i_TileDownloaderConfig,
  i_TileDownlodSession,
  i_SimpleFactory;

type
  TTileDownloaderFactoryBase = class(TInterfacedObject, ITileDownlodSessionFactory)
  private
    function CreateSession: ITileDownlodSession; virtual;
  public
    constructor Create(AConfigData: IConfigDataProvider);
  end;

  TTileDownloaderFactory = class(TTileDownloaderFactoryBase, ISimpleFactory)
  private
    FConfig: ITileDownloaderConfig;
    function CreateInstance: IInterface;
    function CreateSession: ITileDownlodSession; override;
  public
    constructor Create(AConfigData: IConfigDataProvider; AConfig: ITileDownloaderConfig);
  end;

implementation

uses
  u_GlobalState,
  u_TileDownloaderBase;

{ TTileDownloaderBaseFactory }

constructor TTileDownloaderFactory.Create(AConfigData: IConfigDataProvider; AConfig: ITileDownloaderConfig);
var
  VParams: IConfigDataProvider;
begin
  inherited Create(AConfigData);
  FConfig := AConfig;
end;

function TTileDownloaderFactory.CreateInstance: IInterface;
begin
  Result := CreateSession;
end;

function TTileDownloaderFactory.CreateSession: ITileDownlodSession;
var
  VDownloader: TTileDownloaderBase;
begin
  VDownloader := TTileDownloaderBase.Create(FConfig, FIgnoreContent_Type,
    FContent_Type, FDefaultContent_Type, GState.InetConfig);
  Result := VDownloader;
end;

{ TTileDownloaderFactoryBase }

constructor TTileDownloaderFactoryBase.Create(AConfigData: IConfigDataProvider);
begin
end;

function TTileDownloaderFactoryBase.CreateSession: ITileDownlodSession;
begin
  Result := nil;
end;

end.
