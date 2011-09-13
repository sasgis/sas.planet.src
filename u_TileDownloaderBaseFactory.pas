unit u_TileDownloaderBaseFactory;

interface

uses
  i_TileDownloaderConfig,
  i_DownloadResultFactory,
  i_TileDownlodSession,
  i_SimpleFactory;

type
  TTileDownloaderFactoryBase = class(TInterfacedObject, ITileDownlodSessionFactory)
  private
    function CreateSession: ITileDownlodSession; virtual;
  end;

  TTileDownloaderFactory = class(TTileDownloaderFactoryBase, ISimpleFactory)
  private
    FConfig: ITileDownloaderConfig;
    FResultFactory: IDownloadResultFactory;
    function CreateInstance: IInterface;
    function CreateSession: ITileDownlodSession; override;
  public
    constructor Create(
      AResultFactory: IDownloadResultFactory;
      AConfig: ITileDownloaderConfig
    );
  end;

implementation

uses
  u_TileDownloaderBase;

{ TTileDownloaderBaseFactory }

constructor TTileDownloaderFactory.Create(
  AResultFactory: IDownloadResultFactory;
  AConfig: ITileDownloaderConfig
);
begin
  inherited Create;
  FConfig := AConfig;
  FResultFactory := AResultFactory;
end;

function TTileDownloaderFactory.CreateInstance: IInterface;
begin
  Result := CreateSession;
end;

function TTileDownloaderFactory.CreateSession: ITileDownlodSession;
begin
  Result := TTileDownloaderBase.Create(FResultFactory, FConfig);
end;

{ TTileDownloaderFactoryBase }

function TTileDownloaderFactoryBase.CreateSession: ITileDownlodSession;
begin
  Result := nil;
end;

end.
