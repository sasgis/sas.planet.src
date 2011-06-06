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
  end;

  TTileDownloaderFactory = class(TTileDownloaderFactoryBase, ISimpleFactory)
  private
    FConfig: ITileDownloaderConfig;
    function CreateInstance: IInterface;
    function CreateSession: ITileDownlodSession; override;
  public
    constructor Create(AConfig: ITileDownloaderConfig);
  end;

implementation

uses
  u_GlobalState,
  u_TileDownloaderBase;

{ TTileDownloaderBaseFactory }

constructor TTileDownloaderFactory.Create(AConfig: ITileDownloaderConfig);
begin
  inherited Create;
  FConfig := AConfig;
end;

function TTileDownloaderFactory.CreateInstance: IInterface;
begin
  Result := CreateSession;
end;

function TTileDownloaderFactory.CreateSession: ITileDownlodSession;
begin
  Result := TTileDownloaderBase.Create(FConfig);
end;

{ TTileDownloaderFactoryBase }

function TTileDownloaderFactoryBase.CreateSession: ITileDownlodSession;
begin
  Result := nil;
end;

end.
