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
    FWaitInterval: Cardinal;
    function CreateSession: ITileDownlodSession; virtual;
    function GetWaitInterval: Cardinal; virtual;
    procedure SetWaitInterval(AValue: Cardinal); virtual;
  public
    constructor Create(AConfigData: IConfigDataProvider);
  end;

  TTileDownloaderFactory = class(TTileDownloaderFactoryBase, ISimpleFactory)
  private
    FConfig: ITileDownloaderConfig;
    FIgnoreContent_Type: Boolean;
    FContent_Type: string;
    FDefaultContent_Type: string;
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
  VParams := AConfigData.GetSubItem('params.txt').GetSubItem('PARAMS');
  FIgnoreContent_Type := VParams.ReadBool('IgnoreContentType', False);
  FDefaultContent_Type := VParams.ReadString('DefaultContentType', 'image/jpg');
  FContent_Type := VParams.ReadString('ContentType', 'image/jpg');
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
  VDownloader.WaitInterval := FWaitInterval;
  Result := VDownloader;
end;

{ TTileDownloaderFactoryBase }

constructor TTileDownloaderFactoryBase.Create(AConfigData: IConfigDataProvider);
var
  VParams: IConfigDataProvider;
begin
  VParams := AConfigData.GetSubItem('params.txt').GetSubItem('PARAMS');
  FWaitInterval := VParams.ReadInteger('Sleep', 0);
end;

function TTileDownloaderFactoryBase.CreateSession: ITileDownlodSession;
begin
  Result := nil;
end;

function TTileDownloaderFactoryBase.GetWaitInterval: Cardinal;
begin
  Result := FWaitInterval;
end;

procedure TTileDownloaderFactoryBase.SetWaitInterval(AValue: Cardinal);
begin
  FWaitInterval := AValue;
end;

end.
