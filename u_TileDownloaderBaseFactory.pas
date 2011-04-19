unit u_TileDownloaderBaseFactory;

interface

uses
  i_ConfigDataProvider,
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
    constructor Create(AConfig: IConfigDataProvider);
  end;

  TTileDownloaderFactory = class(TTileDownloaderFactoryBase, ISimpleFactory)
  private
    FConfig: IConfigDataProvider;
    FIgnoreContent_Type: Boolean;
    FContent_Type: string;
    FDefaultContent_Type: string;
    FSlepOnResetConnection: Cardinal;
    function CreateInstance: IInterface;
    function CreateSession: ITileDownlodSession; override;
  public
    constructor Create(AConfig: IConfigDataProvider);
  end;

implementation

uses
  u_GlobalState,
  u_UrlGenerator,
  u_TileDownloaderBase;

{ TTileDownloaderBaseFactory }

constructor TTileDownloaderFactory.Create(AConfig: IConfigDataProvider);
var
  VParams: IConfigDataProvider;
begin
  inherited;
  FConfig := AConfig;
  VParams := FConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
  FIgnoreContent_Type := VParams.ReadBool('IgnoreContentType', False);
  FDefaultContent_Type := VParams.ReadString('DefaultContentType', 'image/jpg');
  FContent_Type := VParams.ReadString('ContentType', 'image/jpg');
  FSlepOnResetConnection := VParams.ReadInteger('SlepOnResetConnection', 5000);
end;

function TTileDownloaderFactory.CreateInstance: IInterface;
begin
  Result := CreateSession;
end;

function TTileDownloaderFactory.CreateSession: ITileDownlodSession;
var
  VDownloader: TTileDownloaderBase;
  VTryCount: Integer;
begin
  if GState.TwoDownloadAttempt then begin
    VTryCount := 2;
  end else begin
    VTryCount := 1;
  end;
  VDownloader := TTileDownloaderBase.Create(FIgnoreContent_Type,
    FContent_Type, FDefaultContent_Type, VTryCount, GState.InetConfig);
  VDownloader.SleepOnResetConnection := FSlepOnResetConnection;
  VDownloader.WaitInterval := FWaitInterval;
  try
    VDownloader.UrlGenerator := TUrlGenerator.Create(FConfig);
  except
    VDownloader.UrlGenerator := nil;
  end;
  Result := VDownloader;
end;

{ TTileDownloaderFactoryBase }

constructor TTileDownloaderFactoryBase.Create(AConfig: IConfigDataProvider);
var
  VParams: IConfigDataProvider;
begin
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
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
