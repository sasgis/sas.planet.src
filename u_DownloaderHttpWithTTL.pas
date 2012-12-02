unit u_DownloaderHttpWithTTL;

interface

uses
  SysUtils,
  i_NotifierOperation,
  i_ListenerTTLCheck,
  i_NotifierTTLCheck,
  i_DownloadRequest,
  i_DownloadResult,
  i_DownloadResultFactory,
  i_Downloader,
  u_DownloaderHttp,
  u_BaseInterfacedObject;

type
  TDownloaderHttpWithTTL = class(TBaseInterfacedObject, IDownloader)
  private
    FResultFactory: IDownloadResultFactory;
    FGCList: INotifierTTLCheck;

    FTTLListener: IListenerTTLCheck;
    FCS: IReadWriteSync;
    FDownloader: IDownloader;
    procedure OnTTLTrim(Sender: TObject);
  private
    function DoRequest(
      const ARequest: IDownloadRequest;
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer
    ): IDownloadResult;
  public
    constructor Create(
      const AGCList: INotifierTTLCheck;
      const AResultFactory: IDownloadResultFactory
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_Synchronizer,
  u_ListenerTTLCheck;

{ TDownloaderHttpWithTTL }

constructor TDownloaderHttpWithTTL.Create(
  const AGCList: INotifierTTLCheck;
  const AResultFactory: IDownloadResultFactory
);
const
  CHttpClientTTL = 300000; // 5 min
  CHttpClientTTLCheckInterval = 30000; // 30 sec
begin
  inherited Create;
  FResultFactory := AResultFactory;
  FGCList := AGCList;
  FCS := MakeSyncRW_Std(Self, FALSE);
  FTTLListener := TListenerTTLCheck.Create(
    Self.OnTTLTrim,
    CHttpClientTTL,
    CHttpClientTTLCheckInterval
  );
  FGCList.Add(FTTLListener);
end;

destructor TDownloaderHttpWithTTL.Destroy;
begin
  FGCList.Remove(FTTLListener);
  FTTLListener := nil;
  FGCList := nil;
  FCS := nil;
  inherited;
end;

function TDownloaderHttpWithTTL.DoRequest(
  const ARequest: IDownloadRequest;
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer
): IDownloadResult;
var
  VDownloader: IDownloader;
begin
  FCS.BeginWrite;
  try
    FTTLListener.UpdateUseTime;
    VDownloader := FDownloader;
    if VDownloader = nil then begin
      VDownloader := TDownloaderHttp.Create(FResultFactory);
      FDownloader := VDownloader;
    end;
    Result := VDownloader.DoRequest(ARequest, ACancelNotifier, AOperationID);
    FTTLListener.UpdateUseTime;
  finally
    FCS.EndWrite;
  end;
end;

procedure TDownloaderHttpWithTTL.OnTTLTrim(Sender: TObject);
begin
  FCS.BeginWrite;
  try
    FDownloader := nil;
  finally
    FCS.EndWrite;
  end;
end;

end.
