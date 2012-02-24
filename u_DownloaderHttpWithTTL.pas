unit u_DownloaderHttpWithTTL;

interface

uses
  SyncObjs,
  i_OperationNotifier,
  i_TTLCheckListener,
  i_TTLCheckNotifier,
  i_DownloadRequest,
  i_DownloadResult,
  i_DownloadResultFactory,
  i_Downloader,
  u_DownloaderHttp;

type
  TDownloaderHttpWithTTL = class(TInterfacedObject, IDownloader)
  private
    FResultFactory: IDownloadResultFactory;
    FGCList: ITTLCheckNotifier;

    FTTLListener: ITTLCheckListener;
    FCS: TCriticalSection;
    FDownloader: IDownloader;
    procedure OnTTLTrim(Sender: TObject);
  protected
    function DoRequest(
      ARequest: IDownloadRequest;
      ACancelNotifier: IOperationNotifier;
      AOperationID: Integer
    ): IDownloadResult;
  public
    constructor Create(
      AGCList: ITTLCheckNotifier;
      AResultFactory: IDownloadResultFactory
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_TTLCheckListener;

{ TDownloaderHttpWithTTL }

constructor TDownloaderHttpWithTTL.Create(
  AGCList: ITTLCheckNotifier;
  AResultFactory: IDownloadResultFactory
);
const
  CHttpClientTTL = 300000; // 5 min
  CHttpClientTTLCheckInterval = 30000; // 30 sec
begin
  FResultFactory := AResultFactory;
  FGCList := AGCList;
  FCS := TCriticalSection.Create;
  FTTLListener := TTTLCheckListener.Create(
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
  FCS.Free;
  inherited;
end;

function TDownloaderHttpWithTTL.DoRequest(
  ARequest: IDownloadRequest;
  ACancelNotifier: IOperationNotifier;
  AOperationID: Integer
): IDownloadResult;
var
  VDownloader: IDownloader;
begin
  FCS.Acquire;
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
    FCS.Release;
  end;
end;

procedure TDownloaderHttpWithTTL.OnTTLTrim(Sender: TObject);
begin
  FCS.Acquire;
  try
    FDownloader := nil;
  finally
    FCS.Release;
  end;
end;

end.
