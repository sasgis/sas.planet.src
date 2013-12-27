unit u_DownloaderFaked;

interface

uses
  i_NotifierOperation,
  i_DownloadRequest,
  i_DownloadResult,
  i_DownloadResultFactory,
  i_Downloader,
  u_BaseInterfacedObject;

type
  TDownloaderFaked = class(TBaseInterfacedObject, IDownloader)
  private
    FResultFactory: IDownloadResultFactory;
  private
    function DoRequest(
      const ARequest: IDownloadRequest;
      const ACancelNotifier: INotifierOperation;
      const AOperationID: Integer
    ): IDownloadResult;
  public
    constructor Create(
      const AResultFactory: IDownloadResultFactory
    );
  end;

implementation


{ TDownloaderFaked }

constructor TDownloaderFaked.Create(const AResultFactory: IDownloadResultFactory);
begin
  inherited Create;
  FResultFactory := AResultFactory;
end;

function TDownloaderFaked.DoRequest(
  const ARequest: IDownloadRequest;
  const ACancelNotifier: INotifierOperation;
  const AOperationID: Integer
): IDownloadResult;
begin
  Result := FResultFactory.BuildCanceled(ARequest);
end;

end.
