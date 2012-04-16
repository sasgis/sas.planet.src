unit u_DownloaderFaked;

interface

uses
  i_OperationNotifier,
  i_DownloadRequest,
  i_DownloadResult,
  i_DownloadResultFactory,
  i_Downloader;

type
  TDownloaderFaked = class(TInterfacedObject, IDownloader)
  private
    FResultFactory: IDownloadResultFactory;
  protected
    function DoRequest(
      const ARequest: IDownloadRequest;
      const ACancelNotifier: IOperationNotifier;
      AOperationID: Integer
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
  const ACancelNotifier: IOperationNotifier;
  AOperationID: Integer
): IDownloadResult;
begin
  Result := FResultFactory.BuildCanceled(ARequest);
end;

end.
