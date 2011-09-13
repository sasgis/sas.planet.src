unit i_DownloadResultFactory;

interface

uses
  Types,
  i_DownloadRequest,
  i_DownloadResult;

type
  IDownloadResultFactory = interface
    ['{672345FB-40BA-4B13-AADE-6771192478FD}']
    function BuildCanceled(ARequest: IDownloadRequest): IDownloadResultCanceled;
    function BuildOk(
      ARequest: IDownloadRequest;
      const AStatusCode: Cardinal;
      const ARawResponseHeader: string;
      const AContentType: string;
      const ASize: Integer;
      const ABuffer: Pointer
    ): IDownloadResultOk;
    function BuildUnexpectedProxyAuth(ARequest: IDownloadRequest): IDownloadResultProxyError;
    function BuildBadProxyAuth(ARequest: IDownloadRequest): IDownloadResultProxyError;
    function BuildNoConnetctToServerByErrorCode(
      ARequest: IDownloadRequest;
      const AErrorCode: DWORD
    ): IDownloadResultNoConnetctToServer;
    function BuildLoadErrorByStatusCode(
      ARequest: IDownloadRequest;
      const AStatusCode: DWORD
    ): IDownloadResultError;
    function BuildLoadErrorByUnknownStatusCode(
      ARequest: IDownloadRequest;
      const AStatusCode: DWORD
    ): IDownloadResultError;
    function BuildLoadErrorByErrorCode(
      ARequest: IDownloadRequest;
      const AErrorCode: DWORD
    ): IDownloadResultError;
    function BuildBadContentType(
      ARequest: IDownloadRequest;
      const AContentType, ARawResponseHeader: string
    ): IDownloadResultBadContentType;
    function BuildBanned(
      ARequest: IDownloadRequest;
      const ARawResponseHeader: string
    ): IDownloadResultBanned;
    function BuildDataNotExists(
      ARequest: IDownloadRequest;
      const AReasonText, ARawResponseHeader: string
    ): IDownloadResultDataNotExists;
    function BuildDataNotExistsByStatusCode(
      ARequest: IDownloadRequest;
      const ARawResponseHeader: string;
      const AStatusCode: DWORD
    ): IDownloadResultDataNotExists;
    function BuildDataNotExistsZeroSize(
      ARequest: IDownloadRequest;
      const ARawResponseHeader: string
    ): IDownloadResultDataNotExists;
    function BuildNotNecessary(
      ARequest: IDownloadRequest;
      const AReasonText, ARawResponseHeader: string
    ): IDownloadResultNotNecessary;
  end;

implementation

end.
