unit i_DownloadResultFactory;

interface

uses
  Types,
  i_DownloadResult;

type
  IDownloadResultFactory = interface
    ['{672345FB-40BA-4B13-AADE-6771192478FD}']
    function BuildCanceled: IDownloadResultCanceled;
    function BuildOk(
      const AStatusCode: Cardinal;
      const ARawResponseHeader: string;
      const AContentType: string;
      const ASize: Integer;
      const ABuffer: Pointer
    ): IDownloadResultOk;
    function BuildUnexpectedProxyAuth: IDownloadResultProxyError;
    function BuildBadProxyAuth: IDownloadResultProxyError;
    function BuildNoConnetctToServerByErrorCode(
      const AErrorCode: DWORD
    ): IDownloadResultNoConnetctToServer;
    function BuildLoadErrorByStatusCode(
      const AStatusCode: DWORD
    ): IDownloadResultError;
    function BuildLoadErrorByUnknownStatusCode(
      const AStatusCode: DWORD
    ): IDownloadResultError;
    function BuildLoadErrorByErrorCode(
      const AErrorCode: DWORD
    ): IDownloadResultError;
    function BuildBadContentType(
      const AContentType, ARawResponseHeader: string
    ): IDownloadResultBadContentType;
    function BuildBanned(
      const ARawResponseHeader: string
    ): IDownloadResultBanned;
    function BuildDataNotExists(
      const AReasonText, ARawResponseHeader: string
    ): IDownloadResultDataNotExists;
    function BuildDataNotExistsByStatusCode(
      const ARawResponseHeader: string;
      const AStatusCode: DWORD
    ): IDownloadResultDataNotExists;
    function BuildDataNotExistsZeroSize(
      const ARawResponseHeader: string
    ): IDownloadResultDataNotExists;
    function BuildNotNecessary(
      const AReasonText, ARawResponseHeader: string
    ): IDownloadResultNotNecessary;
  end;

implementation

end.
