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
      AStatusCode: Cardinal;
      ARawResponseHeader: string;
      AContentType: string;
      ASize: Integer;
      ABuffer: Pointer
    ): IDownloadResultOk;
    function BuildUnexpectedProxyAuth: IDownloadResultProxyError;
    function BuildBadProxyAuth: IDownloadResultProxyError;
    function BuildNoConnetctToServerByErrorCode(AErrorCode: DWORD): IDownloadResultNoConnetctToServer;
    function BuildLoadErrorByStatusCode(AStatusCode: DWORD): IDownloadResultError;
    function BuildLoadErrorByUnknownStatusCode(AStatusCode: DWORD): IDownloadResultError;
    function BuildLoadErrorByErrorCode(AErrorCode: DWORD): IDownloadResultError;
    function BuildBadContentType(AContentType, ARawResponseHeader: string): IDownloadResultBadContentType;
    function BuildBanned(ARawResponseHeader: string): IDownloadResultBanned;
    function BuildDataNotExists(AReasonText, ARawResponseHeader: string): IDownloadResultDataNotExists;
    function BuildDataNotExistsByStatusCode(ARawResponseHeader: string; AStatusCode: DWORD): IDownloadResultDataNotExists;
    function BuildDataNotExistsZeroSize(ARawResponseHeader: string): IDownloadResultDataNotExists;
    function BuildNotNecessary(AReasonText, ARawResponseHeader: string): IDownloadResultNotNecessary;
  end;

implementation

end.
