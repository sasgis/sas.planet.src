unit i_DownloadResultFactory;

interface

uses
  Types,
  i_DownloadResult;

type
  IDownloadResultFactory = interface
    ['{672345FB-40BA-4B13-AADE-6771192478FD}']
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
    function BuildBadContentType(AContentType: string): IDownloadResultBadContentType;
    function BuildBanned: IDownloadResultBanned;
    function BuildDataNotExists(AReasonText: string): IDownloadResultDataNotExists;
    function BuildDataNotExistsByStatusCode(AStatusCode: DWORD): IDownloadResultDataNotExists;
    function BuildDataNotExistsZeroSize: IDownloadResultDataNotExists;
    function BuildNotNecessary(AReasonText: string): IDownloadResultNotNecessary;
  end;

implementation

end.
