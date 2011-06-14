unit i_DownloadResultTextProvider;

interface

type
  IDownloadResultTextProvider = interface
    ['{70C2269A-0A1C-4D26-B4F6-D65C16698C76}']
    function GetMessageBadContentType: string;
    function GetMessageBadProxyAuth: string;
    function GetMessageBanned: string;
    function GetMessageDataNotExistsByStatusCode: string;
    function GetMessageDataNotExistsZeroSize: string;
    function GetMessageLoadErrorByErrorCode: string;
    function GetMessageLoadErrorByStatusCode: string;
    function GetMessageLoadErrorByUnknownStatusCode: string;
    function GetMessageNoConnetctToServerByErrorCode: string;
    function GetMessageUnexpectedProxyAuth: string;
  end;

implementation

end.
