unit u_DownloadExceptions;

interface

uses
  SysUtils;

type
  EDownloadError = class(Exception);

  EDownloadErrorWithoutConnect = class(EDownloadError);

  EProxyAuthError = class(EDownloadErrorWithoutConnect);
  ENoServerConnection = class(EDownloadErrorWithoutConnect);

  EDownloadErrorWithConnect = class(EDownloadError);

  EMimeTypeError = class(EDownloadErrorWithConnect);
  ESameTileSize = class(EDownloadErrorWithConnect);
  ETileNotExists = class(EDownloadErrorWithConnect);
  EDownloadBanned = class(EDownloadErrorWithConnect);

implementation

end.
