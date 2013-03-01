unit u_HttpStatusChecker;

interface

function IsConnectError(const ALastError: Cardinal): Boolean;
function IsDownloadError(const ALastError: Cardinal): Boolean;
function IsDownloadErrorStatus(const AStatusCode: Cardinal): Boolean;
function IsOkStatus(const AStatusCode: Cardinal): Boolean;
function IsContentNotExistStatus(const AStatusCode: Cardinal): Boolean;

implementation

uses
  WinInet;

function IsConnectError(const ALastError: Cardinal): Boolean;
begin
  case ALastError of
    ERROR_INTERNET_OUT_OF_HANDLES,
    ERROR_INTERNET_TIMEOUT,
    ERROR_INTERNET_INTERNAL_ERROR,
    ERROR_INTERNET_INVALID_URL,
    ERROR_INTERNET_UNRECOGNIZED_SCHEME,
    ERROR_INTERNET_NAME_NOT_RESOLVED,
    ERROR_INTERNET_PROTOCOL_NOT_FOUND,
    ERROR_INTERNET_SHUTDOWN,
    ERROR_INTERNET_INVALID_OPERATION,
    ERROR_INTERNET_OPERATION_CANCELLED,
    ERROR_INTERNET_NOT_PROXY_REQUEST,
    ERROR_INTERNET_NO_DIRECT_ACCESS,
    ERROR_INTERNET_INCORRECT_FORMAT,
    ERROR_INTERNET_ITEM_NOT_FOUND,
    ERROR_INTERNET_CANNOT_CONNECT,
    ERROR_INTERNET_INVALID_PROXY_REQUEST,
    ERROR_HTTP_INVALID_HEADER,
    ERROR_INTERNET_TCPIP_NOT_INSTALLED,
    ERROR_INTERNET_SERVER_UNREACHABLE,
    ERROR_INTERNET_PROXY_SERVER_UNREACHABLE,
    ERROR_INTERNET_BAD_AUTO_PROXY_SCRIPT,
    ERROR_INTERNET_UNABLE_TO_DOWNLOAD_SCRIPT:
    begin
      Result := True;
    end;
  else begin
    Result := False;
  end;
  end;
end;

function IsDownloadError(const ALastError: Cardinal): Boolean;
begin
  case ALastError of
    ERROR_INTERNET_EXTENDED_ERROR,
    ERROR_INTERNET_CONNECTION_ABORTED,
    ERROR_INTERNET_CONNECTION_RESET,
    ERROR_INTERNET_SEC_CERT_CN_INVALID,
    ERROR_INTERNET_HTTP_TO_HTTPS_ON_REDIR,
    ERROR_INTERNET_HTTPS_TO_HTTP_ON_REDIR,
    ERROR_HTTP_DOWNLEVEL_SERVER,
    ERROR_HTTP_INVALID_SERVER_RESPONSE,
    ERROR_HTTP_INVALID_HEADER,
    ERROR_HTTP_REDIRECT_FAILED:
    begin
      Result := True;
    end;
  else begin
    Result := False;
  end;
  end;
end;

function IsDownloadErrorStatus(const AStatusCode: Cardinal): Boolean;
begin
  case AStatusCode of
    HTTP_STATUS_SERVER_ERROR,
    HTTP_STATUS_NOT_SUPPORTED,
    HTTP_STATUS_BAD_GATEWAY,
    HTTP_STATUS_SERVICE_UNAVAIL,
    HTTP_STATUS_GATEWAY_TIMEOUT:
    begin
      Result := True;
    end;
  else begin
    Result := False;
  end;
  end;
end;

function IsOkStatus(const AStatusCode: Cardinal): Boolean;
begin
  case AStatusCode of
    HTTP_STATUS_OK,
    HTTP_STATUS_CREATED,
    HTTP_STATUS_ACCEPTED,
    HTTP_STATUS_PARTIAL,
    HTTP_STATUS_RESET_CONTENT,
    HTTP_STATUS_PARTIAL_CONTENT:
    begin
      Result := True;
    end;
  else begin
    Result := False;
  end;
  end;
end;

function IsContentNotExistStatus(const AStatusCode: Cardinal): Boolean;
begin
  case AStatusCode of
    HTTP_STATUS_NO_CONTENT,
    HTTP_STATUS_BAD_REQUEST,
    HTTP_STATUS_NOT_FOUND:
    begin
      Result := True;
    end;
  else begin
    Result := False;
  end;
  end;
end;

end.