unit u_DownloadResultFactory;

interface

uses
  Types,
  i_DownloadResult,
  i_DownloadRequest,
  i_DownloadResultTextProvider,
  i_DownloadResultFactory;

type
  TDownloadResultFactory = class(TInterfacedObject, IDownloadResultFactory)
  private
    FTextProvider: IDownloadResultTextProvider;
  protected
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
  public
    constructor Create(
      ATextProvider: IDownloadResultTextProvider
    );
  end;

implementation

uses
  u_DownloadResult;

{ TDownloadResultFactory }

constructor TDownloadResultFactory.Create(
  ATextProvider: IDownloadResultTextProvider
);
begin
  FTextProvider := ATextProvider;
end;

function TDownloadResultFactory.BuildBadContentType(ARequest: IDownloadRequest;
    const AContentType, ARawResponseHeader: string):
    IDownloadResultBadContentType;
begin
  Result := TDownloadResultBadContentType.Create(ARequest, AContentType, ARawResponseHeader, 'Неожиданный тип %s');
end;

function TDownloadResultFactory.BuildBadProxyAuth(ARequest: IDownloadRequest):
    IDownloadResultProxyError;
begin
  Result := TDownloadResultProxyError.Create(ARequest, 'Ошибка авторизации на прокси');
end;

function TDownloadResultFactory.BuildBanned(ARequest: IDownloadRequest; const
    ARawResponseHeader: string): IDownloadResultBanned;
begin
  Result := TDownloadResultBanned.Create(ARequest, ARawResponseHeader, 'Похоже вас забанили');
end;

function TDownloadResultFactory.BuildCanceled(ARequest: IDownloadRequest):
    IDownloadResultCanceled;
begin
  Result := TDownloadResultCanceled.Create(ARequest);
end;

function TDownloadResultFactory.BuildDataNotExists(ARequest: IDownloadRequest;
    const AReasonText, ARawResponseHeader: string):
    IDownloadResultDataNotExists;
begin
  Result := TDownloadResultDataNotExists.Create(ARequest, AReasonText, ARawResponseHeader);
end;

function TDownloadResultFactory.BuildDataNotExistsByStatusCode(ARequest:
    IDownloadRequest; const ARawResponseHeader: string; const AStatusCode:
    DWORD): IDownloadResultDataNotExists;
begin
  Result := TDownloadResultDataNotExistsByStatusCode.Create(ARequest, ARawResponseHeader, 'Данныео отсутствуют. Статус %d', AStatusCode);
end;

function TDownloadResultFactory.BuildDataNotExistsZeroSize(ARequest:
    IDownloadRequest; const ARawResponseHeader: string):
    IDownloadResultDataNotExists;
begin
  Result := TDownloadResultDataNotExistsZeroSize.Create(ARequest, ARawResponseHeader, 'Получен ответ нулевой длинны');
end;

function TDownloadResultFactory.BuildLoadErrorByErrorCode(ARequest:
    IDownloadRequest; const AErrorCode: DWORD): IDownloadResultError;
begin
  Result := TDownloadResultLoadErrorByErrorCode.Create(ARequest, 'Ошибка загрузки. Код ошибки %d', AErrorCode);
end;

function TDownloadResultFactory.BuildLoadErrorByStatusCode(ARequest:
    IDownloadRequest; const AStatusCode: DWORD): IDownloadResultError;
begin
  Result := TDownloadResultLoadErrorByStatusCode.Create(ARequest, 'Ошибка загрузки. Статус %d', AStatusCode);
end;

function TDownloadResultFactory.BuildLoadErrorByUnknownStatusCode(ARequest:
    IDownloadRequest; const AStatusCode: DWORD): IDownloadResultError;
begin
  Result := TDownloadResultLoadErrorByUnknownStatusCode.Create(ARequest, 'Неизвестный статус %d', AStatusCode);
end;

function TDownloadResultFactory.BuildNoConnetctToServerByErrorCode(ARequest:
    IDownloadRequest; const AErrorCode: DWORD):
    IDownloadResultNoConnetctToServer;
begin
  Result := TDownloadResultNoConnetctToServerByErrorCode.Create(ARequest, 'Ошибка подключения к серверу. Код ошибки %d', AErrorCode);
end;

function TDownloadResultFactory.BuildNotNecessary(ARequest: IDownloadRequest;
    const AReasonText, ARawResponseHeader: string): IDownloadResultNotNecessary;
begin
  Result := TDownloadResultNotNecessary.Create(ARequest, AReasonText, ARawResponseHeader);
end;

function TDownloadResultFactory.BuildOk(ARequest: IDownloadRequest; const
    AStatusCode: Cardinal; const ARawResponseHeader: string; const
    AContentType: string; const ASize: Integer; const ABuffer: Pointer):
    IDownloadResultOk;
begin
  Result := TDownloadResultOk.Create(ARequest, AStatusCode, ARawResponseHeader, AContentType, ASize, ABuffer);
end;

function TDownloadResultFactory.BuildUnexpectedProxyAuth(ARequest:
    IDownloadRequest): IDownloadResultProxyError;
begin
  Result := TDownloadResultProxyError.Create(ARequest, 'Настройки не предусматривают авторизацию на прокси');
end;

end.

