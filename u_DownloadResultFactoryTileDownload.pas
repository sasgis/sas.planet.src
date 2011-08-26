unit u_DownloadResultFactoryTileDownload;

interface

uses
  Types,
  i_DownloadResult,
  u_MapType,
  i_DownloadRequest,
  i_DownloadResultTextProvider,
  i_DownloadResultFactory;

type
  TDownloadResultFactory = class(TInterfacedObject, IDownloadResultFactory)
  private
    FTextProvider: IDownloadResultTextProvider;
    FRequest: IDownloadRequest;
  protected
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
  public
    constructor Create(
      ATextProvider: IDownloadResultTextProvider;
      ARequest: IDownloadRequest
    );
  end;

implementation

uses
  u_DownloadResult;

{ TDownloadResultFactory }

constructor TDownloadResultFactory.Create(
  ATextProvider: IDownloadResultTextProvider;
  ARequest: IDownloadRequest
);
begin
  FTextProvider := ATextProvider;
  FRequest := ARequest;
end;

function TDownloadResultFactory.BuildBadContentType(
  const AContentType, ARawResponseHeader: string
): IDownloadResultBadContentType;
begin
  Result := TDownloadResultBadContentType.Create(FRequest, AContentType, ARawResponseHeader, 'Неожиданный тип %s');
end;

function TDownloadResultFactory.BuildBadProxyAuth: IDownloadResultProxyError;
begin
  Result := TDownloadResultProxyError.Create(FRequest, 'Ошибка авторизации на прокси');
end;

function TDownloadResultFactory.BuildBanned(
  const ARawResponseHeader: string
): IDownloadResultBanned;
begin
  Result := TDownloadResultBanned.Create(FRequest, ARawResponseHeader, 'Похоже вас забанили');
end;

function TDownloadResultFactory.BuildCanceled: IDownloadResultCanceled;
begin
  Result := TDownloadResultCanceled.Create(FRequest);
end;

function TDownloadResultFactory.BuildDataNotExists(
  const AReasonText, ARawResponseHeader: string
): IDownloadResultDataNotExists;
begin
  Result := TDownloadResultDataNotExists.Create(FRequest, AReasonText, ARawResponseHeader);
end;

function TDownloadResultFactory.BuildDataNotExistsByStatusCode(
  const ARawResponseHeader: string;
  const AStatusCode: DWORD
): IDownloadResultDataNotExists;
begin
  Result := TDownloadResultDataNotExistsByStatusCode.Create(FRequest, ARawResponseHeader, 'Данныео отсутствуют. Статус %d', AStatusCode);
end;

function TDownloadResultFactory.BuildDataNotExistsZeroSize(
  const ARawResponseHeader: string
): IDownloadResultDataNotExists;
begin
  Result := TDownloadResultDataNotExistsZeroSize.Create(FRequest, ARawResponseHeader, 'Получен ответ нулевой длинны');
end;

function TDownloadResultFactory.BuildLoadErrorByErrorCode(
  const AErrorCode: DWORD
): IDownloadResultError;
begin
  Result := TDownloadResultLoadErrorByErrorCode.Create(FRequest, 'Ошибка загрузки. Код ошибки %d', AErrorCode);
end;

function TDownloadResultFactory.BuildLoadErrorByStatusCode(
  const AStatusCode: DWORD
): IDownloadResultError;
begin
  Result := TDownloadResultLoadErrorByStatusCode.Create(FRequest, 'Ошибка загрузки. Статус %d', AStatusCode);
end;

function TDownloadResultFactory.BuildLoadErrorByUnknownStatusCode(
  const AStatusCode: DWORD
): IDownloadResultError;
begin
  Result := TDownloadResultLoadErrorByUnknownStatusCode.Create(FRequest, 'Неизвестный статус %d', AStatusCode);
end;

function TDownloadResultFactory.BuildNoConnetctToServerByErrorCode(
  const AErrorCode: DWORD
): IDownloadResultNoConnetctToServer;
begin
  Result := TDownloadResultNoConnetctToServerByErrorCode.Create(FRequest, 'Ошибка подключения к серверу. Код ошибки %d', AErrorCode);
end;

function TDownloadResultFactory.BuildNotNecessary(
  const AReasonText, ARawResponseHeader: string
): IDownloadResultNotNecessary;
begin
  Result := TDownloadResultNotNecessary.Create(FRequest, AReasonText, ARawResponseHeader);
end;

function TDownloadResultFactory.BuildOk(
  const AStatusCode: Cardinal;
  const ARawResponseHeader, AContentType: string;
  const ASize: Integer;
  const ABuffer: Pointer
): IDownloadResultOk;
begin
  Result := TDownloadResultOk.Create(FRequest, AStatusCode, ARawResponseHeader, AContentType, ASize, ABuffer);
end;

function TDownloadResultFactory.BuildUnexpectedProxyAuth: IDownloadResultProxyError;
begin
  Result := TDownloadResultProxyError.Create(FRequest, 'Настройки не предусматривают авторизацию на прокси');
end;

end.

