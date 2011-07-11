unit u_DownloadResultFactorySimpleDownload;

interface

uses
  Types,
  i_DownloadResult,
  i_DownloadResultTextProvider,
  i_DownloadResultFactory;

type
  TDownloadResultFactorySimpleDownload = class(TInterfacedObject, IDownloadResultFactory)
  private
    FTextProvider: IDownloadResultTextProvider;
    FUrl: string;
    FRequestHead: string;
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
      AUrl: string;
      ARequestHead: string
    );
  end;

implementation

uses
  u_DownloadResult;

{ TDownloadResultFactorySimpleDownload }

constructor TDownloadResultFactorySimpleDownload.Create(
  ATextProvider: IDownloadResultTextProvider;
  AUrl, ARequestHead: string
);
begin
  FUrl := AUrl;
  FRequestHead := ARequestHead;
  FTextProvider := ATextProvider;
end;

function TDownloadResultFactorySimpleDownload.BuildBadContentType(
  const AContentType, ARawResponseHeader: string
): IDownloadResultBadContentType;
begin
  Result := TDownloadResultBadContentType.Create(FUrl, FRequestHead, AContentType, ARawResponseHeader, 'Неожиданный тип %s');
end;

function TDownloadResultFactorySimpleDownload.BuildBadProxyAuth: IDownloadResultProxyError;
begin
  Result := TDownloadResultProxyError.Create(FUrl, FRequestHead, 'Ошибка авторизации на прокси');
end;

function TDownloadResultFactorySimpleDownload.BuildBanned(
  const ARawResponseHeader: string
): IDownloadResultBanned;
begin
  Result := TDownloadResultBanned.Create(FUrl, FRequestHead, ARawResponseHeader, 'Похоже вас забанили');
end;

function TDownloadResultFactorySimpleDownload.BuildCanceled: IDownloadResultCanceled;
begin
  Result := TDownloadResultCanceled.Create(FUrl, FRequestHead);
end;

function TDownloadResultFactorySimpleDownload.BuildDataNotExists(
  const AReasonText, ARawResponseHeader: string
): IDownloadResultDataNotExists;
begin
  Result := TDownloadResultDataNotExists.Create(FUrl, FRequestHead, AReasonText, ARawResponseHeader);
end;

function TDownloadResultFactorySimpleDownload.BuildDataNotExistsByStatusCode(
  const ARawResponseHeader: string;
  const AStatusCode: DWORD
): IDownloadResultDataNotExists;
begin
  Result := TDownloadResultDataNotExistsByStatusCode.Create(FUrl, FRequestHead, ARawResponseHeader, 'Данныео отсутствуют. Статус %d', AStatusCode);
end;

function TDownloadResultFactorySimpleDownload.BuildDataNotExistsZeroSize(
  const ARawResponseHeader: string
): IDownloadResultDataNotExists;
begin
  Result := TDownloadResultDataNotExistsZeroSize.Create(FUrl, FRequestHead, ARawResponseHeader, 'Получен ответ нулевой длинны');
end;

function TDownloadResultFactorySimpleDownload.BuildLoadErrorByErrorCode(
  const AErrorCode: DWORD
): IDownloadResultError;
begin
  Result := TDownloadResultLoadErrorByErrorCode.Create(FUrl, FRequestHead, 'Ошибка загрузки. Код ошибки %d', AErrorCode);
end;

function TDownloadResultFactorySimpleDownload.BuildLoadErrorByStatusCode(
  const AStatusCode: DWORD
): IDownloadResultError;
begin
  Result := TDownloadResultLoadErrorByStatusCode.Create(FUrl, FRequestHead, 'Ошибка загрузки. Статус %d', AStatusCode);
end;

function TDownloadResultFactorySimpleDownload.BuildLoadErrorByUnknownStatusCode(
  const AStatusCode: DWORD
): IDownloadResultError;
begin
  Result := TDownloadResultLoadErrorByUnknownStatusCode.Create(FUrl, FRequestHead, 'Неизвестный статус %d', AStatusCode);
end;

function TDownloadResultFactorySimpleDownload.BuildNoConnetctToServerByErrorCode(
  const AErrorCode: DWORD
): IDownloadResultNoConnetctToServer;
begin
  Result := TDownloadResultNoConnetctToServerByErrorCode.Create(FUrl, FRequestHead, 'Ошибка подключения к серверу. Код ошибки %d', AErrorCode);
end;

function TDownloadResultFactorySimpleDownload.BuildNotNecessary(
  const AReasonText, ARawResponseHeader: string
): IDownloadResultNotNecessary;
begin
  Result := TDownloadResultNotNecessary.Create(FUrl, FRequestHead, AReasonText, ARawResponseHeader);
end;

function TDownloadResultFactorySimpleDownload.BuildOk(
  const AStatusCode: Cardinal;
  const ARawResponseHeader, AContentType: string;
  const ASize: Integer;
  const ABuffer: Pointer
): IDownloadResultOk;
begin
  Result := TDownloadResultOk.Create(FUrl, FRequestHead, AStatusCode, ARawResponseHeader, AContentType, ASize, ABuffer);
end;

function TDownloadResultFactorySimpleDownload.BuildUnexpectedProxyAuth: IDownloadResultProxyError;
begin
  Result := TDownloadResultProxyError.Create(FUrl, FRequestHead, 'Настройки не предусматривают авторизацию на прокси');
end;

end.
