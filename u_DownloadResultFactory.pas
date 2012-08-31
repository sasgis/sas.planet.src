{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_DownloadResultFactory;

interface

uses
  Types,
  i_BinaryData,
  i_DownloadResult,
  i_DownloadRequest,
  i_DownloadResultTextProvider,
  i_DownloadResultFactory;

type
  TDownloadResultFactory = class(TInterfacedObject, IDownloadResultFactory)
  private
    FTextProvider: IDownloadResultTextProvider;
  private
    function BuildCanceled(
      const ARequest: IDownloadRequest
    ): IDownloadResultCanceled;
    function BuildOk(
      const ARequest: IDownloadRequest;
      const AStatusCode: Cardinal;
      const ARawResponseHeader: AnsiString;
      const AContentType: AnsiString;
      const AData: IBinaryData
    ): IDownloadResultOk;
    function BuildUnexpectedProxyAuth(
      const ARequest: IDownloadRequest
    ): IDownloadResultProxyError;
    function BuildBadProxyAuth(
      const ARequest: IDownloadRequest
    ): IDownloadResultProxyError;
    function BuildNoConnetctToServerByErrorCode(
      const ARequest: IDownloadRequest;
      const AErrorCode: DWORD
    ): IDownloadResultNoConnetctToServer;
    function BuildLoadErrorByStatusCode(
      const ARequest: IDownloadRequest;
      const AStatusCode: DWORD
    ): IDownloadResultError;
    function BuildLoadErrorByUnknownStatusCode(
      const ARequest: IDownloadRequest;
      const AStatusCode: DWORD
    ): IDownloadResultError;
    function BuildLoadErrorByErrorCode(
      const ARequest: IDownloadRequest;
      const AErrorCode: DWORD
    ): IDownloadResultError;
    function BuildLoadErrorByUnknownReason(
      const ARequest: IDownloadRequest;
      const AReason: string
    ): IDownloadResultError;
    function BuildBadContentType(
      const ARequest: IDownloadRequest;
      const AContentType: AnsiString;
      const AStatusCode: DWORD;
      const ARawResponseHeader: AnsiString
    ): IDownloadResultBadContentType;
    function BuildBanned(
      const ARequest: IDownloadRequest;
      const AStatusCode: DWORD;
      const ARawResponseHeader: AnsiString
    ): IDownloadResultBanned;
    function BuildDataNotExists(
      const ARequest: IDownloadRequest;
      const AReasonText: string;
      const AStatusCode: DWORD;
      const ARawResponseHeader: AnsiString
    ): IDownloadResultDataNotExists;
    function BuildDataNotExistsByStatusCode(
      const ARequest: IDownloadRequest;
      const ARawResponseHeader: AnsiString;
      const AStatusCode: DWORD
    ): IDownloadResultDataNotExists;
    function BuildDataNotExistsZeroSize(
      const ARequest: IDownloadRequest;
      const AStatusCode: DWORD;
      const ARawResponseHeader: AnsiString
    ): IDownloadResultDataNotExists;
    function BuildNotNecessary(
      const ARequest: IDownloadRequest;
      const AReasonText: string;
      const AStatusCode: DWORD;
      const ARawResponseHeader: AnsiString
    ): IDownloadResultNotNecessary;
  public
    constructor Create(
      const ATextProvider: IDownloadResultTextProvider
    );
  end;

implementation

uses
  u_DownloadResult;

{ TDownloadResultFactory }

constructor TDownloadResultFactory.Create(
  const ATextProvider: IDownloadResultTextProvider
);
begin
  inherited Create;
  FTextProvider := ATextProvider;
end;

function TDownloadResultFactory.BuildBadContentType(
  const ARequest: IDownloadRequest;
  const AContentType: AnsiString;
  const AStatusCode: DWORD;
  const ARawResponseHeader: AnsiString
): IDownloadResultBadContentType;
begin
  Result := TDownloadResultBadContentType.Create(ARequest, AContentType, AStatusCode, ARawResponseHeader, 'Неожиданный тип %s');
end;

function TDownloadResultFactory.BuildBadProxyAuth(
  const ARequest: IDownloadRequest
): IDownloadResultProxyError;
begin
  Result := TDownloadResultProxyError.Create(ARequest, 'Ошибка авторизации на прокси');
end;

function TDownloadResultFactory.BuildBanned(
  const ARequest: IDownloadRequest;
  const AStatusCode: DWORD;
  const ARawResponseHeader: AnsiString
): IDownloadResultBanned;
begin
  Result := TDownloadResultBanned.Create(ARequest, AStatusCode, ARawResponseHeader, 'Похоже вас забанили');
end;

function TDownloadResultFactory.BuildCanceled(
  const ARequest: IDownloadRequest
): IDownloadResultCanceled;
begin
  Result := TDownloadResultCanceled.Create(ARequest);
end;

function TDownloadResultFactory.BuildDataNotExists(
  const ARequest: IDownloadRequest;
  const AReasonText: string;
  const AStatusCode: DWORD;
  const ARawResponseHeader: AnsiString
): IDownloadResultDataNotExists;
begin
  Result := TDownloadResultDataNotExists.Create(ARequest, AReasonText, AStatusCode, ARawResponseHeader);
end;

function TDownloadResultFactory.BuildDataNotExistsByStatusCode(
  const ARequest: IDownloadRequest;
  const ARawResponseHeader: AnsiString;
  const AStatusCode: DWORD
): IDownloadResultDataNotExists;
begin
  Result := TDownloadResultDataNotExistsByStatusCode.Create(ARequest, ARawResponseHeader, 'Данныео отсутствуют. Статус %d', AStatusCode);
end;

function TDownloadResultFactory.BuildDataNotExistsZeroSize(
  const ARequest: IDownloadRequest;
  const AStatusCode: DWORD;
  const ARawResponseHeader: AnsiString
): IDownloadResultDataNotExists;
begin
  Result := TDownloadResultDataNotExistsZeroSize.Create(ARequest, AStatusCode, ARawResponseHeader, 'Получен ответ нулевой длинны');
end;

function TDownloadResultFactory.BuildLoadErrorByErrorCode(
  const ARequest: IDownloadRequest;
  const AErrorCode: DWORD
): IDownloadResultError;
begin
  Result := TDownloadResultLoadErrorByErrorCode.Create(ARequest, 'Ошибка загрузки. Код ошибки %d', AErrorCode);
end;

function TDownloadResultFactory.BuildLoadErrorByStatusCode(
  const ARequest: IDownloadRequest;
  const AStatusCode: DWORD
): IDownloadResultError;
begin
  Result := TDownloadResultLoadErrorByStatusCode.Create(ARequest, 'Ошибка загрузки. Статус %d', AStatusCode);
end;

function TDownloadResultFactory.BuildLoadErrorByUnknownReason(
  const ARequest: IDownloadRequest;
  const AReason: string
): IDownloadResultError;
begin
  Result := TDownloadResultLoadError.Create(ARequest, 'Ошибка при загрузке:' + AReason);
end;

function TDownloadResultFactory.BuildLoadErrorByUnknownStatusCode(
  const ARequest: IDownloadRequest;
  const AStatusCode: DWORD
): IDownloadResultError;
begin
  Result := TDownloadResultLoadErrorByUnknownStatusCode.Create(ARequest, 'Неизвестный статус %d', AStatusCode);
end;

function TDownloadResultFactory.BuildNoConnetctToServerByErrorCode(
  const ARequest: IDownloadRequest;
  const AErrorCode: DWORD
): IDownloadResultNoConnetctToServer;
begin
  Result := TDownloadResultNoConnetctToServerByErrorCode.Create(ARequest, 'Ошибка подключения к серверу. Код ошибки %d', AErrorCode);
end;

function TDownloadResultFactory.BuildNotNecessary(
  const ARequest: IDownloadRequest;
  const AReasonText: string;
  const AStatusCode: DWORD;
  const ARawResponseHeader: AnsiString
): IDownloadResultNotNecessary;
begin
  Result := TDownloadResultNotNecessary.Create(ARequest, AReasonText, AStatusCode, ARawResponseHeader);
end;

function TDownloadResultFactory.BuildOk(
  const ARequest: IDownloadRequest;
  const AStatusCode: Cardinal;
  const ARawResponseHeader: AnsiString;
  const AContentType: AnsiString;
  const AData: IBinaryData
): IDownloadResultOk;
begin
  Result := TDownloadResultOk.Create(ARequest, AStatusCode, ARawResponseHeader, AContentType, AData);
end;

function TDownloadResultFactory.BuildUnexpectedProxyAuth(
  const ARequest: IDownloadRequest
): IDownloadResultProxyError;
begin
  Result := TDownloadResultProxyError.Create(ARequest, 'Настройки не предусматривают авторизацию на прокси');
end;

end.
