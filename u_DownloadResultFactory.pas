{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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
  protected
    function BuildCanceled(ARequest: IDownloadRequest): IDownloadResultCanceled;
    function BuildOk(
      ARequest: IDownloadRequest;
      const AStatusCode: Cardinal;
      const ARawResponseHeader: string;
      const AContentType: string;
      AData: IBinaryData
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
    function BuildLoadErrorByUnknownReason(
      ARequest: IDownloadRequest;
      const AReason: string
    ): IDownloadResultError;
    function BuildBadContentType(
      ARequest: IDownloadRequest;
      const AContentType: string;
      const AStatusCode: DWORD;
      const ARawResponseHeader: string
    ): IDownloadResultBadContentType;
    function BuildBanned(
      ARequest: IDownloadRequest;
      const AStatusCode: DWORD;
      const ARawResponseHeader: string
    ): IDownloadResultBanned;
    function BuildDataNotExists(
      ARequest: IDownloadRequest;
      const AReasonText: string;
      const AStatusCode: DWORD;
      const ARawResponseHeader: string
    ): IDownloadResultDataNotExists;
    function BuildDataNotExistsByStatusCode(
      ARequest: IDownloadRequest;
      const ARawResponseHeader: string;
      const AStatusCode: DWORD
    ): IDownloadResultDataNotExists;
    function BuildDataNotExistsZeroSize(
      ARequest: IDownloadRequest;
      const AStatusCode: DWORD;
      const ARawResponseHeader: string
    ): IDownloadResultDataNotExists;
    function BuildNotNecessary(
      ARequest: IDownloadRequest;
      const AReasonText: string;
      const AStatusCode: DWORD;
      const ARawResponseHeader: string
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

function TDownloadResultFactory.BuildBadContentType(
  ARequest: IDownloadRequest;
  const AContentType: string;
  const AStatusCode: DWORD;
  const ARawResponseHeader: string
): IDownloadResultBadContentType;
begin
  Result := TDownloadResultBadContentType.Create(ARequest, AContentType, AStatusCode, ARawResponseHeader, 'Неожиданный тип %s');
end;

function TDownloadResultFactory.BuildBadProxyAuth(ARequest: IDownloadRequest):
    IDownloadResultProxyError;
begin
  Result := TDownloadResultProxyError.Create(ARequest, 'Ошибка авторизации на прокси');
end;

function TDownloadResultFactory.BuildBanned(
  ARequest: IDownloadRequest;
  const AStatusCode: DWORD;
  const ARawResponseHeader: string
): IDownloadResultBanned;
begin
  Result := TDownloadResultBanned.Create(ARequest, AStatusCode, ARawResponseHeader, 'Похоже вас забанили');
end;

function TDownloadResultFactory.BuildCanceled(ARequest: IDownloadRequest):
    IDownloadResultCanceled;
begin
  Result := TDownloadResultCanceled.Create(ARequest);
end;

function TDownloadResultFactory.BuildDataNotExists(
  ARequest: IDownloadRequest;
  const AReasonText: string;
  const AStatusCode: DWORD;
  const ARawResponseHeader: string
): IDownloadResultDataNotExists;
begin
  Result := TDownloadResultDataNotExists.Create(ARequest, AReasonText, AStatusCode, ARawResponseHeader);
end;

function TDownloadResultFactory.BuildDataNotExistsByStatusCode(ARequest:
    IDownloadRequest; const ARawResponseHeader: string; const AStatusCode:
    DWORD): IDownloadResultDataNotExists;
begin
  Result := TDownloadResultDataNotExistsByStatusCode.Create(ARequest, ARawResponseHeader, 'Данныео отсутствуют. Статус %d', AStatusCode);
end;

function TDownloadResultFactory.BuildDataNotExistsZeroSize(
  ARequest: IDownloadRequest;
  const AStatusCode: DWORD;
  const ARawResponseHeader: string
): IDownloadResultDataNotExists;
begin
  Result := TDownloadResultDataNotExistsZeroSize.Create(ARequest, AStatusCode, ARawResponseHeader, 'Получен ответ нулевой длинны');
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

function TDownloadResultFactory.BuildLoadErrorByUnknownReason(
  ARequest: IDownloadRequest; const AReason: string): IDownloadResultError;
begin
  Result := TDownloadResultLoadError.Create(ARequest, 'Ошибка при загрузке:' + AReason);
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

function TDownloadResultFactory.BuildNotNecessary(
  ARequest: IDownloadRequest;
  const AReasonText: string;
  const AStatusCode: DWORD;
  const ARawResponseHeader: string
): IDownloadResultNotNecessary;
begin
  Result := TDownloadResultNotNecessary.Create(ARequest, AReasonText, AStatusCode, ARawResponseHeader);
end;

function TDownloadResultFactory.BuildOk(
  ARequest: IDownloadRequest; const
  AStatusCode: Cardinal;
  const ARawResponseHeader: string;
  const AContentType: string;
  AData: IBinaryData
): IDownloadResultOk;
begin
  Result := TDownloadResultOk.Create(ARequest, AStatusCode, ARawResponseHeader, AContentType, AData);
end;

function TDownloadResultFactory.BuildUnexpectedProxyAuth(ARequest:
    IDownloadRequest): IDownloadResultProxyError;
begin
  Result := TDownloadResultProxyError.Create(ARequest, 'Настройки не предусматривают авторизацию на прокси');
end;

end.

