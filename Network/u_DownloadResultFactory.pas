{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_DownloadResultFactory;

interface

uses
  Types,
  i_BinaryData,
  i_DownloadResult,
  i_DownloadRequest,
  i_DownloadResultFactory,
  u_BaseInterfacedObject;

type
  TDownloadResultFactory = class(TBaseInterfacedObject, IDownloadResultFactory)
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
      const AReasonTextFormat: string;
      const AReasonTextArgs: array of const
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
      const AReasonTextFormat: string;
      const AReasonTextArgs: array of const;
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
      const AReasonTextFormat: string;
      const AReasonTextArgs: array of const;
      const AStatusCode: DWORD;
      const ARawResponseHeader: AnsiString
    ): IDownloadResultNotNecessary;
  end;

implementation

uses
  gnugettext,
  u_DownloadResult;

{ TDownloadResultFactory }

function TDownloadResultFactory.BuildBadContentType(
  const ARequest: IDownloadRequest;
  const AContentType: AnsiString;
  const AStatusCode: DWORD;
  const ARawResponseHeader: AnsiString
): IDownloadResultBadContentType;
begin
  Result :=
    TDownloadResultBadContentType.Create(
      ARequest,
      AContentType,
      AStatusCode,
      ARawResponseHeader,
      gettext_noop('Unexpected content type %s'),
      [AContentType]
    );
end;

function TDownloadResultFactory.BuildBadProxyAuth(
  const ARequest: IDownloadRequest
): IDownloadResultProxyError;
begin
  Result :=
    TDownloadResultProxyError.Create(
      ARequest,
      gettext_noop('Proxy authorization error'),
      []
    );
end;

function TDownloadResultFactory.BuildBanned(
  const ARequest: IDownloadRequest;
  const AStatusCode: DWORD;
  const ARawResponseHeader: AnsiString
): IDownloadResultBanned;
begin
  Result :=
    TDownloadResultBanned.Create(
      ARequest,
      AStatusCode,
      ARawResponseHeader,
      gettext_noop('Most likely you''ve been banned by the server!'),
      []
    );
end;

function TDownloadResultFactory.BuildCanceled(
  const ARequest: IDownloadRequest
): IDownloadResultCanceled;
begin
  Result := TDownloadResultCanceled.Create(ARequest);
end;

function TDownloadResultFactory.BuildDataNotExists(
  const ARequest: IDownloadRequest;
  const AReasonTextFormat: string;
  const AReasonTextArgs: array of const;
  const AStatusCode: DWORD;
  const ARawResponseHeader: AnsiString
): IDownloadResultDataNotExists;
begin
  Result :=
    TDownloadResultDataNotExists.Create(
      ARequest,
      AReasonTextFormat,
      AReasonTextArgs,
      AStatusCode,
      ARawResponseHeader
    );
end;

function TDownloadResultFactory.BuildDataNotExistsByStatusCode(
  const ARequest: IDownloadRequest;
  const ARawResponseHeader: AnsiString;
  const AStatusCode: DWORD
): IDownloadResultDataNotExists;
var
  VMessage: string;
begin
  case AStatusCode of
    204:
      VMessage := gettext_noop('HTTP %d No Content');
    400:
      VMessage := gettext_noop('HTTP %d Bad Request');
    404:
      VMessage := gettext_noop('HTTP %d Not Found');
    else
      VMessage := gettext_noop('HTTP %d Unknown Error');
  end;
  Result :=
    TDownloadResultDataNotExistsByStatusCode.Create(
      ARequest,
      ARawResponseHeader,
      VMessage,
      [AStatusCode],
      AStatusCode
    );
end;

function TDownloadResultFactory.BuildDataNotExistsZeroSize(
  const ARequest: IDownloadRequest;
  const AStatusCode: DWORD;
  const ARawResponseHeader: AnsiString
): IDownloadResultDataNotExists;
begin
  Result :=
    TDownloadResultDataNotExistsZeroSize.Create(
      ARequest,
      AStatusCode,
      ARawResponseHeader,
      gettext_noop('Size of answer is zero'),
      []
    );
end;

function TDownloadResultFactory.BuildLoadErrorByErrorCode(
  const ARequest: IDownloadRequest;
  const AErrorCode: DWORD
): IDownloadResultError;
begin
  Result :=
    TDownloadResultLoadErrorByErrorCode.Create(
      ARequest,
      gettext_noop('Download error. Error code %d'),
      [AErrorCode],
      AErrorCode
    );
end;

function TDownloadResultFactory.BuildLoadErrorByStatusCode(
  const ARequest: IDownloadRequest;
  const AStatusCode: DWORD
): IDownloadResultError;
begin
  Result :=
    TDownloadResultLoadErrorByStatusCode.Create(
      ARequest,
      gettext_noop('Download error. Status code %d'),
      [AStatusCode],
      AStatusCode
    );
end;

function TDownloadResultFactory.BuildLoadErrorByUnknownReason(
  const ARequest: IDownloadRequest;
  const AReasonTextFormat: string;
  const AReasonTextArgs: array of const
): IDownloadResultError;
begin
  Result :=
    TDownloadResultUnknownError.Create(
      ARequest,
      AReasonTextFormat,
      AReasonTextArgs
    );
end;

function TDownloadResultFactory.BuildLoadErrorByUnknownStatusCode(
  const ARequest: IDownloadRequest;
  const AStatusCode: DWORD
): IDownloadResultError;
begin
  Result :=
    TDownloadResultLoadErrorByUnknownStatusCode.Create(
      ARequest,
      gettext_noop('Unknown status code %d'),
      [AStatusCode],
      AStatusCode
    );
end;

function TDownloadResultFactory.BuildNoConnetctToServerByErrorCode(
  const ARequest: IDownloadRequest;
  const AErrorCode: DWORD
): IDownloadResultNoConnetctToServer;
begin
  Result :=
    TDownloadResultNoConnetctToServerByErrorCode.Create(
      ARequest,
      gettext_noop('Connect to server error. Error code %d'),
      [AErrorCode],
      AErrorCode
    );
end;

function TDownloadResultFactory.BuildNotNecessary(
  const ARequest: IDownloadRequest;
  const AReasonTextFormat: string;
  const AReasonTextArgs: array of const;
  const AStatusCode: DWORD;
  const ARawResponseHeader: AnsiString
): IDownloadResultNotNecessary;
begin
  Result :=
    TDownloadResultNotNecessary.Create(
      ARequest,
      AReasonTextFormat,
      AReasonTextArgs,
      AStatusCode,
      ARawResponseHeader
    );
end;

function TDownloadResultFactory.BuildOk(
  const ARequest: IDownloadRequest;
  const AStatusCode: Cardinal;
  const ARawResponseHeader: AnsiString;
  const AContentType: AnsiString;
  const AData: IBinaryData
): IDownloadResultOk;
begin
  Result :=
    TDownloadResultOk.Create(
      ARequest,
      AStatusCode,
      ARawResponseHeader,
      AContentType,
      AData
    );
end;

function TDownloadResultFactory.BuildUnexpectedProxyAuth(
  const ARequest: IDownloadRequest
): IDownloadResultProxyError;
begin
  Result :=
    TDownloadResultProxyError.Create(
      ARequest,
      gettext_noop('Unexpected proxy authorization'),
      []
    );
end;

end.
