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

unit i_DownloadResultFactory;

interface

uses
  Types,
  i_BinaryData,
  i_DownloadRequest,
  i_DownloadResult;

type
  IDownloadResultFactory = interface
    ['{672345FB-40BA-4B13-AADE-6771192478FD}']
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
  end;

implementation

end.
