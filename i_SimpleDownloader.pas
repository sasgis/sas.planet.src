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

unit i_SimpleDownloader;

interface

uses
  Classes;

type
  TSimpleDownloaderEvent = procedure (
    Sender: TObject;
    AResponseCode: Cardinal;
    const AContentType: string;
    const AResponseHead: string;
    AResponseBuf: TMemoryStream
  ) of object;

  ISimpleDownloader = interface
    ['{B45879E0-C88E-4C4F-954B-72991CF39FF4}']
    function GetFromInternet(
      AUrl: string;
      AAcceptEncoding: string;
      ARequestHead: string;
      ARequestBuf: TMemoryStream;
      AResponseBuf: TMemoryStream;
      out AContentType: string;
      out AResponseHead: string
    ): Cardinal;
    procedure GetFromInternetAsync(
      AUrl: string;
      AAcceptEncoding: string;
      ARequestHead: string;
      ARequestBuf: TMemoryStream;
      AOnDownload: TSimpleDownloaderEvent
    );
  end;

implementation

end.
