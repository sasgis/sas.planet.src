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

unit i_Downloader;

interface

uses
  i_NotifierOperation,
  i_DownloadResult,
  i_DownloadRequest;

type
  TRequestAsyncCallBack = procedure(
      const AResult: IDownloadResult;
      const AOperationID: Integer
    ) of object;

  TOnDownloadProgress = procedure(
      const ARead: Integer;
      const ATotal: Integer
    ) of object;

  IDownloader = interface
    ['{08A98FF9-5EDE-4F6E-9D5B-351FBF4C05BE}']
    function DoRequest(
      const ARequest: IDownloadRequest;
      const ACancelNotifier: INotifierOperation;
      const AOperationID: Integer
    ): IDownloadResult;
  end;

  IDownloaderAsync = interface
    ['{52812DC3-BD28-4641-8C01-12B4A5933950}']
    procedure DoRequestAsync(
      const ARequest: IDownloadRequest;
      const ACancelNotifier: INotifierOperation;
      const AOperationID: Integer;
      const AOnResultCallBack: TRequestAsyncCallBack
    );
  end;

implementation

end.
