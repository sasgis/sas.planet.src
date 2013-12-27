{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2013, SAS.Planet development team.                      *}
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

unit i_UpdateDownloader;

interface

type
  TUpdateDownloaderState = (
    udsIdle = 0,
    udsSearch,
    udsDownload,
    udsError
  );

  TUpdateChannel = (
    ucNightly = 0,
    ucStable
  );

  IUpdateDownloader = interface
    ['{19488A83-101C-4BBC-A5C3-36DE1EE7B2FA}']
    function SearchAvailableVersionInfoAsync(const AOperationID: Integer): TUpdateDownloaderState;
    function GetAvailableVersionInfo(out ADate: TDateTime; out ARev: Integer; out ABuildType: string): Boolean;

    function DownloadAvailableVersionAsync(const AOperationID: Integer): TUpdateDownloaderState;
    function GetDownloadProgress(out ADone, ATotal: Integer): Boolean;

    function GetState: TUpdateDownloaderState;
    
    function GetError: string;

    function GetFileName: string;

    procedure SetUpdateChannel(const AValue: TUpdateChannel);
  end;

implementation

end.
