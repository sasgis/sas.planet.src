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

unit i_DownloadRequest;

interface

uses
  i_InetConfig;

type
  IDownloadRequest = interface
    ['{CE40F570-AB2A-465C-843D-0217CB2CFC47}']
    function GetUrl: string;
    property Url: string read GetUrl;

    function GetRequestHeader: string;
    property RequestHeader: string read GetRequestHeader;

    function GetInetConfig: IInetConfigStatic;
    property InetConfig: IInetConfigStatic read GetInetConfig;
  end;

implementation

end.
