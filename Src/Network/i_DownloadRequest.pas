{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit i_DownloadRequest;

interface

uses
  i_BinaryData,
  i_InetConfig;

type
  IDownloadRequest = interface
    ['{CE40F570-AB2A-465C-843D-0217CB2CFC47}']
    function GetUrl: AnsiString;
    property Url: AnsiString read GetUrl;

    function GetRequestHeader: AnsiString;
    property RequestHeader: AnsiString read GetRequestHeader;

    function GetInetConfig: IInetConfigStatic;
    property InetConfig: IInetConfigStatic read GetInetConfig;
  end;

  IDownloadPostRequest = interface(IDownloadRequest)
    ['{5AFD72E6-E99C-49B8-8594-13773AB8914A}']
    function GetPostData: IBinaryData;
    property PostData: IBinaryData read GetPostData;
  end;

  IDownloadHeadRequest = interface(IDownloadRequest)
    ['{3CE70650-F3B7-4687-B19D-98A324EB877A}']
  end;

implementation

end.
