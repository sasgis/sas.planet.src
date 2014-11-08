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

unit i_DownloadUIConfig;

interface

uses
  t_CommonTypes,
  i_ThreadConfig,
  i_ConfigDataElement;

type
  IDownloadUIConfig = interface(IConfigDataElement)
    ['{CED08DA4-F287-49A5-9FF2-C7959F1712F5}']
    function GetUseDownload: TTileSource;
    procedure SetUseDownload(const AValue: TTileSource);
    property UseDownload: TTileSource read GetUseDownload write SetUseDownload;

    function GetTileMaxAgeInInternet: TDateTime;
    procedure SetTileMaxAgeInInternet(const AValue: TDateTime);
    property TileMaxAgeInInternet: TDateTime read GetTileMaxAgeInInternet write SetTileMaxAgeInInternet;

    function GetTilesOut: Integer;
    procedure SetTilesOut(const AValue: Integer);
    property TilesOut: Integer read GetTilesOut write SetTilesOut;

    function GetMapUiRequestCount: Integer;
    procedure SetMapUiRequestCount(const AValue: Integer);
    property MapUiRequestCount: Integer read GetMapUiRequestCount write SetMapUiRequestCount;

    function GetThreadConfig: IThreadConfig;
    property ThreadConfig: IThreadConfig read GetThreadConfig;
  end;


implementation

end.
