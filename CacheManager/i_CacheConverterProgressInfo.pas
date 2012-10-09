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

unit i_CacheConverterProgressInfo;

interface

type
  ICacheConverterProgressInfo = interface
    ['{B5F0E45C-D55B-45D8-A4A5-72630ED002B1}']
    function GetTilesProcessed: Int64;
    procedure SetTilesProcessed(const AValue: Int64);
    property TilesProcessed: Int64 read GetTilesProcessed write SetTilesProcessed;

    function GetTilesSkipped: Int64;
    procedure SetTilesSkipped(const AValue: Int64);
    property TilesSkipped: Int64 read GetTilesSkipped write SetTilesSkipped;

    function GetTilesSize: Int64;
    procedure SetTilesSize(const AValue: Int64);
    property TilesSize: Int64 read GetTilesSize write SetTilesSize;

    function GetLastTileName: string;
    procedure SetLastTileName(const AValue: string);
    property LastTileName: string read GetLastTileName write SetLastTileName;

    function GetIsFinished: Boolean;
    procedure SetIsFinished(const AValue: Boolean);
    property Finished: Boolean read GetIsFinished write SetIsFinished;

    function GetProgressAbortErrorStr: string;
    procedure SetProgressAbortErrorStr(const AValue: string);
    property ProgressAbortErrorStr: string read GetProgressAbortErrorStr write SetProgressAbortErrorStr;
  end;

implementation

end.
