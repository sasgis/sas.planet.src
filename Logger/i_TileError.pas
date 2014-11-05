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

unit i_TileError;

interface

uses
  Types;

type
  ITileErrorInfo = interface
    ['{35CA6508-14F7-43D0-BA19-C6FE088936FB}']
    function GetMapTypeGUID: TGUID;
    property MapTypeGUID: TGUID read GetMapTypeGUID;

    function GetZoom: Byte;
    property Zoom: Byte read GetZoom;

    function GetTile: TPoint;
    property Tile: TPoint read GetTile;

    function GetErrorText: string;
    property ErrorText: string read GetErrorText;
  end;

  ITileErrorLogger = interface
    ['{693E489D-A78E-42B8-87FB-F16EF5F53ACF}']
    procedure LogError(const AValue: ITileErrorInfo);
  end;

implementation

end.
