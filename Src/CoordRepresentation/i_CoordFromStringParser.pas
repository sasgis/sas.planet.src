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

unit i_CoordFromStringParser;

interface

uses
  t_GeoTypes;

type
  ICoordFromStringParser = interface
    ['{B5EE19FE-DBBE-458B-B933-2B5A6EAFC51B}']
    function TryStrToCoord(
      const ALon: string;
      const ALat: string;
      out ACoord: TDoublePoint
    ): Boolean; overload;

    function TryStrToCoord(
      const AX: string;
      const AY: string;
      const AZone: string;
      out ACoord: TDoublePoint
    ): Boolean; overload;

    function TryStrToCoord(
      const AStr: string;
      out ACoord: TDoublePoint
    ): Boolean; overload;
  end;

implementation

end.
