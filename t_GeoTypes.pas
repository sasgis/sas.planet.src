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

unit t_GeoTypes;

interface

uses
  vsagps_public_point,
  Types;

type
  TPointRounding = (prClosest, prToTopLeft, prToBottomRight);
  TRectRounding = (rrClosest, rrOutside, rrInside, rrToTopLeft);

  TDoublePoint = vsagps_public_point.TDoublePoint;

  TDoubleRect = packed record
    case Integer of
      0: (Left, Top: Double;
        Right, Bottom: Double);
      1: (TopLeft, BottomRight: TDoublePoint);
  end;


  PPointArray = ^TPointArray;
  TPointArray = array [0..0] of TPoint;
  PArrayOfPoint = ^TArrayOfPoint;
  TArrayOfPoint = array of TPoint;

  PDoublePointArray = ^TDoublePointArray;
  TDoublePointArray = array [0..0] of TDoublePoint;

// Скопировал из ECWReader что бы не добавлять лишние зависимости от того юнита.
{$MINENUMSIZE 4}
type
  TCellSizeUnits =
    (
    // Invalid cell units
    CELL_UNITS_INVALID = 0,
    // Cell units are standard meters
    CELL_UNITS_METERS = 1,
    // Degrees
    CELL_UNITS_DEGREES = 2,
    // US Survey feet
    CELL_UNITS_FEET = 3,
    // Unknown cell units
    CELL_UNITS_UNKNOWN = 4
    );

implementation

end.
