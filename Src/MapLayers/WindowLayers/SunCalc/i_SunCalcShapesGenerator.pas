{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2017, SAS.Planet development team.                      *}
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

unit i_SunCalcShapesGenerator;

interface

uses
  GR32,
  t_GeoTypes;

type
  ISunCalcShapesGenerator = interface
    ['{2B7E3ECE-89D4-4B9B-967A-64BF08444C77}']
    procedure ValidateCache;

    function IsIntersectScreenRect: Boolean;
    
    procedure SetLocation(const AValue: TDoublePoint);
    procedure SetDateTime(const AValue: TDateTime);

    procedure GetCirclePoints(
      out ACirclePoints: TArrayOfFixedPoint
    );

    procedure GetYearInfoPoints(
      out ALongestDayPoints: TArrayOfFixedPoint;
      out AShortestDayPoints: TArrayOfFixedPoint;
      out ASunlightFillPoints: TArrayOfFixedPoint
    );

    procedure GetDayInfoPoints(
      out ADayPoints: TArrayOfFixedPoint;
      out ASunrise: TFixedPoint;
      out ASunset: TFixedPoint;
      out ACenter: TFixedPoint
    );

    procedure GetTimeInfoPoints(
      out ASunPos: TFixedPoint;
      out ACenter: TFixedPoint
    );
  end;

implementation

end.
