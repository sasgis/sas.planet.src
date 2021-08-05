{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2021, SAS.Planet development team.                      *}
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

unit i_SunCalcShapesGenerator;

interface

uses
  GR32,
  t_GeoTypes,
  i_SunCalcDataProvider;

type
  ISunCalcShapesGenerator = interface
    ['{2B7E3ECE-89D4-4B9B-967A-64BF08444C77}']
    procedure ValidateCache;

    function IsIntersectScreenRect: Boolean;
    
    procedure SetLocation(
      const ALonLat: TDoublePoint
    );

    procedure SetDateTime(
      const AUtcDateTime: TDateTime;
      const AUtcOffset: Double
    );

    procedure SetDataProvider(
      const AProvider: ISunCalcDataProvider
    );

    procedure GetCirclePoints(
      out ACirclePoints: TArrayOfFloatPoint
    );

    procedure GetMinMaxAltitudePoints(
      out AMinAltitudePoints: TArrayOfFloatPoint;
      out AMaxAltitudePoints: TArrayOfFloatPoint;
      out AMinMaxAltitudePolygon: TArrayOfFloatPoint
    );

    procedure GetDayInfoPoints(
      out ADayPoints: TArrayOfArrayOfFloatPoint;
      out ARise: TFloatPoint;
      out ASet: TFloatPoint;
      out ACenter: TFloatPoint
    );

    procedure GetTimeInfoPoints(
      out APos: TFloatPoint;
      out ACenter: TFloatPoint
    );
  end;

implementation

end.
