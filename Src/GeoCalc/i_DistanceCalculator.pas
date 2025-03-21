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

unit i_DistanceCalculator;

interface

type
  IDistanceCalculator = interface
    ['{DF3AFB47-4A41-4460-9021-2849BE6953CF}']
    procedure ComputeFinishPosition(
      const ALat1, ALon1: Double;
      const AInitialBearing: Double;
      const ADistance: Double;
      out ALat2, ALon2: Double
    );

    function ComputeDistance(
      const ALat1, ALon1: Double;
      const ALat2, ALon2: Double;
      out AInitialBearing: Double;
      out AFinalBearing: Double
    ): Double; overload;

    function ComputeDistance(
      const ALat1, ALon1: Double;
      const ALat2, ALon2: Double
    ): Double; overload;
  end;

implementation

end.
