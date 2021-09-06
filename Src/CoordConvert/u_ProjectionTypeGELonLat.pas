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

unit u_ProjectionTypeGELonLat;

interface

uses
  t_GeoTypes,
  u_ProjectionTypeBase;

type
  TProjectionTypeGELonLat = class(TProjectionTypeBase)
  protected
    function Relative2LonLatInternal(const APoint: TDoublePoint): TDoublePoint; override;
    function LonLat2RelativeInternal(const APoint: TDoublePoint): TDoublePoint; override;
  end;

implementation


{ TProjectionTypeGELonLat }

function TProjectionTypeGELonLat.LonLat2RelativeInternal(
  const APoint: TDoublePoint
): TDoublePoint;
begin
  Result.X := (0.5 + APoint.X / 360);
  Result.Y := (0.5 - APoint.Y / 360);
end;

function TProjectionTypeGELonLat.Relative2LonLatInternal(
  const APoint: TDoublePoint
): TDoublePoint;
begin
  Result.X :=  (APoint.X - 0.5) * 360;
  Result.Y := -(APoint.Y - 0.5) * 360;
end;
end;

end.
