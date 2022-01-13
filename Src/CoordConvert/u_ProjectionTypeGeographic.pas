{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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

unit u_ProjectionTypeGeographic;

interface

uses
  t_GeoTypes,
  u_ProjectionTypeBase;

type
  TProjectionTypeGeographic = class(TProjectionTypeBase)
  protected
    function Relative2LonLatInternal(const APoint: TDoublePoint): TDoublePoint; override;
    function LonLat2RelativeInternal(const APoint: TDoublePoint): TDoublePoint; override;
  protected
    procedure ValidateLonLatPos(var APoint: TDoublePoint); override;
    procedure ValidateLonLatRect(var ARect: TDoubleRect); override;
  end;

implementation

const
  CMaxLatitude = 90;

{ TProjectionTypeGeographic }

function TProjectionTypeGeographic.LonLat2RelativeInternal(
  const APoint: TDoublePoint
): TDoublePoint;
begin
  Result.X := (0.5 + APoint.X / 360);
  Result.Y := (0.5 - APoint.Y / 360);
end;

function TProjectionTypeGeographic.Relative2LonLatInternal(
  const APoint: TDoublePoint
): TDoublePoint;
begin
  Result.X :=  (APoint.X - 0.5) * 360;
  Result.Y := -(APoint.Y - 0.5) * 360;
end;

procedure _ValidateLonLatPos(var APoint: TDoublePoint); inline;
begin
  if APoint.X < -180 then begin
    APoint.X := -180;
  end else
  if APoint.X > 180 then begin
    APoint.X := 180;
  end;

  if APoint.Y < -CMaxLatitude then begin
    APoint.Y := -CMaxLatitude;
  end else
  if APoint.Y > CMaxLatitude then begin
    APoint.Y := CMaxLatitude;
  end;
end;

procedure TProjectionTypeGeographic.ValidateLonLatPos(var APoint: TDoublePoint);
begin
  _ValidateLonLatPos(APoint);
end;

procedure TProjectionTypeGeographic.ValidateLonLatRect(var ARect: TDoubleRect);
begin
  _ValidateLonLatPos(ARect.TopLeft);
  _ValidateLonLatPos(ARect.BottomRight);
end;

end.
