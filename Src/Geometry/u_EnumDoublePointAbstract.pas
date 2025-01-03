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

unit u_EnumDoublePointAbstract;

interface

uses
  t_GeoTypes,
  i_EnumDoublePoint,
  u_BaseInterfacedObject;

type
  TEnumDoublePointAbstract = class(TBaseInterfacedObject, IEnumDoublePoint)
  protected
    { IEnumDoublePoint }
    function Next(
      out APoint: TDoublePoint
    ): Boolean; overload; virtual; abstract;

    function Next(
      out APoint: TDoublePoint;
      out AMeta: TDoublePointsMetaItem
    ): Boolean; overload; virtual;
  end;

implementation

uses
  u_DoublePointsMetaFunc;

{ TEnumDoublePointAbstract }

function TEnumDoublePointAbstract.Next(
  out APoint: TDoublePoint;
  out AMeta: TDoublePointsMetaItem
): Boolean;
begin
  Assert(False, 'Not implemented!');

  Result := Next(APoint);

  if Result then begin
    ResetMetaItem(@AMeta);
  end;
end;

end.
