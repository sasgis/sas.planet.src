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

unit i_CoordToStringConverter;

interface

uses
  t_GeoTypes,
  t_CoordRepresentation,
  i_Changeable;

type
  TCoordToStringConverterOption = (
    coCutZero,
    coIncludeZone,
    coLatitudeFirst
  );

  TCoordToStringConverterOptions = set of TCoordToStringConverterOption;

  TCoordPartItem = (
    cpiZone, cpiLon, cpiLat // do not change order
  );

  TCoordPartArray = array[TCoordPartItem] of string;

  ICoordToStringConverter = interface
  ['{0140A97B-47A3-44DE-91D5-9BAA54B34C4C}']
    function GetCoordSysInfo(
      const ALonLat: TDoublePoint
    ): string;

    function LonLatConvert(
      const ALonLat: TDoublePoint;
      const AOptions: TCoordToStringConverterOptions = []
    ): string;

    function LonLatConvertExt(
      const ALonLat: TDoublePoint;
      const AOptions: TCoordToStringConverterOptions = []
    ): TCoordPartArray; overload;

    function LonLatConvertExt(
      const ALonLat: TDoublePoint;
      const ACoordSysType: TCoordSysType;
      const AOptions: TCoordToStringConverterOptions = []
    ): TCoordPartArray; overload;
  end;

  ICoordToStringConverterChangeable = interface(IChangeable)
  ['{BEAE1C2A-2516-43CF-818C-6D7C5B42D788}']
    function GetStatic: ICoordToStringConverter;
  end;

implementation

end.
