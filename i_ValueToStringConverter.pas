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

unit i_ValueToStringConverter;

interface

uses
  t_GeoTypes,
  i_Changeable;

type
  IValueToStringConverter = interface
    ['{9EC20437-48BD-4D18-BF95-D2390C6F26F5}']
    function DataSizeConvert(const ASizeInKb: Double): string;
    function DistConvert(const ADistInMeters: Double): string;
    function DistPerPixelConvert(const ADistPerPixelInMeters: Double): string;
    function AreaConvert(const AAreaInSqm: Double): string;
    function SpeedConvert(const AKmph: Double): string;
    function AltitudeConvert(const AMeters: Double): string;
    function LonLatConvert(const ALonLat: TDoublePoint): string;
    function LonConvert(
      const ALon: Double;
      ACutZero: boolean
    ): string;
    function LatConvert(
      const ALat: Double;
      ACutZero: boolean
    ): string;
  end;

  IValueToStringConverterChangeable = interface(IChangeable)
    ['{3204C996-689A-4F5D-92FE-7E942AF1822A}']
    function GetStatic: IValueToStringConverter;
  end;

implementation

end.
