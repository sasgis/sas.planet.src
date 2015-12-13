{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit i_CoordToStringConverter;

interface

uses
  t_GeoTypes,
  i_Changeable;

type
  ICoordToStringConverter = interface
  ['{0140A97B-47A3-44DE-91D5-9BAA54B34C4C}']
    function LonLatConvert(
      const ALonLat: TDoublePoint
    ): string;

    function LonConvert(
      const ALon: Double;
      const ACutZero: Boolean
    ): string;

    function LatConvert(
      const ALat: Double;
      const ACutZero: Boolean
    ): string;
  end;

  ICoordToStringConverterChangeable = interface(IChangeable)
  ['{BEAE1C2A-2516-43CF-818C-6D7C5B42D788}']
    function GetStatic: ICoordToStringConverter;
  end;

implementation

end.
