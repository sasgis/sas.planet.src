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

unit i_ProjConverter;

interface

uses
  t_GeoTypes;

type
  IProjConverter = interface
    ['{F51E7967-01AF-40CA-A7A1-9BE0E2CF03AE}']
    function LonLat2XY(const AProjLP: TDoublePoint): TDoublePoint;
    function XY2LonLat(const AProjXY: TDoublePoint): TDoublePoint;
  end;

  IProjConverterFactory = interface
    ['{49DDCC5B-B9D4-471B-8247-4CA183B9C680}']
    function GetByEPSG(const AEPSG: Integer): IProjConverter;
    function GetByInitString(const AArgs: AnsiString): IProjConverter;
  end;

implementation

end.
