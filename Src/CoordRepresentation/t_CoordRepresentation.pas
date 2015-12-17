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

unit t_CoordRepresentation;

interface

type
  TDegrShowFormat = (
    dshCharDegrMinSec = 0,
    dshCharDegrMin = 1,
    dshCharDegr = 2,
    dshSignDegrMinSec = 3,
    dshSignDegrMin = 4,
    dshSignDegr = 5
  );

  TCoordSysType = (
    cstWGS84 = 0,
    cstSK42 = 1,    // SK42 (Pulkovo-1942)
    cstSK42GK = 2   // SK42 in Gauss-Kruger projection
  );

implementation

end.
