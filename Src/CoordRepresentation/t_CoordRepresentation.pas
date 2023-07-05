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

unit t_CoordRepresentation;

interface

type
  TDegrShowFormat = (
    dshCharDegrMinSec,
    dshCharDegrMin,
    dshCharDegr,
    dshCharDegr2,
    dshSignDegrMinSec,
    dshSignDegrMin,
    dshSignDegr,
    dshSignDegr2
  );

  TCoordSysType = (
    cstWGS84 = 0,   // WGS 84 / Geographic
    cstSK42 = 1,    // SK-42 (Pulkovo-1942) / Geographic
    cstSK42GK = 2,  // SK-42 / Gauss-Kruger zones
    cstUTM = 3      // WGS 84 / UTM zones
  );

  TCoordSysInfoType = (
    csitDontShow = 0,
    csitShowExceptWGS84 = 1,
    csitShowForAll = 2
  );

implementation

end.
