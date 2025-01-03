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

unit t_CoordRepresentation;

interface

type
  TGeogCoordShowFormat = (
    dshCharDegrMinSec,
    dshCharDegrMin,
    dshCharDegr,
    dshCharDegr2,
    dshSignDegrMinSec,
    dshSignDegrMin,
    dshSignDegr,
    dshSignDegr2
  );

  TProjCoordShowFormat = (
    psfRoundedToWhole,
    psfRoundedToTenth,
    psfRoundedToHundredths,
    psfRoundedToThousandths
  );

  TMgrsCoordShowFormat = (
    msfSplitted,
    msfJoined
  );

  TCoordSysType = (
    cstWGS84,    // WGS 84 / Geographic
    cstUTM,      // WGS 84 / UTM zones
    cstMGRS,     // WGS 84 / MGRS

    cstSK42,     // SK-42 / Geographic
    cstSK42GK,   // SK-42 / Gauss-Kruger zones

    cstGSK2011,  // GSK-2011 / Geographic
    cstGSK2011GK // GSK-2011 / Gauss-Kruger zones
  );

  TCoordSysInfoType = (
    csitDontShow = 0,
    csitShowExceptWGS84 = 1,
    csitShowForAll = 2
  );

implementation

end.
