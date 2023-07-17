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

unit t_CommonTypes;

interface

type
  TTileSource = (tsInternet = 0, tsCache = 1, tsCacheInternet = 2);

  TDistStrFormat = (
    dsfKmAndM,
    dsfKmOrM,

    dsfSimpleKm,
    dsfSimpleMeter,
    dsfSimpleCentimeter,

    dsfSimpleMile,
    dsfSimpleYard,
    dsfSimpleFoot,
    dsfSimpleInch,

    dsfSimpleNauticalMile
  );

  TAreaStrFormat = (asfAuto = 0, asfSqM = 1, asfSqKm = 2, asfHa = 3);

  TStringTypeSupport = (stsAscii = 0, stsAnsi = 1, stsUnicode = 2);
  
implementation

end.
