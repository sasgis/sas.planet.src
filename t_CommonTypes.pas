{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit t_CommonTypes;

interface

type
  TAccesState = (asUnknown = 0, asEnabled = 1, asDisabled = -1);

  TTileSource = (tsInternet = 0, tsCache = 1, tsCacheInternet = 2);

  { Способ отображения расстояний
  dsfKmAndM - в виде 12 км 299 м
  dsfSimpleKM - в виед 12.299 км
  }
  TDistStrFormat = (dsfKmAndM = 0, dsfSimpleKM = 1);

  TDegrShowFormat = (dshCharDegrMinSec = 0, dshCharDegrMin = 1, dshCharDegr = 2, dshSignDegrMinSec = 3, dshSignDegrMin = 4, dshSignDegr = 5);

  TAreaStrFormat = (asfAuto = 0, asfSqM = 1, asfSqKm = 2, asfHa = 3);

implementation

end.
