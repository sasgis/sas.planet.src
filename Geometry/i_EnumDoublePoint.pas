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

unit i_EnumDoublePoint;

interface

uses
  t_GeoTypes;

type
  IEnumDoublePoint = interface
    ['{A821C4B3-DB65-4B93-94A2-19ADC919EDCC}']
    function Next(out APoint: TDoublePoint): Boolean;
  end;

  IEnumLonLatPoint = interface(IEnumDoublePoint)
    ['{E8365B09-9819-4372-B24F-65BBFDC84558}']
  end;

  IEnumProjectedPoint = interface(IEnumDoublePoint)
    ['{BC88EBFF-54FA-4322-BB2A-0845A5943804}']
  end;

  IEnumLocalPoint = interface(IEnumDoublePoint)
    ['{70250A89-1BA1-45BA-8A33-0FE97E714771}']
  end;

implementation

end.
