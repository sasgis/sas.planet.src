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

unit i_DoublePointFilter;

interface

uses
  i_EnumDoublePoint;

type
  IDoublePointFilter = interface
    ['{451EB2BA-2201-4585-B850-8C36584A1CBD}']
    function CreateFilteredEnum(const ASource: IEnumDoublePoint): IEnumDoublePoint;
  end;

  ILonLatPointFilter = interface
    ['{04D0479B-F80F-4B6C-9932-3B00F5CF30AA}']
    function CreateFilteredEnum(const ASource: IEnumLonLatPoint): IEnumLonLatPoint;
  end;

  ILonLatPointConverter = interface
    ['{6BBEF425-25B7-447E-B3D1-BCDDE461DE8A}']
    function CreateFilteredEnum(const ASource: IEnumLonLatPoint): IEnumProjectedPoint;
  end;

  IProjectedPointFilter = interface
    ['{9AC5D0F8-F390-4804-AD75-B507B39904E7}']
    function CreateFilteredEnum(const ASource: IEnumProjectedPoint): IEnumProjectedPoint;
  end;

  IProjectedPointConverter = interface
    ['{0CD429D3-D095-4015-9B5B-9107FF14FEF0}']
    function CreateFilteredEnum(const ASource: IEnumProjectedPoint): IEnumLocalPoint;
  end;

  ILocalPointFilter = interface
    ['{B70EE6B7-D75D-420E-B90F-A0C9AD1DD41A}']
    function CreateFilteredEnum(const ASource: IEnumLocalPoint): IEnumLocalPoint;
  end;

implementation

end.
