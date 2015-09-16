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

unit i_CoordConverterFactory;

interface

uses
  i_Datum,
  i_ProjectionType,
  i_ProjectionSet,
  i_ConfigDataProvider;

type
  IDatumFactory = interface
    ['{6F33BA85-B340-44BE-BCCE-049216B2C472}']
    function GetByCode(ADatumEPSG: Integer): IDatum;
    function GetByRadius(const ARadiusA, ARadiusB: Double): IDatum;
  end;

  IProjectionTypeFactory = interface
    ['{B7FCF190-0A79-493D-9A39-A87DE2676236}']
    function GetByConfig(const AConfig: IConfigDataProvider): IProjectionType;
    function GetByCode(
      AProjectionEPSG: Integer
    ): IProjectionType;
  end;

  IProjectionSetFactory = interface
    ['{F7D31D52-79A1-4764-95E5-9653F4EDBD26}']
    function GetProjectionSetByConfig(const AConfig: IConfigDataProvider): IProjectionSet;
    function GetProjectionSetByCode(
      AProjectionEPSG: Integer;
      ATileSplitCode: Integer
    ): IProjectionSet;
  end;

implementation

end.
