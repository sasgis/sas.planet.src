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

unit i_ProjectionSetFactory;

interface

uses
  i_ProjectionSet,
  i_ConfigDataProvider;

type
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
