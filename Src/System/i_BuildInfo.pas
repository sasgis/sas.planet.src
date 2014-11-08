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

unit i_BuildInfo;

interface

type
  IBuildInfo = interface
    ['{541D6359-3C4F-44D5-9BC0-CA12CAE45220}']
    function GetVersion: string;
    function GetVersionDetaled: string;
    function GetBuildDate: TDateTime;
    function GetBuildType: string;
    function GetBuildSrcInfo(out ARev: Integer; out ANode: string): Boolean;
    function GetBuildReqInfo(out ARev: Integer; out ANode: string): Boolean;
    function GetCompilerInfo: string;
    function GetDescription: string;
  end;

implementation

end.
