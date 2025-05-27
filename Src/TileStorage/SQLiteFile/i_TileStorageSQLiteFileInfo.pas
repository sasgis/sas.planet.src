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

unit i_TileStorageSQLiteFileInfo;

interface

type
  ITileStorageSQLiteFileInfo = interface
    ['{1F8688DE-A3F5-41DA-808E-4E1767CAC816}']
    function GetFileName: string;
    property FileName: string read GetFileName;

    function GetFileDate: TDateTime;
    property FileDate: TDateTime read GetFileDate;

    procedure AddOrSetMetadataValue(const AKey, AValue: string);
    function TryGetMetadataValue(const AKey: string; var AValue: string): Boolean;
  end;

implementation

end.
