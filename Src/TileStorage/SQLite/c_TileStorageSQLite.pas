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

unit c_TileStorageSQLite;

interface

const
  cSQLiteDBFileExt = '.sqlitedb';

  cStorageSyncInterval = 60000; // 1-2 min after last operation

  c_Log_Init    = 'i';
  c_Log_Delete  = 'd';
  c_Log_Select  = 's';
  c_Log_Replace = 'r';
  c_Log_GetVer  = 'v';
  c_Log_SetVer  = 'w';
  c_Log_GetMap  = 'm';

  cDefaultVersionAsIntValue = 0;
  cDefaultVersionAsStrValue = ''; // empty string

implementation

end.
