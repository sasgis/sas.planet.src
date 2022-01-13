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

unit i_TileStorageSQLiteHolder;

interface

uses
  i_ContentTypeInfo,
  i_MapVersionInfo;

type
  TSetSQLiteExecProc = procedure (const ASQLStatement: AnsiString) of object;

  ITileStorageSQLiteHolder = interface
    ['{BCDBCE62-2C15-4296-AB26-E385BE4D4BC3}']
    procedure LogError(
      const ACmd: AnsiChar;
      const AMsg: String;
      const ARaiseError: Boolean
    );

    // execute statements (set params or make tables)
    procedure ExecMakeSession(const AExecProc: TSetSQLiteExecProc);
    procedure ExecForNewTable(const AExecProc: TSetSQLiteExecProc);
    procedure ExecEstablished(const AExecProc: TSetSQLiteExecProc);

    // contenttypes
    function GetContentTypeToDB(const AContentType: IContentTypeInfoBasic): AnsiString;
    function GetContentTypeInfo(const AContentTypeFromDB: AnsiString): IContentTypeInfoBasic;

    // get version
    function GetVersionInfo(const AVersionStr: String): IMapVersionInfo;
  end;

implementation

end.
