{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2013, SAS.Planet development team.                      *}
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

unit i_BerkeleyDBEnv;

interface

type
  PBerkeleyTxn = Pointer;

  IBerkeleyDBEnvironment = interface
    ['{0D73208B-3729-43F3-9AAE-DF1616107648}']
    function GetEnvironmentPointerForApi: Pointer;
    property dbenv: Pointer read GetEnvironmentPointerForApi;

    function GetRootPath: string;
    property RootPath: string read GetRootPath;

    function GetClientsCount: Integer;
    procedure SetClientsCount(const AValue: Integer);
    property ClientsCount: Integer read GetClientsCount write SetClientsCount;

    procedure TransactionBegin(out ATxn: PBerkeleyTxn);
    procedure TransactionCommit(var ATxn: PBerkeleyTxn);
    procedure TransactionAbort(var ATxn: PBerkeleyTxn);

    procedure Sync;
  end;

implementation

end.
