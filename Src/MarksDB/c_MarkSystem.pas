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

unit c_MarkSystem;

interface

const
  cSMLMarksDbGUID: TGUID = '{F5A9FB07-FF9B-4B4A-98D3-AB8C5F24F36B}';
  cORMSQLiteMarksDbGUID: TGUID = '{776BC34B-7A4B-4237-BE9A-D905228FDA1A}';
  cORMMongoDbMarksDbGUID: TGUID = '{AA5DF0DA-C785-4C1F-ADF5-2FC90146BE5B}';
  cORMODBCMarksDbGUID: TGUID = '{03578F25-094A-4C2F-8609-5D7E76699BA3}';
  cORMZDBCMarksDbGUID: TGUID = '{4D7A6F63-412E-46E3-AFD0-4C0E8B9AF2C5}';

const
  cSMLMarksDbDefFileName = 'marks';
  cSMLMarksDbFileExt = '.sml';

  cORMSQLiteMarksDbDefFileName = 'Marks';
  cORMSQLiteMarksDbFileExt = '.db3';

resourcestring
  rsSMLMarksDbName = 'SML';
  rsORMSQLiteMarksDbName = 'SQLite3';
  rsORMMongoDbMarksDbName = 'MongoDB';
  rsORMODBCMarksDbName = 'DBMS (ODBC driver)';
  rsORMZDBCMarksDbName = 'DBMS (Native driver)';

implementation

end.
