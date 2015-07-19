{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit i_MarkSystemImplORMClientProvider;

interface

uses
  SynDB,
  SynMongoDB,
  mORMotSQLite3,
  t_MarkSystemORM;

type
  TMarkSystemImplORMClientType = (ctSQLite3, ctMongoDB, ctZeosDBMS);

  IMarkSystemImplORMClientProvider = interface
    ['{0A4FAB10-FA04-4C52-8E3A-9CD1FCA99727}']
    function GetUserID: TID;
    property UserID: TID read GetUserID;

    function GetRestClientType: TMarkSystemImplORMClientType;
    property RestClientType: TMarkSystemImplORMClientType read GetRestClientType;

    function GetRestClient: TSQLRestClientDB;
    property RestClient: TSQLRestClientDB read GetRestClient;

    function GetMongoDatabase: TMongoDatabase;
    property MongoDatabase: TMongoDatabase read GetMongoDatabase;

    function GetDBMSProps: TSQLDBConnectionPropertiesThreadSafe;
    property DBMSProperties: TSQLDBConnectionPropertiesThreadSafe read GetDBMSProps;
  end;

implementation

end.
