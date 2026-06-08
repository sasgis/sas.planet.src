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

unit i_MarkSystemImplORMClientProvider;

interface

uses
  mormot.orm.core,
  mormot.rest.sqlite3,
  t_MarkSystemORM;

type
  TMarkSystemImplORMClientType = (ctSQLite3, ctMongoDB, ctZDBC, ctODBC);

  TTransactionRec = record
    FSessionID: Cardinal;
    FIsInternal: Boolean;
    FIsReadOnly: Boolean;
  end;

  IMarkSystemORMTransaction = interface
    ['{70601050-4E0B-4664-9FF8-4343BC448519}']
    function Start(const AOrmClass: TOrmClass; const AIsReadOnly: Boolean): TTransactionRec;
    procedure Commit(var ATrans: TTransactionRec);
    procedure RollBack(var ATrans: TTransactionRec);
  end;

  IMarkSystemImplORMClientProvider = interface
    ['{0A4FAB10-FA04-4C52-8E3A-9CD1FCA99727}']
    function GetUserID: TID;
    property UserID: TID read GetUserID;

    function GetRestClientType: TMarkSystemImplORMClientType;
    property RestClientType: TMarkSystemImplORMClientType read GetRestClientType;

    function GetRestClient: TRestClientDB;
    property RestClient: TRestClientDB read GetRestClient;

    function GetTransaction: IMarkSystemORMTransaction;
    property Transaction: IMarkSystemORMTransaction read GetTransaction;
  end;

implementation

end.
