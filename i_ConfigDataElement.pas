{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit i_ConfigDataElement;

interface

uses
  i_JclNotify,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider;

type
  IConfigDataElement = interface
    ['{AAD224E2-F566-43CC-BBAF-EF9C175009E7}']
    procedure LockRead;
    procedure LockWrite;
    procedure UnlockRead;
    procedure UnlockWrite;
    procedure ReadConfig(AConfigData: IConfigDataProvider);
    procedure WriteConfig(AConfigData: IConfigDataWriteProvider);
    procedure StopNotify;
    procedure StartNotify;

    function GetBeforeChangeNotifier: IJclNotifier;
    property BeforeChangeNotifier: IJclNotifier read GetBeforeChangeNotifier;

    function GetChangeNotifier: IJclNotifier;
    property ChangeNotifier: IJclNotifier read GetChangeNotifier;

    function GetAfterChangeNotifier: IJclNotifier;
    property AfterChangeNotifier: IJclNotifier read GetAfterChangeNotifier;
  end;

implementation

end.
