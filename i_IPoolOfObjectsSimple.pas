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

unit i_IPoolOfObjectsSimple;

interface

uses
  Types,
  i_OperationNotifier,
  i_PoolElement;

type
  IPoolOfObjectsSimple = interface
    ['{897D0F1B-A25C-4CEB-8CD0-5E96DDD5D543}']
    function TryGetPoolElement(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier
    ): IPoolElement;
    function GetPoolSize: Cardinal;
  end;

implementation

end.
