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

unit i_MarkCategoryDBSmlInternal;

interface

uses
  i_Category,
  i_MarkCategory,
  i_NotifierOperation;

type
  IMarkCategoryDBSmlInternal = interface
    ['{44BC212B-F9A2-4304-9E04-D44D16817445}']
    procedure Initialize(AOperationID: Integer; const ACancelNotifier: INotifierOperation);

    function IsCategoryFromThisDb(const ACategory: ICategory): Boolean;
    function GetCategoryByID(id: integer): IMarkCategory;
    function GetCategoryByName(const AName: string): IMarkCategory;
  end;

implementation

end.
