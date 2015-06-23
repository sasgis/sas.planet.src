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

unit i_MarkCategoryDbInternalORM;

interface

uses
  t_MarkSystemORM,
  i_Category,
  i_MarkCategory,
  i_NotifierOperation;

type
  IMarkCategoryDbInternalORM = interface
    ['{B68D1FBF-370E-4B58-84ED-F7BEAE2E2BE7}']
    function IsCategoryFromThisDb(const ACategory: ICategory): Boolean;
    function GetCategoryByID(const ID: TID): IMarkCategory;
    function GetCategoryByName(const AName: string): IMarkCategory;
  end;

implementation

end.
