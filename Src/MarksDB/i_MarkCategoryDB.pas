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

unit i_MarkCategoryDB;

interface

uses
  i_Notifier,
  i_MarkCategory,
  i_MarkCategoryList,
  i_MarkCategoryTree,
  i_MarkCategoryFactory;

type
  IMarkCategoryDB = interface
    ['{F418B319-3B89-4B09-BC9E-0E4FC684BADF}']
    function GetFirstCategoryByName(const AName: string): IMarkCategory;
    function GetCategoryByNameCount(const AName: string): Integer;
    function GetCategoryWithSubCategories(const ACategory: IMarkCategory): IMarkCategoryList;
    function GetSubCategoryListForCategory(const ACategory: IMarkCategory): IMarkCategoryList;
    function GetCategoriesList: IMarkCategoryList;
    function GetVisibleCategories(AZoom: Byte): IMarkCategoryList;
    function GetVisibleCategoriesIgnoreZoom: IMarkCategoryList;
    function CategoryListToStaticTree(const AList: IMarkCategoryList): IMarkCategoryTree;
    function FilterVisibleCategories(const ASourceList: IMarkCategoryList): IMarkCategoryList;

    function UpdateCategory(
      const AOldCategory: IMarkCategory;
      const ANewCategory: IMarkCategory
    ): IMarkCategory;
    function UpdateCategoryList(
      const AOldCategory: IMarkCategoryList;
      const ANewCategory: IMarkCategoryList
    ): IMarkCategoryList;

    procedure SetAllCategoriesVisible(ANewVisible: Boolean);

    function GetFactory: IMarkCategoryFactory;
    property Factory: IMarkCategoryFactory read GetFactory;

    function GetChangeNotifier: INotifier;
    property ChangeNotifier: INotifier read GetChangeNotifier;
  end;

implementation

end.
