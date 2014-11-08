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

unit i_MarkDb;

interface

uses
  i_Notifier,
  t_GeoTypes,
  i_Category,
  i_MarkFactory,
  i_InterfaceListStatic,
  i_VectorItemSubset,
  i_MarkId,
  i_VectorDataItemSimple;

type
  IMarkDb = interface
    ['{66181215-0260-42A3-9CEA-549329D85F74}']
    function GetMarkByName(
      const AName: string;
      const ACategory: ICategory
    ): IVectorDataItem;

    function GetMarkSubsetByCategoryList(
      const ACategoryList: IInterfaceListStatic;
      const AIncludeHiddenMarks: Boolean
    ): IVectorItemSubset;
    function GetMarkSubsetByCategory(
      const ACategory: ICategory;
      const AIncludeHiddenMarks: Boolean
    ): IVectorItemSubset;
    function GetMarkSubsetByCategoryListInRect(
      const ARect: TDoubleRect;
      const ACategoryList: IInterfaceListStatic;
      const AIncludeHiddenMarks: Boolean
    ): IVectorItemSubset;
    function GetMarkSubsetByCategoryInRect(
      const ARect: TDoubleRect;
      const ACategory: ICategory;
      const AIncludeHiddenMarks: Boolean
    ): IVectorItemSubset;
    function FindMarks(
      const ASearch: string;
      const AMaxCount: Integer;
      const AIncludeHiddenMarks: Boolean;
      const ASearchInDescription: Boolean
    ): IVectorItemSubset;
    function UpdateMark(
      const AOldMark: IVectorDataItem;
      const ANewMark: IVectorDataItem
    ): IVectorDataItem;
    function UpdateMarkList(
      const AOldMarkList: IInterfaceListStatic;
      const ANewMarkList: IInterfaceListStatic
    ): IInterfaceListStatic;

    function GetAllMarkIdList: IInterfaceListStatic;
    function GetMarkIdListByCategory(const ACategory: ICategory): IInterfaceListStatic;

    function GetMarkByID(const AMarkId: IMarkId): IVectorDataItem;

    procedure SetMarkVisibleByID(
      const AMark: IMarkId;
      AVisible: Boolean
    );
    procedure SetMarkVisible(
      const AMark: IVectorDataItem;
      AVisible: Boolean
    );

    procedure SetMarkVisibleByIDList(
      const AMarkList: IInterfaceListStatic;
      AVisible: Boolean
    );
    procedure ToggleMarkVisibleByIDList(const AMarkList: IInterfaceListStatic);

    function GetMarkVisibleByID(const AMark: IMarkId): Boolean;
    function GetMarkVisible(const AMark: IVectorDataItem): Boolean;
    procedure SetAllMarksInCategoryVisible(
      const ACategory: ICategory;
      ANewVisible: Boolean
    );

    function GetFactory: IMarkFactory;
    property Factory: IMarkFactory read GetFactory;

    function GetChangeNotifier: INotifier;
    property ChangeNotifier: INotifier read GetChangeNotifier;
  end;

implementation

end.
