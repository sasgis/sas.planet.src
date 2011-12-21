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

unit i_MarksDb;

interface

uses
  Classes,
  t_GeoTypes,
  i_MarkCategory,
  i_MarkFactory,
  i_MarksSimple;

type
  IMarksDbNew = interface
    ['{66181215-0260-42A3-9CEA-549329D85F74}']
    function UpdateMark(AOldMark: IInterface; ANewMark: IMark): IMark;
    function UpdateMarksList(AOldMarkList: IInterfaceList; ANewMarkList: IInterfaceList): IInterfaceList;

    function GetMarkByID(AMarkId: IMarkId): IMark;
    function GetMarkIsNew(AMark: IMark): Boolean;

    function GetAllMarskIdList: IInterfaceList;
    function GetMarskIdListByCategory(ACategory: ICategory): IInterfaceList;

    procedure SetMarkVisibleByID(AMark: IMarkId; AVisible: Boolean);
    function GetMarkVisible(AMark: IMarkId): Boolean; overload;
    function GetMarkVisible(AMark: IMark): Boolean; overload;
    procedure SetAllMarksInCategoryVisible(ACategory: ICategory; ANewVisible: Boolean);

    function GetMarksSubset(ARect: TDoubleRect; ACategoryList: IInterfaceList; AIgnoreVisible: Boolean): IMarksSubset; overload;
    function GetMarksSubset(ARect: TDoubleRect; ACategory: ICategory; AIgnoreVisible: Boolean): IMarksSubset; overload;

    function GetFactory: IMarkFactory;
    property Factory: IMarkFactory read GetFactory;
  end;

  IMarksDb = interface
    ['{0B5DFEC6-E519-4D06-8DBA-2D24E2F9A372}']
    function GetMarkByID(AMarkId: IMarkId): IMark;
    function DeleteMark(AMarkId: IMarkId): Boolean;
    procedure DeleteMarksList(AMarkList: IInterfaceList);
    procedure DeleteMarksByCategoryID(ACategory: ICategory);
    procedure WriteMark(AMark: IMark);
    procedure WriteMarksList(AMarkList: IInterfaceList);
    procedure SetMarkVisibleByID(AMark: IMarkId; AVisible: Boolean);
    function GetMarkVisible(AMark: IMarkId): Boolean; overload;
    function GetMarkVisible(AMark: IMark): Boolean; overload;
    function GetMarkIsNew(AMark: IMark): Boolean;
    function GetAllMarskIdList: IInterfaceList;
    function GetMarskIdListByCategory(ACategory: ICategory): IInterfaceList;

    procedure SetAllMarksInCategoryVisible(ACategory: ICategory; ANewVisible: Boolean);

    function GetMarksSubset(ARect: TDoubleRect; ACategoryList: IInterfaceList; AIgnoreVisible: Boolean): IMarksSubset; overload;
    function GetMarksSubset(ARect: TDoubleRect; ACategory: ICategory; AIgnoreVisible: Boolean): IMarksSubset; overload;

    function GetFactory: IMarkFactory;
    property Factory: IMarkFactory read GetFactory;
  end;

implementation

end.
