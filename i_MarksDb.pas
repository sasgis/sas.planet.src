{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
  IMarksDb = interface
    ['{66181215-0260-42A3-9CEA-549329D85F74}']
    function UpdateMark(
      const AOldMark: IInterface;
      const ANewMark: IMark
    ): IMark;
    function UpdateMarksList(
      const AOldMarkList: IInterfaceList;
      const ANewMarkList: IInterfaceList
    ): IInterfaceList;

    function GetMarkByID(const AMarkId: IMarkId): IMark;
    function GetMarkIsNew(const AMark: IMark): Boolean;

    function GetAllMarskIdList: IInterfaceList;
    function GetMarskIdListByCategory(const ACategory: ICategory): IInterfaceList;

    procedure SetMarkVisibleByID(
      const AMark: IMarkId;
      AVisible: Boolean
    );
    function GetMarkVisible(const AMark: IMarkId): Boolean; overload;
    function GetMarkVisible(const AMark: IMark): Boolean; overload;
    procedure SetAllMarksInCategoryVisible(
      const ACategory: ICategory;
      ANewVisible: Boolean
    );

    function GetMarksSubset(
      const ARect: TDoubleRect;
      const ACategoryList: IInterfaceList;
      AIgnoreVisible: Boolean
    ): IMarksSubset; overload;
    function GetMarksSubset(
      const ARect: TDoubleRect;
      const ACategory: ICategory;
      AIgnoreVisible: Boolean
    ): IMarksSubset; overload;

    function GetFactory: IMarkFactory;
    property Factory: IMarkFactory read GetFactory;
  end;

implementation

end.
