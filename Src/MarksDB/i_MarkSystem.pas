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

unit i_MarkSystem;

interface

uses
  i_ReadWriteState,
  i_ImportConfig,
  i_InterfaceListStatic,
  i_VectorItemTree,
  i_VectorDataItemSimple,
  i_MarkCategory,
  i_MarkCategoryTree,
  i_MarkDb,
  i_MarkCategoryDB;

type
  IMarkSystem = interface
    ['{E974C3C0-499C-4BB0-B82E-34D39AFCBA9F}']
    function GetState: IReadWriteStateChangeble;
    property State: IReadWriteStateChangeble read GetState;

    function GetMarkDb: IMarkDb;
    property MarkDb: IMarkDb read GetMarkDb;

    function GetCategoryDB: IMarkCategoryDB;
    property CategoryDB: IMarkCategoryDB read GetCategoryDB;

    function GetStringIdByMark(const AMark: IVectorDataItem): string;
    function GetMarkByStringId(const AId: string): IVectorDataItem;
    function GetMarkCategoryByStringId(const AId: string): IMarkCategory;

    function ImportItemsTree(
      const ADataItemTree: IVectorItemTree;
      const AImportConfig: IImportConfig
    ): IInterfaceListStatic;

    procedure DeleteCategoryWithMarks(const ACategory: IMarkCategory);

    function CategoryTreeToMarkTree(
      const ACategoryTree: IMarkCategoryTree;
      const AIncludeHiddenMarks: Boolean
    ): IVectorItemTree;
  end;

implementation

end.
