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

unit i_MarkSystemImpl;

interface

uses
  i_ReadWriteState,
  i_VectorDataItemSimple,
  i_MarkCategory,
  i_MarkDbImpl,
  i_MarkCategoryDBImpl;

type
  IMarkSystemImpl = interface
    ['{E974C3C0-499C-4BB0-B82E-34D39AFCBA9F}']
    function GetState: IReadWriteStateChangeble;
    property State: IReadWriteStateChangeble read GetState;

    function GetMarkDb: IMarkDbImpl;
    property MarkDb: IMarkDbImpl read GetMarkDb;

    function GetCategoryDB: IMarkCategoryDBImpl;
    property CategoryDB: IMarkCategoryDBImpl read GetCategoryDB;

    function GetStringIdByMark(const AMark: IVectorDataItemSimple): string;
    function GetMarkByStringId(const AId: string): IVectorDataItemSimple;
    function GetMarkCategoryByStringId(const AId: string): IMarkCategory;
  end;

implementation

end.
