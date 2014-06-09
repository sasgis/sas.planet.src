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

unit i_MarkId;

interface

uses
  i_Category,
  i_VectorDataItemSimple;

type
  TMarkIdType = (midPoint = 0, midLine, midPoly);

  IMarkId = interface
    ['{A3FE0170-8D32-4777-A3EA-53D678875B7B}']
    function GetCategory: ICategory;
    property Category: ICategory read GetCategory;

    function GetName: string;
    property Name: string read GetName;

    function GetMarkType: TMarkIdType;
    property MarkType: TMarkIdType read GetMarkType;

    function IsSameId(const AMarkId: IMarkId): Boolean;
    function IsSameMark(const AMark: IVectorDataItem): Boolean;
  end;

implementation

end.
