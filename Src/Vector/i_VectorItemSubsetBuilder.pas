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

unit i_VectorItemSubsetBuilder;

interface

uses
  i_VectorDataItemSimple,
  i_VectorItemSubset;

type
  IVectorItemSubsetBuilder = interface
    ['{2D06DA41-7DEB-4D01-9478-FEE9E9EF19AC}']
    procedure Clear;
    function Add(const AItem: IVectorDataItem): Integer;

    function GetItem(AIndex: Integer): IVectorDataItem;
    property Items[Index: Integer]: IVectorDataItem read GetItem; default;

    function GetCapacity: Integer;
    procedure SetCapacity(ANewCapacity: Integer);
    property Capacity: Integer read GetCapacity write SetCapacity;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    procedure RemoveDuplicates;

    function MakeStaticAndClear: IVectorItemSubset;
    function MakeStaticCopy: IVectorItemSubset;
  end;

  IVectorItemSubsetBuilderFactory = interface
    ['{6322B1BA-E3E4-4D31-A20A-B27BC6174BCC}']
    function Build: IVectorItemSubsetBuilder;
  end;

implementation

end.
