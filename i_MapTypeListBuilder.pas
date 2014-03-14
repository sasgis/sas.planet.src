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

unit i_MapTypeListBuilder;

interface

uses
  i_MapTypes,
  i_MapTypeListStatic;

type
  IMapTypeListBuilder = interface
    ['{9349108A-1B0F-4920-B273-32710FE6659B}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetCapacity: Integer;
    procedure SetCapacity(ANewCapacity: Integer);
    property Capacity: Integer read GetCapacity write SetCapacity;

    function GetItem(AIndex: Integer): IMapType;
    procedure SetItem(AIndex: Integer; const AItem: IMapType);
    property Items[Index: Integer]: IMapType read GetItem write SetItem; default;

    procedure Add(const AItem: IMapType);
    procedure Clear;
    procedure Delete(AIndex: Integer);
    procedure Exchange(AIndex1, AIndex2: Integer);
    function MakeCopy: IMapTypeListStatic;
    function MakeAndClear: IMapTypeListStatic;
  end;

  IMapTypeListBuilderFactory = interface
    ['{C5573186-2284-470D-B617-30F4C22898FF}']
    function Build: IMapTypeListBuilder;
  end;

implementation

end.
