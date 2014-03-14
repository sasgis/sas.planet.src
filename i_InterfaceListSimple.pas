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

unit i_InterfaceListSimple;

interface

uses
  Classes,
  i_InterfaceListStatic;

type
  IInterfaceListSimple = interface
    ['{3B5B128E-0BCB-4DBC-8826-6DEFAC4F3152}']
    procedure Clear;
    procedure Delete(AIndex: Integer);
    procedure Exchange(AIndex1, AIndex2: Integer);
    function First: IInterface;
    function IndexOf(const AItem: IInterface): Integer;
    function Add(const AItem: IInterface): Integer;
    procedure AddList(const AList: IInterfaceList);
    procedure AddListStatic(const AList: IInterfaceListStatic);
    procedure AddListSimple(const AList: IInterfaceListSimple);
    procedure Insert(AIndex: Integer; const AItem: IInterface);
    function Last: IInterface;
    function Remove(const AItem: IInterface): Integer;

    function GetItem(AIndex: Integer): IInterface;
    procedure SetItem(AIndex: Integer; const AItem: IInterface);
    property Items[Index: Integer]: IInterface read GetItem write SetItem; default;

    function GetCapacity: Integer;
    procedure SetCapacity(ANewCapacity: Integer);
    property Capacity: Integer read GetCapacity write SetCapacity;

    function GetCount: Integer;
    procedure SetCount(ANewCount: Integer);
    property Count: Integer read GetCount write SetCount;

    function MakeStaticAndClear: IInterfaceListStatic;
    function MakeStaticCopy: IInterfaceListStatic;
  end;

implementation

end.
