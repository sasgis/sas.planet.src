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

unit i_MapTypeSetBuilder;

interface

uses
  i_MapTypes,
  i_MapTypeSet;

type
  IMapTypeSetBuilder = interface
    ['{B41BE8B9-B70A-4E7D-B462-DA31513DB13A}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetCapacity: Integer;
    procedure SetCapacity(ANewCapacity: Integer);
    property Capacity: Integer read GetCapacity write SetCapacity;

    procedure Add(const AItem: IMapType);
    procedure Clear;
    function MakeCopy: IMapTypeSet;
    function MakeAndClear: IMapTypeSet;
  end;

  IMapTypeSetBuilderFactory = interface
    ['{C5573186-2284-470D-B617-30F4C22898FF}']
    function Build(const AAllowNil: Boolean): IMapTypeSetBuilder;
  end;

implementation

end.
