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

unit i_VectorDataItemSimple;

interface

uses
  t_GeoTypes;

type
  IVectorDataItemSimple =  interface
    ['{1242B43D-C878-4AC9-9F29-0A3E258F4670}']
    function GetName: string;
    property Name: string read GetName;

    function GetDesc: string;
    property Desc: string read GetDesc;

    function GetLLRect: TDoubleRect;
    property LLRect: TDoubleRect read GetLLRect;

    function GetPoints: TArrayOfDoublePoint;
    property Points: TArrayOfDoublePoint read GetPoints;

    function GetHintText: string;
    function GetHintTextWithoutDesc: string;
    function GetInfoHTML: string;
    function GetInfoCaption: string;

    function IsPoint: Boolean;
    function IsLine: Boolean;
    function IsPoly: Boolean;
  end;

  IVectorDataItemList = interface
    ['{E2BF9449-23E0-41C2-9824-DCAF226712D8}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(AIndex: Integer): IVectorDataItemSimple;
  end;

implementation

end.
