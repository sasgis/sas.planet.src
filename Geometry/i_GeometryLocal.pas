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

unit i_GeometryLocal;

interface

uses
  t_GeoTypes,
  i_EnumDoublePoint,
  i_LocalCoordConverter;

type
  IGeometryLocal = interface
    ['{C9B8B666-3B15-4A97-BA95-172D53916742}']
    function GetLocalConverter: ILocalCoordConverter;
    property LocalConverter: ILocalCoordConverter read GetLocalConverter;
  end;

  IGeometryLocalLine = interface(IGeometryLocal)
    ['{2DB59206-AD9C-47FD-B1CF-329579BEE20B}']
    function GetEnum: IEnumLocalPoint;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetPoints: PDoublePointArray;
    property Points: PDoublePointArray read GetPoints;
  end;

  IGeometryLocalPolygon = interface(IGeometryLocal)
    ['{0716D252-1516-440B-AD80-826C60AAC063}']
    function GetEnum: IEnumLocalPoint;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetPoints: PDoublePointArray;
    property Points: PDoublePointArray read GetPoints;
  end;

  IGeometryLocalMultiLine = interface(IGeometryLocal)
    ['{C5B0DB77-DC25-4802-BB90-F0FE90DC1DFC}']
    function GetEnum: IEnumLocalPoint;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(AIndex: Integer): IGeometryLocalLine;
    property Item[AIndex: Integer]: IGeometryLocalLine read GetItem;
  end;

  IGeometryLocalMultiPolygon = interface(IGeometryLocal)
    ['{86702869-BE39-41C8-8373-A7C19E20ED7B}']
    function GetEnum: IEnumLocalPoint;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(AIndex: Integer): IGeometryLocalPolygon;
    property Item[AIndex: Integer]: IGeometryLocalPolygon read GetItem;
  end;

implementation

end.
