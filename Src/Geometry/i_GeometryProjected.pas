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

unit i_GeometryProjected;

interface

uses
  t_GeoTypes,
  i_EnumDoublePoint;

type
  IGeometryProjected = interface
    ['{162D40D7-29D5-44B1-BDB8-7E7616289769}']
    function GetBounds: TDoubleRect;
    property Bounds: TDoubleRect read GetBounds;
  end;

  IGeometryProjectedMultiPoint = interface(IGeometryProjected)
    ['{D0241C2A-E725-490C-AADB-C8ED4B585E55}']
    function GetEnum: IEnumProjectedPoint;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetPoints: PDoublePointArray;
    property Points: PDoublePointArray read GetPoints;
  end;

  IGeometryProjectedLine = interface(IGeometryProjected)
    ['{C2726CC7-0661-4116-9D08-B7DBE1D54612}']
    function IsPointOnPath(
      const APoint: TDoublePoint;
      const ADist: Double
    ): Boolean;
    function IsRectIntersectPath(const ARect: TDoubleRect): Boolean;
  end;

  IGeometryProjectedSingleLine = interface(IGeometryProjectedLine)
    ['{0D9B7321-DBA0-494F-959C-5026DB27C681}']
    function GetEnum: IEnumProjectedPoint;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetPoints: PDoublePointArray;
    property Points: PDoublePointArray read GetPoints;
  end;

  IGeometryProjectedMultiLine = interface(IGeometryProjectedLine)
    ['{781FAF61-C109-4CC9-A861-90CBE807D8E1}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(AIndex: Integer): IGeometryProjectedSingleLine;
    property Item[AIndex: Integer]: IGeometryProjectedSingleLine read GetItem;
  end;

  IGeometryProjectedPolygon = interface(IGeometryProjected)
    ['{828A0CAD-B231-46F2-86D8-F10437828179}']
    function IsPointInPolygon(const APoint: TDoublePoint): Boolean;
    function IsPointOnBorder(
      const APoint: TDoublePoint;
      const ADist: Double
    ): Boolean;
    function IsRectIntersectPolygon(const ARect: TDoubleRect): Boolean;
    function IsRectIntersectBorder(const ARect: TDoubleRect): Boolean;
    function CalcArea: Double;
  end;

  IGeometryProjectedContour = interface(IGeometryProjectedPolygon)
    ['{962D2412-3432-4859-8D12-D4D48BACFAAE}']
    function GetEnum: IEnumProjectedPoint;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetPoints: PDoublePointArray;
    property Points: PDoublePointArray read GetPoints;
  end;

  IGeometryProjectedSinglePolygon = interface(IGeometryProjectedPolygon)
    ['{30424113-D148-45EB-A4C8-C0150DB89D22}']
    function GetOuterBorder: IGeometryProjectedContour;
    property OuterBorder: IGeometryProjectedContour read GetOuterBorder;

    function GetHoleCount: Integer;
    property HoleCount: Integer read GetHoleCount;

    function GetHoleBorder(const AIndex: Integer): IGeometryProjectedContour;
    property HoleBorder[const AIndex: Integer]: IGeometryProjectedContour read GetHoleBorder;
  end;

  IGeometryProjectedMultiPolygon = interface(IGeometryProjectedPolygon)
    ['{02C310DE-60C3-4175-8811-367D5C5AC0CE}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(AIndex: Integer): IGeometryProjectedSinglePolygon;
    property Item[AIndex: Integer]: IGeometryProjectedSinglePolygon read GetItem;
  end;

implementation

end.
