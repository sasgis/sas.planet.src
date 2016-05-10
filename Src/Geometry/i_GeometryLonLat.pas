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

unit i_GeometryLonLat;

interface

uses
  t_GeoTypes,
  t_Hash,
  i_EnumDoublePoint,
  i_LonLatRect;

type
  IGeometryLonLat = interface
    ['{E53FCF09-DA26-44B1-854C-CC2A1330A3F0}']
    function GetHash: THashValue;
    property Hash: THashValue read GetHash;

    function GetBounds: ILonLatRect;
    property Bounds: ILonLatRect read GetBounds;

    function GetGoToPoint: TDoublePoint;
    function IsSameGeometry(const AGeometry: IGeometryLonLat): Boolean;
  end;

  IGeometryLonLatPoint = interface(IGeometryLonLat)
    ['{C52B78AD-2635-48A6-9C8B-E94C4592CFD0}']
    function IsSame(const APoint: IGeometryLonLatPoint): Boolean;

    function GetPoint: TDoublePoint;
    property Point: TDoublePoint read GetPoint;
  end;

  IGeometryLonLatMultiPoint = interface(IGeometryLonLat)
    ['{D8376ED0-D2F9-4D8A-AEE0-F61AEB7CE254}']
    function IsSame(const APoint: IGeometryLonLatMultiPoint): Boolean;
    function GetEnum: IEnumLonLatPoint;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetPoints: PDoublePointArray;
    property Points: PDoublePointArray read GetPoints;
  end;


  IGeometryLonLatLine = interface(IGeometryLonLat)
    ['{05412527-06DC-43F9-8902-97D7112E1FFD}']
  end;

  IGeometryLonLatSingleLine = interface(IGeometryLonLatLine)
    ['{F309D486-2E2A-4526-8BB8-A38A47E3C8FF}']
    function IsSame(const ALine: IGeometryLonLatSingleLine): Boolean;
    function GetEnum: IEnumLonLatPoint;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetPoints: PDoublePointArray;
    property Points: PDoublePointArray read GetPoints;
  end;

  IGeometryLonLatMultiLine = interface(IGeometryLonLatLine)
    ['{5BB3E4AF-5420-4EDB-9DE0-D44FFA38519E}']
    function IsSame(const ALine: IGeometryLonLatMultiLine): Boolean;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(AIndex: Integer): IGeometryLonLatSingleLine;
    property Item[AIndex: Integer]: IGeometryLonLatSingleLine read GetItem;
  end;

  IGeometryLonLatPolygon = interface(IGeometryLonLat)
    ['{0D3F41A0-4170-40E1-9A01-939824A6CF34}']
  end;

  IGeometryLonLatContour = interface(IGeometryLonLatPolygon)
    ['{4729DBB2-3537-42F7-BA7E-C37669D89811}']
    function IsSame(const ALine: IGeometryLonLatContour): Boolean;
    function GetEnum: IEnumLonLatPoint;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetPoints: PDoublePointArray;
    property Points: PDoublePointArray read GetPoints;
  end;

  IGeometryLonLatSinglePolygon = interface(IGeometryLonLatPolygon)
    ['{C9FF5A32-B90D-43D2-9394-9E54A4F29905}']
    function IsSame(const ALine: IGeometryLonLatSinglePolygon): Boolean;

    function GetOuterBorder: IGeometryLonLatContour;
    property OuterBorder: IGeometryLonLatContour read GetOuterBorder;

    function GetHoleCount: Integer;
    property HoleCount: Integer read GetHoleCount;

    function GetHoleBorder(const AIndex: Integer): IGeometryLonLatContour;
    property HoleBorder[const AIndex: Integer]: IGeometryLonLatContour read GetHoleBorder;
  end;

  IGeometryLonLatMultiPolygon = interface(IGeometryLonLatPolygon)
    ['{E71E059B-8FB3-42AD-97BD-7777AC66C8F2}']
    function IsSame(const ALine: IGeometryLonLatMultiPolygon): Boolean;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(AIndex: Integer): IGeometryLonLatSinglePolygon;
    property Item[AIndex: Integer]: IGeometryLonLatSinglePolygon read GetItem;
  end;

implementation

end.
