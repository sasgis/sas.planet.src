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

unit i_LineOnMapEdit;

interface

uses
  t_GeoTypes,
  i_GeometryLonLat,
  i_Changeable;

type
  ILonLatPathWithSelected = interface
    ['{E94EE4A3-5B01-4310-B710-252E9FAAD6D7}']
    function GetGeometry: IGeometryLonLatLine;
    property Geometry: IGeometryLonLatLine read GetGeometry;

    function GetSelectedPoint: TDoublePoint;
    function GetSelectedSegmentIndex: Integer;
    function GetSelectedPointIndex: Integer;
  end;

  ILonLatPolygonWithSelected = interface
    ['{586F7096-3641-4B08-8D7E-0C74ECC32096}']
    function GetGeometry: IGeometryLonLatPolygon;
    property Geometry: IGeometryLonLatPolygon read GetGeometry;

    function GetSelectedPoint: TDoublePoint;
    function GetSelectedSegmentIndex: Integer;
    function GetSelectedPointIndex: Integer;
  end;

  ILineOnMapEdit = interface(IChangeable)
    ['{BD78781E-F5E0-406B-AE16-E5015BA87743}']
    procedure SetSelectedPoint(
      ASegmentIndex: Integer;
      APointIndex: Integer
    );
    function SetSelectedNextPoint: TDoublePoint;
    function SetSelectedPrevPoint: TDoublePoint;
    function SelectPointInLonLatRect(const ARect: TDoubleRect): Boolean;

    function IsEmpty: Boolean;
    function IsReady: Boolean;
    procedure Clear;
    procedure DeleteActivePoint;
    procedure InsertPoint(const APoint: TDoublePoint);
    procedure MoveActivePoint(const APoint: TDoublePoint);
  end;

  IPathOnMapEdit = interface(ILineOnMapEdit)
    ['{A374154F-48FF-4597-8FD1-599FFE6B4345}']
    function GetPath: ILonLatPathWithSelected;
    property Path: ILonLatPathWithSelected read GetPath;

    procedure SetPath(const AValue: ILonLatPathWithSelected); overload;
    procedure SetPath(const AValue: IGeometryLonLatLine); overload;
  end;

  IPolygonOnMapEdit = interface(ILineOnMapEdit)
    ['{6566E834-169F-4988-99FE-F5489BC985EA}']
    function GetPolygon: ILonLatPolygonWithSelected;
    property Polygon: ILonLatPolygonWithSelected read GetPolygon;

    procedure SetPolygon(const AValue: ILonLatPolygonWithSelected); overload;
    procedure SetPolygon(const AValue: IGeometryLonLatPolygon); overload;
  end;

implementation

end.
