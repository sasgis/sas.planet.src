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

unit i_LineOnMapEdit;

interface

uses
  t_GeoTypes,
  i_VectorItemLonLat,
  i_ConfigDataElement;

type
  ILineOnMapEdit = interface(IConfigDataElement)
  ['{76049798-151B-4E06-9EF9-3BE14451BCFF}']
    function GetCount: Integer;
    function GetActiveIndex: Integer;
    function GetPoints: TArrayOfDoublePoint;
    function GetPointIndexInLonLatRect(ARect: TDoubleRect): Integer;
    procedure Empty;
    procedure SetActiveIndex(AValue: Integer);
    procedure DeleteActivePoint;
    procedure InsertPoint(APoint: TDoublePoint);
    procedure MoveActivePoint(APoint: TDoublePoint);
    procedure SetPoints(AValue: TArrayOfDoublePoint);
  end;

  ILonLatPathWithSelected = interface(ILonLatPath)
    ['{3ED6ABA4-D618-4A82-A428-EFF74D482161}']
    function GetSelectedPoint: TDoublePoint;
    function GetSelectedSegmentIndex: Integer;
    function GetSelectedPointIndex: Integer;
  end;

  ILonLatPolygonWithSelected = interface(ILonLatPolygon)
    ['{4F1931DF-57E1-4082-A83F-D23FB74F2F28}']
    function GetSelectedPoint: TDoublePoint;
    function GetSelectedSegmentIndex: Integer;
    function GetSelectedPointIndex: Integer;
  end;

  ILineOnMapEditNew = interface(IConfigDataElement)
    ['{BD78781E-F5E0-406B-AE16-E5015BA87743}']
    procedure SetSelectedPoint(ASegmentIndex: Integer; APointIndex: Integer);
    procedure SetSelectedNextPoint;
    procedure SetSelectedPrevPoint;
    function SelectPointInLonLatRect(ARect: TDoubleRect): Boolean;

    procedure Clear;
    procedure DeleteActivePoint;
    procedure InsertPoint(APoint: TDoublePoint);
    procedure MoveActivePoint(APoint: TDoublePoint);
  end;

  IPathOnMapEdit = interface(ILineOnMapEditNew)
    ['{A374154F-48FF-4597-8FD1-599FFE6B4345}']
    function GetPath: ILonLatPathWithSelected;
    procedure SetPath(AValue: ILonLatPathWithSelected);
    property Path: ILonLatPathWithSelected read GetPath write SetPath;
  end;

  IPolygonOnMapEdit = interface(ILineOnMapEditNew)
    ['{6566E834-169F-4988-99FE-F5489BC985EA}']
    function GetPolygon: ILonLatPolygonWithSelected;
    procedure SetPolygon(AValue: ILonLatPolygonWithSelected);
    property Polygon: ILonLatPolygonWithSelected read GetPolygon write SetPolygon;
  end;

implementation

end.
