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

unit u_GeometryFromStreamSML;

interface

uses
  Classes,
  i_GeometryLonLat,
  i_GeometryFromStream,
  i_GeometryLonLatFactory,
  u_BaseInterfacedObject;

type
  TGeometryFromStreamSML = class(TBaseInterfacedObject, IGeometryFromStream)
  private
    FFactory: IGeometryLonLatFactory;
  private
    function Parse(
      const AStream: TStream
    ): IGeometryLonLat;
  public
    constructor Create(
      const AFactory: IGeometryLonLatFactory
    );
  end;

implementation

uses
  Math,
  t_GeoTypes,
  t_GeometryPointSML,
  i_DoublePointsAggregator,
  u_DoublePointsAggregator,
  u_GeoFunc;

procedure Stream2ExtArr(
  const AStream: TStream;
  const AAggregator: IDoublePointsAggregator
);
const
  CMaxDegres: Extended = 360;
  CMinDegres: Extended = -360;
var
  VSize: Integer;
  VPointsCount: Integer;
  i: Integer;
  VPoint: TGeometryPointSML;
  VDoublePoint: TDoublePoint;
begin
  Assert(SizeOf(TGeometryPointSML) = 24);
  VSize := AStream.Size;
  VPointsCount := VSize div SizeOf(TGeometryPointSML);
  for i := 0 to VPointsCount - 1 do begin
    AStream.ReadBuffer(VPoint, SizeOf(TGeometryPointSML));
    try
      if IsNan(VPoint.X) or IsNan(VPoint.Y) then begin
        VDoublePoint := CEmptyDoublePoint;
      end else if (VPoint.X >= CMaxDegres) or (VPoint.X <= CMinDegres) or (VPoint.Y >= CMaxDegres) or (VPoint.Y <= CMinDegres) then begin
        VDoublePoint := CEmptyDoublePoint;
      end else begin
        VDoublePoint := DoublePoint(VPoint.X, VPoint.Y);
      end;
    except
      VDoublePoint := CEmptyDoublePoint;
    end;
    AAggregator.Add(VDoublePoint);
  end;
end;

{ TGeometryFromStreamSML }

constructor TGeometryFromStreamSML.Create(
  const AFactory: IGeometryLonLatFactory
);
begin
  Assert(Assigned(AFactory));
  inherited Create;
  FFactory := AFactory;
end;

function TGeometryFromStreamSML.Parse(const AStream: TStream): IGeometryLonLat;
var
  VPoints: IDoublePointsAggregator;
begin
  VPoints := TDoublePointsAggregator.Create;
  Stream2ExtArr(AStream, VPoints);
  Result := nil;
  if VPoints.Count > 0 then begin
    if VPoints.Count = 1 then begin
      if not PointIsEmpty(VPoints.Points[0]) then begin
        Result := FFactory.CreateLonLatPoint(VPoints.Points[0]);
      end;
    end else begin
      if DoublePointsEqual(VPoints.Points[0], VPoints.Points[VPoints.Count - 1]) then begin
        Result := FFactory.CreateLonLatMultiPolygon(VPoints.Points, VPoints.Count - 1);
      end else begin
        Result := FFactory.CreateLonLatMultiLine(VPoints.Points, VPoints.Count);
      end;
    end;
  end;
end;

end.
