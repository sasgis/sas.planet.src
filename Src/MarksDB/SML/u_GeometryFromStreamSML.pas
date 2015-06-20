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
  u_GeoFunc,
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
  u_DoublePointsAggregator;

const
  CMaxDegres: Extended = 360;
  CMinDegres: Extended = -360;

procedure PointToArray(
  const APoint: PGeometryPointSML;
  const Arr: IDoublePointsAggregator
); inline;
var
  VDoublePoint: TDoublePoint;
begin
  try
    if IsNan(APoint.X) or IsNan(APoint.Y) then begin
      VDoublePoint := CEmptyDoublePoint;
    end else if (APoint.X >= CMaxDegres) or (APoint.X <= CMinDegres) or (APoint.Y >= CMaxDegres) or (APoint.Y <= CMinDegres) then begin
      VDoublePoint := CEmptyDoublePoint;
    end else begin
      VDoublePoint := DoublePoint(APoint.X, APoint.Y);
    end;
  except
    VDoublePoint := CEmptyDoublePoint;
  end;
  Arr.Add(VDoublePoint);
end;

function StreamToDoublePointsArray(
  const AStream: TStream
): IDoublePointsAggregator; inline;
var
  VSize: Integer;
  VPointsCount: Integer;
  I: Integer;
  VPoint: TGeometryPointSML;
  VSmlPoint: PGeometryPointSML;
  VSmlPointRecSize: Integer;
begin
  VSmlPointRecSize := SizeOf(TGeometryPointSML);

  Assert(VSmlPointRecSize = 24);

  VSize := AStream.Size;

  if AStream is TMemoryStream then begin
    VSmlPoint := (AStream as TMemoryStream).Memory;
  end else begin
    VSmlPoint := nil;
  end;

  VPointsCount := VSize div VSmlPointRecSize;
  Result := TDoublePointsAggregator.Create(VPointsCount);

  if VSmlPoint <> nil then begin
    for I := 0 to VPointsCount - 1 do begin
      PointToArray(VSmlPoint, Result);
      Inc(VSmlPoint);
    end;
  end else begin
    for I := 0 to VPointsCount - 1 do begin
      AStream.ReadBuffer(VPoint, VSmlPointRecSize);
      PointToArray(@VPoint, Result);
    end;
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
  Result := nil;
  VPoints := StreamToDoublePointsArray(AStream);
  if Assigned(VPoints) and (VPoints.Count > 0) then begin
    if VPoints.Count = 1 then begin
      if not PointIsEmpty(VPoints.Points[0]) then begin
        Result := FFactory.CreateLonLatPoint(VPoints.Points[0]);
      end;
    end else begin
      if DoublePointsEqual(VPoints.Points[0], VPoints.Points[VPoints.Count - 1]) then begin
        Result := FFactory.CreateLonLatPolygon(VPoints.Points, VPoints.Count - 1);
      end else begin
        Result := FFactory.CreateLonLatLine(VPoints.Points, VPoints.Count);
      end;
    end;
  end;
end;

end.
