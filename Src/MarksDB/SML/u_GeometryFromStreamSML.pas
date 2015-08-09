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
    function ParseLine(
      const AStream: TStream;
      const ACount: Integer
    ): IGeometryLonLatLine;
    function ParsePolygon(
      const AStream: TStream;
      const ACount: Integer
    ): IGeometryLonLatPolygon;
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

function SMLPointToDoublePoint(
  const APoint: TGeometryPointSML
): TDoublePoint; inline;
begin
  try
    if IsNan(APoint.X) or IsNan(APoint.Y) then begin
      Result := CEmptyDoublePoint;
    end else if (APoint.X >= CMaxDegres) or (APoint.X <= CMinDegres) or (APoint.Y >= CMaxDegres) or (APoint.Y <= CMinDegres) then begin
      Result := CEmptyDoublePoint;
    end else begin
      Result := DoublePoint(APoint.X, APoint.Y);
    end;
  except
    Result := CEmptyDoublePoint;
  end;
end;

function SMLPointToDoublePointEx(
  const APoint: TGeometryPointSML;
  out AXIsValid: Boolean;
  out AYIsValid: Boolean
): TDoublePoint; inline;
begin
  try
    if IsNan(APoint.X) then begin
      AXIsValid := False;
      if IsNan(APoint.Y) then begin
        Result := CEmptyDoublePoint;
        AYIsValid := False;
      end else begin
        Result.X := NaN;
        Result.Y := -1;
        AYIsValid := True;
      end;
    end else if (APoint.X >= CMaxDegres) or (APoint.X <= CMinDegres) or (APoint.Y >= CMaxDegres) or (APoint.Y <= CMinDegres) then begin
      Result := CEmptyDoublePoint;
      AXIsValid := False;
      AYIsValid := False;
    end else begin
      Result := DoublePoint(APoint.X, APoint.Y);
      AXIsValid := True;
      AYIsValid := True;
    end;
  except
    Result := CEmptyDoublePoint;
    AXIsValid := False;
    AYIsValid := False;
  end;
end;

const
  CSmlPointRecSize = SizeOf(TGeometryPointSML);

function GetPointsCountBySmlSize(const ASize: Int64): Integer; inline;
begin
  Assert(CSmlPointRecSize = 24);
  Assert(ASize >= 0);
  Assert(ASize < MaxInt / CSmlPointRecSize);
  Result := ASize div CSmlPointRecSize;
end;

function ReadDoublePointFromStream(
  const AStream: TStream
): TDoublePoint; inline;
var
  VPoint: TGeometryPointSML;
begin
  AStream.ReadBuffer(VPoint, CSmlPointRecSize);
  Result := SMLPointToDoublePoint(VPoint);
end;

function SmlPointsEqual(const p1,p2: TGeometryPointSML): Boolean; inline;
var
  VP1Empty: Boolean;
  VP2Empty: Boolean;
begin
  VP1Empty := IsNan(p1.x) or IsNan(p1.Y);
  VP2Empty := IsNan(p2.x) or IsNan(p2.Y);
  if VP1Empty and VP2Empty then begin
    Result := True;
  end else begin
    if not VP1Empty and not VP2Empty then begin
      Result := (p1.x=p2.X)and(p1.y=p2.y);
    end else begin
      Result := False;
    end;
  end;
end;

function IsPolygonInStream(
  const AStream: TStream;
  var ACount: Integer
): Boolean; // inline;
var
  VSmlPoint: PGeometryPointSML;
  VFirst: TGeometryPointSML;
  VLast: TGeometryPointSML;
begin
  Assert(Assigned(AStream));
  Assert(ACount > 0);
  Assert(ACount = GetPointsCountBySmlSize(AStream.Size));
  if AStream is TCustomMemoryStream then begin
    VSmlPoint := (AStream as TCustomMemoryStream).Memory;
    VFirst := VSmlPoint^;
    Inc(VSmlPoint, ACount - 1);
    VLast := VSmlPoint^;
  end else begin
    AStream.ReadBuffer(VFirst, CSmlPointRecSize);
    AStream.Seek((ACount - 1) * CSmlPointRecSize, soBeginning);
    AStream.ReadBuffer(VLast, CSmlPointRecSize);
    AStream.Seek(0, soBeginning);
  end;
  if SmlPointsEqual(VFirst, VLast) then begin
    Result := True;
    Dec(ACount);
  end else begin
    Result := False;
    if IsNan(VLast.X) or IsNan(VLast.Y) then begin
      Dec(ACount);
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

function TGeometryFromStreamSML.ParseLine(
  const AStream: TStream;
  const ACount: Integer
): IGeometryLonLatLine;
var
  VSmlPoint: PGeometryPointSML;
  I: Integer;
  VDoublePoint: TDoublePoint;
  VBuilder: IGeometryLonLatLineBuilder;
  VTemp: IDoublePointsAggregator;
  VXIsValid: Boolean;
  VYIsValid: Boolean;
begin
  if AStream is TCustomMemoryStream then begin
    VBuilder := FFactory.MakeLineBuilder;
    VTemp := TDoublePointsAggregator.Create;
    VSmlPoint := (AStream as TCustomMemoryStream).Memory;
    for I := 0 to ACount - 1 do begin
      VDoublePoint := SMLPointToDoublePointEx(VSmlPoint^, VXIsValid, VYIsValid);
      if VXIsValid and VYIsValid then begin
        VTemp.Add(VDoublePoint);
      end else begin
        if VTemp.Count > 0 then begin
          VBuilder.AddLine(VTemp.MakeStaticCopy);
          VTemp.Clear;
        end;
      end;
      Inc(VSmlPoint);
    end;
    if VTemp.Count > 0 then begin
      VBuilder.AddLine(VTemp.MakeStaticCopy);
      VTemp.Clear;
    end;
    Result := VBuilder.MakeStaticAndClear;
  end else begin
    Assert(False, 'Not Implemented');
  end;
end;

function TGeometryFromStreamSML.ParsePolygon(
  const AStream: TStream;
  const ACount: Integer
): IGeometryLonLatPolygon;
var
  VSmlPoint: PGeometryPointSML;
  I: Integer;
  VDoublePoint: TDoublePoint;
  VBuilder: IGeometryLonLatPolygonBuilder;
  VTemp: IDoublePointsAggregator;
  VIsOuter: Boolean;
  VXIsValid: Boolean;
  VYIsValid: Boolean;
begin
  if AStream is TCustomMemoryStream then begin
    VBuilder := FFactory.MakePolygonBuilder;
    VTemp := TDoublePointsAggregator.Create;
    VSmlPoint := (AStream as TCustomMemoryStream).Memory;
    VIsOuter := True;
    for I := 0 to ACount - 1 do begin
      VDoublePoint := SMLPointToDoublePointEx(VSmlPoint^, VXIsValid, VYIsValid);
      if VXIsValid and VYIsValid then begin
        VTemp.Add(VDoublePoint);
      end else begin
        if VTemp.Count > 0 then begin
          if VIsOuter then begin
            VBuilder.AddOuter(VTemp.MakeStaticCopy);
          end else begin
            VBuilder.AddHole(VTemp.MakeStaticCopy);
          end;
          VTemp.Clear;
          VIsOuter := not VYIsValid;
        end;
      end;
      Inc(VSmlPoint);
    end;
    if VTemp.Count > 0 then begin
      if VIsOuter then begin
        VBuilder.AddOuter(VTemp.MakeStaticCopy);
      end else begin
        VBuilder.AddHole(VTemp.MakeStaticCopy);
      end;
      VTemp.Clear;
    end;
    Result := VBuilder.MakeStaticAndClear;
  end else begin
    Assert(False, 'Not Implemented');
  end;
end;

function TGeometryFromStreamSML.Parse(const AStream: TStream): IGeometryLonLat;
var
  VCount: Integer;
  VPoint: TDoublePoint;
begin
  Assert(Assigned(AStream));
  Assert(AStream.Position = 0);
  Result := nil;
  VCount := GetPointsCountBySmlSize(AStream.Size);
  if VCount > 0 then begin
    if VCount = 1 then begin
      VPoint := ReadDoublePointFromStream(AStream);
      if not PointIsEmpty(VPoint) then begin
        Result := FFactory.CreateLonLatPoint(VPoint);
      end;
    end else begin
      if IsPolygonInStream(AStream, VCount) then begin
        Result := ParsePolygon(AStream, VCount);
      end else begin
        Result := ParseLine(AStream, VCount);
      end;
    end;
  end;
end;

end.
