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

unit u_GeometryToStreamSML;

interface

uses
  Classes,
  t_GeoTypes,
  i_GeometryLonLat,
  i_GeometryToStream,
  u_BaseInterfacedObject;

type
  TGeometryToStreamSML = class(TBaseInterfacedObject, IGeometryToStream)
  private
    procedure SavePoint(
      const AGeometry: IGeometryLonLatPoint;
      const AStream: TStream
    );
    procedure SaveSingleLine(
      const AGeometry: IGeometryLonLatSingleLine;
      const AStream: TStream
    );
    procedure SaveMultiLine(
      const AGeometry: IGeometryLonLatMultiLine;
      const AStream: TStream
    );
    procedure SaveLine(
      const AGeometry: IGeometryLonLatLine;
      const AStream: TStream
    );
    procedure SaveContour(
      const AGeometry: IGeometryLonLatContour;
      const AStream: TStream;
      var AFirstPoint: TDoublePoint
    );
    procedure SaveSinglePolygon(
      const AGeometry: IGeometryLonLatSinglePolygon;
      const AStream: TStream;
      var AFirstPoint: TDoublePoint
    );
    procedure SaveMultiPolygon(
      const AGeometry: IGeometryLonLatMultiPolygon;
      const AStream: TStream;
      var AFirstPoint: TDoublePoint
    );
    procedure SavePolygon(
      const AGeometry: IGeometryLonLatPolygon;
      const AStream: TStream
    );
  private
    procedure Save(
      const AGeometry: IGeometryLonLat;
      const AStream: TStream
    );
  public
    constructor Create();
  end;

implementation

uses
  SysUtils,
  Math,
  t_GeometryPointSML,
  i_EnumDoublePoint,
  u_GeoFunc;

{ TGeometryToStreamSML }

constructor TGeometryToStreamSML.Create;
begin
  inherited Create;
end;

procedure TGeometryToStreamSML.Save(
  const AGeometry: IGeometryLonLat;
  const AStream: TStream
);
var
  VPoint: IGeometryLonLatPoint;
  VLine: IGeometryLonLatLine;
  VPolygon: IGeometryLonLatPolygon;
begin
  if Supports(AGeometry, IGeometryLonLatPoint, VPoint) then begin
    SavePoint(VPoint, AStream);
  end else if Supports(AGeometry, IGeometryLonLatLine, VLine) then begin
    SaveLine(VLine, AStream);
  end else if Supports(AGeometry, IGeometryLonLatPolygon, VPolygon) then begin
    SavePolygon(VPolygon, AStream);
  end else begin
    Assert(False);
  end;
end;

procedure TGeometryToStreamSML.SavePoint(
  const AGeometry: IGeometryLonLatPoint;
  const AStream: TStream
);
var
  VPoint: TGeometryPointSML;
begin
  Assert(SizeOf(TGeometryPointSML) = 24);
  VPoint.X := AGeometry.Point.X;
  VPoint.Y := AGeometry.Point.Y;
  AStream.Write(VPoint, SizeOf(VPoint));
end;

procedure TGeometryToStreamSML.SaveLine(
  const AGeometry: IGeometryLonLatLine;
  const AStream: TStream
);
var
  VSingleLine: IGeometryLonLatSingleLine;
  VMultiLine: IGeometryLonLatMultiLine;
  VPoint: TGeometryPointSML;
begin
  if Supports(AGeometry, IGeometryLonLatSingleLine, VSingleLine) then begin
    SaveSingleLine(VSingleLine, AStream);
  end else if Supports(AGeometry, IGeometryLonLatMultiLine, VMultiLine) then begin
    SaveMultiLine(VMultiLine, AStream);
  end else begin
    Assert(False);
  end;
  if AStream.Size > 0 then begin
    VPoint.X := CEmptyDoublePoint.X;
    VPoint.Y := CEmptyDoublePoint.Y;
    AStream.Write(VPoint, SizeOf(VPoint));
  end;
end;

procedure TGeometryToStreamSML.SaveSingleLine(
  const AGeometry: IGeometryLonLatSingleLine;
  const AStream: TStream
);
var
  VPoint: TGeometryPointSML;
  VEnum: IEnumLonLatPoint;
  VCurrPoint: TDoublePoint;
begin
  Assert(SizeOf(TGeometryPointSML) = 24);
  VEnum := AGeometry.GetEnum;
  while VEnum.Next(VCurrPoint) do begin
    VPoint.X := VCurrPoint.X;
    VPoint.Y := VCurrPoint.Y;
    AStream.Write(VPoint, SizeOf(VPoint));
  end;
end;

procedure TGeometryToStreamSML.SaveMultiLine(
  const AGeometry: IGeometryLonLatMultiLine;
  const AStream: TStream
);
var
  i: Integer;
  VPoint: TGeometryPointSML;
begin
  Assert(SizeOf(TGeometryPointSML) = 24);
  Assert(AGeometry.Count > 0);
  SaveSingleLine(AGeometry.Item[0], AStream);
  for i := 1 to AGeometry.Count - 1 do begin
    VPoint.X := CEmptyDoublePoint.X;
    VPoint.Y := CEmptyDoublePoint.Y;
    AStream.Write(VPoint, SizeOf(VPoint));
    SaveSingleLine(AGeometry.Item[0], AStream);
  end;
end;

procedure TGeometryToStreamSML.SavePolygon(
  const AGeometry: IGeometryLonLatPolygon;
  const AStream: TStream
);
var
  VPoint: TGeometryPointSML;
  VFirstPoint: TDoublePoint;
  VSinglePolygon: IGeometryLonLatSinglePolygon;
  VMultiPolygon: IGeometryLonLatMultiPolygon;
begin
  if Supports(AGeometry, IGeometryLonLatSinglePolygon, VSinglePolygon) then begin
    SaveSinglePolygon(VSinglePolygon, AStream, VFirstPoint);
  end else if Supports(AGeometry, IGeometryLonLatMultiPolygon, VMultiPolygon) then begin
    SaveMultiPolygon(VMultiPolygon, AStream, VFirstPoint);
  end else begin
    Assert(False);
  end;
  if AStream.Size > 0 then begin
    VPoint.X := VFirstPoint.X;
    VPoint.Y := VFirstPoint.Y;
    AStream.Write(VPoint, SizeOf(VPoint));
  end;
end;

procedure TGeometryToStreamSML.SaveContour(
  const AGeometry: IGeometryLonLatContour;
  const AStream: TStream;
  var AFirstPoint: TDoublePoint
);
var
  VPoint: TGeometryPointSML;
  VEnum: IEnumLonLatPoint;
  VCurrPoint: TDoublePoint;
begin
  Assert(SizeOf(TGeometryPointSML) = 24);
  Assert(AGeometry.Count > 0);
  VEnum := AGeometry.GetEnum;
  if VEnum.Next(AFirstPoint) then begin
    VPoint.X := AFirstPoint.X;
    VPoint.Y := AFirstPoint.Y;
    AStream.Write(VPoint, SizeOf(VPoint));
    while VEnum.Next(VCurrPoint) do begin
      VPoint.X := VCurrPoint.X;
      VPoint.Y := VCurrPoint.Y;
      AStream.Write(VPoint, SizeOf(VPoint));
    end;
  end;
end;

procedure TGeometryToStreamSML.SaveSinglePolygon(
  const AGeometry: IGeometryLonLatSinglePolygon;
  const AStream: TStream;
  var AFirstPoint: TDoublePoint
);
var
  VPoint: TGeometryPointSML;
  VContour: IGeometryLonLatContour;
  VFirstPoint: TDoublePoint;
  i: Integer;
begin
  VContour := AGeometry.OuterBorder;
  SaveContour(VContour, AStream, AFirstPoint);
  for i := 0 to AGeometry.HoleCount - 1 do begin
    VPoint.X := NaN;
    VPoint.Y := -1;
    AStream.Write(VPoint, SizeOf(VPoint));
    VContour := AGeometry.HoleBorder[i];
    SaveContour(VContour, AStream, VFirstPoint);
  end;
end;

procedure TGeometryToStreamSML.SaveMultiPolygon(
  const AGeometry: IGeometryLonLatMultiPolygon;
  const AStream: TStream;
  var AFirstPoint: TDoublePoint
);
var
  i: Integer;
  VPolygon: IGeometryLonLatSinglePolygon;
  VPoint: TGeometryPointSML;
  VFirstPoint: TDoublePoint;
begin
  Assert(Assigned(AGeometry));
  Assert(AGeometry.Count > 0);

  VPolygon := AGeometry.Item[0];
  SaveSinglePolygon(VPolygon, AStream, AFirstPoint);
  for i := 1 to AGeometry.Count - 1 do begin
    VPoint.X := NaN;
    VPoint.Y := NaN;
    AStream.Write(VPoint, SizeOf(VPoint));

    VPolygon := AGeometry.Item[i];
    SaveSinglePolygon(VPolygon, AStream, VFirstPoint);
  end;
end;

end.

