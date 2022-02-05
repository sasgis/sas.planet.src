{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_GeometryToWKB;

interface

uses
  Classes,
  i_GeometryLonLat,
  i_GeometryToStream,
  u_BaseInterfacedObject;

type
  TGeometryToWKB = class(TBaseInterfacedObject, IGeometryPointsToStream)
  private
    procedure SavePoint(
      const AGeometry: IGeometryLonLatPoint;
      const AStream: TStream
    );
    procedure SaveLine(
      const AGeometry: IGeometryLonLatSingleLine;
      const AStream: TStream
    );
    procedure SaveContour(
      const AGeometry: IGeometryLonLatContour;
      const AStream: TStream
    );
    procedure SavePolygon(
      const AGeometry: IGeometryLonLatSinglePolygon;
      const AStream: TStream
    );
    procedure SaveMultiLine(
      const AGeometry: IGeometryLonLatMultiLine;
      const AStream: TStream
    );
    procedure SaveMultiPolygon(
      const AGeometry: IGeometryLonLatMultiPolygon;
      const AStream: TStream
    );
  private
    procedure Save(
      const AGeometry: IGeometryLonLat;
      const AStream: TStream
    );
  end;

implementation

uses
  SysUtils,
  t_GeoTypes;

const
  wkbGeometryTypePoint = 1;
  wkbGeometryTypeLine = 2;
  wkbGeometryTypePolygon = 3;
  wkbGeometryTypeMultiLine = 5;
  wkbGeometryTypeMultiPolygon = 6;

{ TGeometryToWKB }

procedure TGeometryToWKB.Save(
  const AGeometry: IGeometryLonLat;
  const AStream: TStream
);
var
  VOrder: Byte;
  VPoint: IGeometryLonLatPoint;
  VLine: IGeometryLonLatSingleLine;
  VPolygon: IGeometryLonLatSinglePolygon;
  VMultiLine: IGeometryLonLatMultiLine;
  VMultiPolygon: IGeometryLonLatMultiPolygon;
begin
  VOrder := 1;
  AStream.WriteBuffer(VOrder, SizeOf(VOrder));
  if Supports(AGeometry, IGeometryLonLatPoint, VPoint) then begin
    SavePoint(VPoint, AStream);
  end else if Supports(AGeometry, IGeometryLonLatSingleLine, VLine) then begin
    SaveLine(VLine, AStream);
  end else if Supports(AGeometry, IGeometryLonLatSinglePolygon, VPolygon) then begin
    SavePolygon(VPolygon, AStream);
  end else if Supports(AGeometry, IGeometryLonLatMultiLine, VMultiLine) then begin
    if VMultiLine.Count <= 0 then begin
      Assert(False);
    end else if VMultiLine.Count = 1 then begin
      SaveLine(VMultiLine.Item[0], AStream);
    end else begin
      SaveMultiLine(VMultiLine, AStream);
    end;
  end else if Supports(AGeometry, IGeometryLonLatMultiPolygon, VMultiPolygon) then begin
    if VMultiPolygon.Count <= 0 then begin
      Assert(False);
    end else if VMultiPolygon.Count = 1 then begin
      SavePolygon(VMultiPolygon.Item[0], AStream);
    end else begin
      SaveMultiPolygon(VMultiPolygon, AStream);
    end;
  end else begin
    Assert(False);
  end;
end;

procedure TGeometryToWKB.SaveLine(
  const AGeometry: IGeometryLonLatSingleLine;
  const AStream: TStream
);
var
  VWKBType: Cardinal;
  VCount: Cardinal;
begin
  VWKBType := wkbGeometryTypeLine;
  AStream.WriteBuffer(VWKBType, SizeOf(VWKBType));
  VCount := AGeometry.Count;
  AStream.WriteBuffer(VCount, SizeOf(VCount));

  AStream.WriteBuffer(AGeometry.Points^, VCount * SizeOf(TDoublePoint));
end;

procedure TGeometryToWKB.SaveMultiLine(
  const AGeometry: IGeometryLonLatMultiLine;
  const AStream: TStream
);
var
  VWKBType: Cardinal;
  VCount: Cardinal;
  i: Integer;
  VOrder: Byte;
begin
  VWKBType := wkbGeometryTypeMultiLine;
  AStream.WriteBuffer(VWKBType, SizeOf(VWKBType));
  VCount := AGeometry.Count;
  AStream.WriteBuffer(VCount, SizeOf(VCount));
  for i := 0 to AGeometry.Count - 1 do begin
    VOrder := 1;
    AStream.WriteBuffer(VOrder, SizeOf(VOrder));
    SaveLine(AGeometry.Item[i], AStream);
  end;
end;

procedure TGeometryToWKB.SaveMultiPolygon(
  const AGeometry: IGeometryLonLatMultiPolygon;
  const AStream: TStream
);
var
  VWKBType: Cardinal;
  VCount: Cardinal;
  i: Integer;
  VOrder: Byte;
begin
  VWKBType := wkbGeometryTypeMultiPolygon;
  AStream.WriteBuffer(VWKBType, SizeOf(VWKBType));
  VCount := AGeometry.Count;
  AStream.WriteBuffer(VCount, SizeOf(VCount));

  for i := 0 to AGeometry.Count - 1 do begin
    VOrder := 1;
    AStream.WriteBuffer(VOrder, SizeOf(VOrder));
    SavePolygon(AGeometry.Item[i], AStream);
  end;
end;

procedure TGeometryToWKB.SavePoint(
  const AGeometry: IGeometryLonLatPoint;
  const AStream: TStream
);
var
  VWKBType: Cardinal;
  VPoint: TDoublePoint;
begin
  VWKBType := wkbGeometryTypePoint;
  AStream.WriteBuffer(VWKBType, SizeOf(VWKBType));
  VPoint := AGeometry.Point;
  AStream.WriteBuffer(VPoint, SizeOf(TDoublePoint));
end;

procedure TGeometryToWKB.SaveContour(
  const AGeometry: IGeometryLonLatContour;
  const AStream: TStream
);
var
  VCount: Cardinal;
begin
  VCount := AGeometry.Count + 1;
  AStream.WriteBuffer(VCount, SizeOf(VCount));
  AStream.WriteBuffer(AGeometry.Points^, AGeometry.Count * SizeOf(TDoublePoint));
  AStream.WriteBuffer(AGeometry.Points[0], SizeOf(TDoublePoint));
end;

procedure TGeometryToWKB.SavePolygon(
  const AGeometry: IGeometryLonLatSinglePolygon;
  const AStream: TStream
);
var
  VWKBType: Cardinal;
  VCount: Cardinal;
  i: Integer;
begin
  VWKBType := wkbGeometryTypePolygon;
  AStream.WriteBuffer(VWKBType, SizeOf(VWKBType));
  VCount := 1 + AGeometry.HoleCount;
  AStream.WriteBuffer(VCount, SizeOf(VCount));
  SaveContour(AGeometry.OuterBorder, AStream);
  for i := 0 to AGeometry.HoleCount - 1 do begin
    SaveContour(AGeometry.HoleBorder[i], AStream);
  end;
end;

end.
