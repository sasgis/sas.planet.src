{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
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

unit u_GeometryMetaToStreamJson;

interface

uses
  Classes,
  i_GeometryLonLat,
  i_GeometryToStream,
  u_BaseInterfacedObject;

type
  TGeometryMetaToStreamJson = class(TBaseinterfacedObject, IGeometryMetaToStream)
  private
    type
      TArrayOfGeometryLonLatSingleLine = array of IGeometryLonLatSingleLine;
  private
    procedure SaveLines(
      const ALines: TArrayOfGeometryLonLatSingleLine;
      const AStream: TStream
    );
  private
    { IGeometryMetaToStream }
    procedure Save(
      const AGeometry: IGeometryLonLat;
      const AStream: TStream
    );
  end;

implementation

uses
  SysUtils,
  superobject,
  EDBase64,
  t_GeoTypes,
  u_GeometryMetaJson;

{ TGeometryMetaToStreamJson }

procedure TGeometryMetaToStreamJson.Save(
  const AGeometry: IGeometryLonLat;
  const AStream: TStream
);
var
  I: Integer;
  VLine: IGeometryLonLatLine;
  VSingleLine: IGeometryLonLatSingleLine;
  VMultiLine: IGeometryLonLatMultiLine;
  VArrayOfLines: TArrayOfGeometryLonLatSingleLine;
begin
  if Supports(AGeometry, IGeometryLonLatPoint) then begin
    // points have no meta
  end else
  if Supports(AGeometry, IGeometryLonLatLine, VLine) then begin
    if Supports(AGeometry, IGeometryLonLatSingleLine, VSingleLine) then begin
      SetLength(VArrayOfLines, 1);
      VArrayOfLines[0] := VSingleLine;
      SaveLines(VArrayOfLines, AStream);
    end else if Supports(AGeometry, IGeometryLonLatMultiLine, VMultiLine) then begin
      SetLength(VArrayOfLines, VMultiLine.Count);
      for I := 0 to Length(VArrayOfLines) - 1 do begin
        VArrayOfLines[I] := VMultiLine.Item[I];
      end;
      SaveLines(VArrayOfLines, AStream);
    end else begin
      Assert(False, 'Unknown GeometryLonLatLine type!');
    end;
  end else
  if Supports(AGeometry, IGeometryLonLatPolygon) then begin
    // polygons have no meta
  end else begin
    Assert(False, 'Unknown GeometryLonLat type!');
  end;
end;

function ArrayOfDoubleToString(const AArr: Pointer; const ACount: Integer): string; inline;
begin
  Result := Base64Encode(AArr, ACount * SizeOf(Double));
end;

procedure TGeometryMetaToStreamJson.SaveLines(
  const ALines: TArrayOfGeometryLonLatSingleLine;
  const AStream: TStream
);
var
  I: Integer;
  VJson: ISuperObject;
  VGeoItem: ISuperObject;
  VMetaItem: ISuperObject;
  VLine: IGeometryLonLatSingleLine;
  VIsMetaEmpty: Boolean;
begin
  VIsMetaEmpty := True;

  VJson := SO; // root object
  VJson.I['v'] := 1; // version
  VJson.I['t'] := Integer(jgLine); // geometry type
  VJson.O['g'] := SA([]); // array of geometries

  for I := 0 to Length(ALines) - 1 do begin
    VLine := ALines[I];

    VGeoItem := SO; // geometry item object
    VGeoItem.O['m'] := SA([]); // array of meta

    if VLine.Meta <> nil then begin
      VIsMetaEmpty := False;

      VGeoItem.I['c'] := VLine.Count; // points count

      if VLine.Meta.Elevation <> nil then begin
        VMetaItem := SO; // meta item object

        VMetaItem.I['t'] := Integer(jdDouble);
        VMetaItem.S['n'] := CJsonMetaKnownGpxTags[jtEle];
        VMetaItem.S['d'] := ArrayOfDoubleToString(VLine.Meta.Elevation, VLine.Count);

        VGeoItem.A['m'].Add(VMetaItem);
      end;

      if VLine.Meta.TimeStamp <> nil then begin
        VMetaItem := SO; // meta item object

        VMetaItem.I['t'] := Integer(jdDouble);
        VMetaItem.S['n'] := CJsonMetaKnownGpxTags[jtTime];
        VMetaItem.S['d'] := ArrayOfDoubleToString(VLine.Meta.TimeStamp, VLine.Count);

        VGeoItem.A['m'].Add(VMetaItem);
      end;
    end;

    VJson.A['g'].Add(VGeoItem);
  end;

  if not VIsMetaEmpty then begin
    AStream.WriteBuffer(CJsonMetaMagic[0], Length(CJsonMetaMagic));
    VJson.SaveTo(AStream); // writes data to stream as AnsiString
  end;
end;

end.
