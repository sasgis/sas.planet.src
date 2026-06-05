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
  TGeometryMetaToStreamJson = class(TBaseInterfacedObject, IGeometryMetaToStream)
  private
    procedure DoSaveLines(
      const ALines: TArrayOfGeometryLonLatSingleLine;
      const AStream: TStream
    );
  private
    { IGeometryMetaToStream }
    procedure Save(
      const AGeometry: IGeometryLonLat;
      const AStream: TStream
    );
  public
    class function HasMeta(const ALine: IGeometryLonLatSingleLine): Boolean; overload; static;
    class function HasMeta(const ALines: TArrayOfGeometryLonLatSingleLine): Boolean; overload; static;
  end;

implementation

uses
  SysUtils,
  SynTable,
  SynCommons,
  t_GeoTypes,
  u_GeometryFunc,
  u_GeometryMetaJson;

{ TGeometryMetaToStreamJson }

procedure TGeometryMetaToStreamJson.Save(
  const AGeometry: IGeometryLonLat;
  const AStream: TStream
);
var
  VLine: IGeometryLonLatLine;
  VLines: TArrayOfGeometryLonLatSingleLine;
begin
  if Supports(AGeometry, IGeometryLonLatPoint) then begin
    // points have no meta
  end else
  if Supports(AGeometry, IGeometryLonLatLine, VLine) then begin
    VLines := GeometryLonLatLineToArray(VLine);
    DoSaveLines(VLines, AStream);
  end else
  if Supports(AGeometry, IGeometryLonLatPolygon) then begin
    // polygons have no meta
  end else begin
    Assert(False, 'Unknown GeometryLonLat type!');
  end;
end;

function ArrayOfDoubleToBase64(const AArr: Pointer; const ACount: Integer): RawUTF8; inline;
begin
  Result := BinToBase64(AArr, ACount * SizeOf(Double));
end;

procedure WriteItem(const AJson: TJSONWriter; const AType: TJsonMetaDataTypeId; const ATag: TJsonMetaKnownGpxTagsId;
  const AData: RawByteString); inline;
begin
  AJson.Add('{"t":%,"n":"%","d":"', [Integer(AType), CJsonMetaKnownGpxTags[ATag]]);
  AJson.FlushToStream;
  AJson.Stream.WriteBuffer(Pointer(AData)^, Length(AData));
  AJson.AddShort('"}');
end;

procedure TGeometryMetaToStreamJson.DoSaveLines(
  const ALines: TArrayOfGeometryLonLatSingleLine;
  const AStream: TStream
);
var
  I: Integer;
  VJson: TJSONWriter;
  VLine: IGeometryLonLatSingleLine;
begin
  if not HasMeta(ALines) then begin
    Exit;
  end;

  // Write magic
  AStream.WriteBuffer(Pointer(CJsonMetaMagic)^, Length(CJsonMetaMagic));

  VJson := TJSONWriter.Create(AStream, False, False);
  try
    // Write root object opening brace
    VJson.Add('{');

    // Write version field
    VJson.Add('"v":%,', [1]);

    // Write geometry type field
    VJson.Add('"t":%,', [Integer(jgLine)]);

    // Start geometries array
    VJson.AddShort('"g":[');

    for I := 0 to Length(ALines) - 1 do begin
      VLine := ALines[I];

      // Add comma separator between array items (except before first item)
      if I > 0 then begin
        VJson.Add(',');
      end;

      // Start geometry object with points count
      VJson.Add('{"c":%,', [VLine.Count]);

      // Start metadata array for this geometry
      // Note: Even if it's empty (VLine.Meta = nil), we still need it
      // for backward compatibility with the legacy parser
      VJson.AddShort('"m":[');

      if VLine.Meta <> nil then begin
        // Write elevation
        if VLine.Meta.Elevation <> nil then begin
          WriteItem(VJson, jdDouble, jtEle, ArrayOfDoubleToBase64(VLine.Meta.Elevation, VLine.Count));
        end;

        // Write timestamp
        if VLine.Meta.TimeStamp <> nil then begin
          if VLine.Meta.Elevation <> nil then begin
            VJson.Add(',');
          end;
          WriteItem(VJson, jdDouble, jtTime, ArrayOfDoubleToBase64(VLine.Meta.TimeStamp, VLine.Count));
        end;
      end;

      // Close metadata array and geometry object
      VJson.AddShort(']}');
    end;

    // Close geometries array and root object
    VJson.AddShort(']}');
  finally
    VJson.FlushFinal;
    VJson.Free;
  end;
end;

class function TGeometryMetaToStreamJson.HasMeta(const ALine: IGeometryLonLatSingleLine): Boolean;
var
  VMeta: PDoublePointsMeta;
begin
  Assert(ALine <> nil);
  VMeta := ALine.Meta;
  Result := (VMeta <> nil) and ( (VMeta.Elevation <> nil) or (VMeta.TimeStamp <> nil) );
end;

class function TGeometryMetaToStreamJson.HasMeta(const ALines: TArrayOfGeometryLonLatSingleLine): Boolean;
var
  I: Integer;
begin
  for I := 0 to Length(ALines) - 1 do begin
    if HasMeta(ALines[I]) then begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

end.
