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

unit u_GeometryMetaFromStreamJson;

interface

uses
  Classes,
  i_DoublePointsMeta,
  i_GeometryFromStream,
  u_BaseInterfacedObject;

type
  TGeometryMetaFromStreamJson = class(TBaseInterfacedObject, IGeometryMetaFromStream)
  private
    { IGeometryMetaFromStream }
    function Parse(const AStream: TStream): IDoublePointsMeta;
  end;

implementation

uses
  SysUtils,
  mormot.core.base,
  mormot.core.buffers,
  mormot.core.variants,
  mormot.core.unicode,
  t_GeoTypes,
  u_GeometryMetaJson,
  u_DoublePointsMetaBuilder;

{ TGeometryMetaFromStreamJson }

function IsMagicOk(const AStream: TStream): Boolean; inline;
var
  VMagic: Cardinal;
begin
  Result :=
    (AStream <> nil) and
    (AStream.Size > SizeOf(VMagic)) and
    (AStream.Read(VMagic, SizeOf(VMagic)) = SizeOf(VMagic)) and
    (VMagic = PCardinal(CJsonMetaMagic)^);
end;

function TGeometryMetaFromStreamJson.Parse(const AStream: TStream): IDoublePointsMeta;
var
  I, J: Integer;
  VStr: RawUTF8;
  VLen: Integer;
  VDoc: TDocVariantData;
  VGeoArray: PDocVariantData;
  VMetaArray: PDocVariantData;
  VPointsCount: Integer;
  VMetaBuilder: TDoublePointsMetaBuilder;
  VTagName: RawUTF8;
  VTagDataType: Integer;
  VMeta: TDoublePointsMeta;
  VData: RawUTF8;
  VDataHolder: array[0..1] of RawByteString;
  VGeoItem: PDocVariantData;
  VMetaItem: PDocVariantData;
  VGeometryType: Integer;
  VVersion: Integer;
  VHasMeta: Boolean;
begin
  Result := nil;

  if not IsMagicOk(AStream) then begin
    Exit;
  end;

  VLen := AStream.Size - AStream.Position;
  if VLen <= 0 then Exit;
  SetLength(VStr, VLen);
  AStream.ReadBuffer(VStr[1], VLen);

  // Parse JSON
  if (VDoc.InitJSONInPlace(Pointer(VStr), JSON_FAST) = nil) or (VDoc.Kind <> dvObject) then begin
    Assert(False, Self.ClassName + ': Invalid JSON');
    Exit;
  end;

  // Version
  if not VDoc.GetAsInteger('v', VVersion) or (VVersion <> 1) then begin
    Assert(False, Self.ClassName + ': Unknown version value ' + IntToStr(VVersion));
    Exit;
  end;

  // Geometry type
  if not VDoc.GetAsInteger('t', VGeometryType) or (VGeometryType <> Integer(jgLine)) then begin
    Assert(False, Self.ClassName + ': Unsupported geometry type id ' + IntToStr(VGeometryType));
    Exit;
  end;

  // Geometries array
  if not VDoc.GetAsDocVariant('g', VGeoArray) or (VGeoArray.Kind <> dvArray) then begin
    Assert(False, Self.ClassName + ': Geometry array not found');
    Exit;
  end;

  VMetaBuilder := TDoublePointsMetaBuilder.Create;
  try
    for I := 0 to VGeoArray.Count - 1 do begin
      // Geometry object
      VGeoItem := _Safe(VGeoArray.Values[I]);
      if VGeoItem.Kind <> dvObject then Continue;

      // Points count
      if not VGeoItem.GetAsInteger('c', VPointsCount) or (VPointsCount <= 0) then Continue;

      if I > 0 then begin
        // New multi-geometry segment starts.
        // We need to add a separation point in order to match the
        // geometry points (de)serialization behavior.
        VMetaBuilder.AddSeparationPoint;
      end;

      // Metadata array
      VHasMeta :=
        VGeoItem.GetAsDocVariant('m', VMetaArray) and
        (VMetaArray.Kind = dvArray) and
        (VMetaArray.Count > 0);

      if VHasMeta then begin
        VDataHolder[0] := ''; // elevation
        VDataHolder[1] := ''; // timestamp

        for J := 0 to VMetaArray.Count - 1 do begin
          // Metadata object
          VMetaItem := _Safe(VMetaArray.Values[J]);
          if VMetaItem.Kind <> dvObject then Continue;

          if not VMetaItem.GetAsInteger('t', VTagDataType) then Continue;
          if not VMetaItem.GetAsRawUTF8('n', VTagName) then Continue;
          if not VMetaItem.GetAsRawUTF8('d', VData) then Continue;

          VTagName := mormot.core.unicode.LowerCase(VTagName);

          if VTagName = CJsonMetaKnownGpxTags[jtEle] then begin
            // Elevation
            if VTagDataType = Integer(jdDouble) then begin
              VDataHolder[0] := Base64ToBin(VData);
              if Length(VDataHolder[0]) <> VPointsCount * SizeOf(Double) then begin
                VDataHolder[0] := '';
                Assert(False, Self.ClassName + ': Invalid elevation data size');
              end;
            end else begin
              Assert(False, Self.ClassName + ': Unexpected "ele" tag data type!');
            end;
          end else
          if VTagName = CJsonMetaKnownGpxTags[jtTime] then begin
            // Timestamp
            if VTagDataType = Integer(jdDouble) then begin
              VDataHolder[1] := Base64ToBin(VData);
              if Length(VDataHolder[1]) <> VPointsCount * SizeOf(TDateTime) then begin
                VDataHolder[1] := '';
                Assert(False, Self.ClassName + ': Invalid timestamp data size');
              end;
            end else begin
              Assert(False, Self.ClassName + ': Unexpected "time" tag data type!');
            end;
          end else begin
            Assert(False, Self.ClassName + ': Unexpected tag name: ' + string(VTagName));
          end;
        end;

        VMeta.Elevation := PArrayOfDouble(Pointer(VDataHolder[0]));
        VMeta.TimeStamp := PArrayOfDateTime(Pointer(VDataHolder[1]));

        VMetaBuilder.Add(@VMeta, VPointsCount);
      end else begin
        // Add multi-geometry segment with no meta
        VMetaBuilder.AddEmptyPoints(VPointsCount);
      end;
    end;

    Result := VMetaBuilder.Build;
  finally
    VMetaBuilder.Free;
  end;
end;

end.
