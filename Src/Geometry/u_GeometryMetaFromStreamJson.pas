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

unit u_GeometryMetaFromStreamJson;

interface

uses
  Classes,
  superobject,
  i_DoublePointsMeta,
  i_GeometryFromStream,
  u_BaseInterfacedObject;

type
  TGeometryMetaFromStreamJson = class(TBaseInterfacedObject, IGeometryMetaFromStream)
  private
    class function IsMagicOk(const AStream: TStream): Boolean;
    class function JsonFromStream(const AStream: TStream): ISuperObject;
  private
    { IGeometryMetaFromStream }
    function Parse(
      const AStream: TStream
    ): IDoublePointsMeta;
  end;

implementation

uses
  SysUtils,
  EDBase64,
  t_GeoTypes,
  u_GeometryMetaJson,
  u_DoublePointsMetaBuilder;

{ TGeometryMetaFromStreamJson }

function TGeometryMetaFromStreamJson.Parse(
  const AStream: TStream
): IDoublePointsMeta;
var
  I, J: Integer;
  VPointsCount: Integer;
  VJson: ISuperObject;
  VGeoArray: TSuperArray;
  VGeoItem: ISuperObject;
  VMetaArray: TSuperArray;
  VMetaItem: ISuperObject;
  VMetaBuilder: TDoublePointsMetaBuilder;
  VTagName: string;
  VTagDataType: TJsonMetaDataTypeId;
  VMeta: TDoublePointsMeta;
  VDataHolder: array [0..1] of AnsiString;
begin
  Result := nil;

  VJson := Self.JsonFromStream(AStream);
  if VJson = nil then begin
    Exit;
  end;

  if VJson.I['v'] <> 1 then begin
    Exit;
  end;

  if VJson.I['t'] <> Integer(jgLine) then begin
    Exit;
  end;

  VGeoArray := VJson.O['g'].AsArray;
  if VGeoArray = nil then begin
    Exit;
  end;

  VMetaBuilder := TDoublePointsMetaBuilder.Create;
  try
    for I := 0 to VGeoArray.Length - 1 do begin
      if I > 0 then begin
        // New multi-geometry segment starts.
        // We need to add a separation point in order to match the
        // geometry points (de)serialization behavior.
        VMetaBuilder.AddSeparationPoint;
      end;

      VGeoItem := VGeoArray.O[I];

      VMetaArray := VGeoItem.O['m'].AsArray;
      VPointsCount := VGeoItem.I['c'];

      if VMetaArray <> nil then begin
        VDataHolder[0] := '';
        VDataHolder[1] := '';

        for J := 0 to VMetaArray.Length - 1 do begin
          VMetaItem := VMetaArray.O[J];

          VTagName := LowerCase(VMetaItem.S['n']);
          VTagDataType := TJsonMetaDataTypeId(VMetaItem.I['t']);

          if VTagName = CJsonMetaKnownGpxTags[jtEle] then begin
            if VTagDataType = jdDouble then begin
              VDataHolder[0] := Base64Decode(AnsiString(VMetaItem.S['d']));
              if Length(VDataHolder[0]) <> VPointsCount * SizeOf(Double) then begin
                VDataHolder[0] := '';
                Assert(False);
              end;
            end else begin
              Assert(False, 'Unexpected "ele" tag data type!');
            end;
          end else
          if VTagName = CJsonMetaKnownGpxTags[jtTime] then begin
            if VTagDataType = jdDouble then begin
              VDataHolder[1] := Base64Decode(AnsiString(VMetaItem.S['d']));
              if Length(VDataHolder[1]) <> VPointsCount * SizeOf(TDateTime) then begin
                VDataHolder[1] := '';
                Assert(False);
              end;
            end else begin
              Assert(False, 'Unexpected "time" tag data type!');
            end;
          end else begin
            Assert(False, 'Unexpected tag name: ' + VTagName);
          end;
        end;

        VMeta.Elevation := PArrayOfDouble(VDataHolder[0]);
        VMeta.TimeStamp := PArrayOfDateTime(VDataHolder[1]);

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

class function TGeometryMetaFromStreamJson.IsMagicOk(const AStream: TStream): Boolean;
var
  VMagic: Cardinal;
begin
  if AStream.Size >= 4 then begin
    AStream.ReadBuffer(VMagic, 4);
    Result := CompareMem(@VMagic, @CJsonMetaMagic[0], 4);
  end else begin
    Result := False;
  end;
end;

class function TGeometryMetaFromStreamJson.JsonFromStream(
  const AStream: TStream
): ISuperObject;
var
  VStr: AnsiString;
  VLen: Integer;
begin
  if IsMagicOk(AStream) then begin
    VLen := AStream.Size - AStream.Position;
    SetLength(VStr, VLen);

    AStream.ReadBuffer(VStr[1], VLen);

    Result := SO(VStr);
  end else begin
    Result := nil;
  end;
end;

end.
