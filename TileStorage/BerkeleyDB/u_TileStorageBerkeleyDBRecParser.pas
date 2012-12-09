{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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

unit u_TileStorageBerkeleyDBRecParser;

interface

uses
  Types,
  Classes;

type
  PBDBData = ^TBDBData;

  TBDBData = record
    RecMagic: array [0..3] of AnsiChar;
    RecCRC32: Cardinal;
    TileSize: Cardinal;
    TileDate: TDateTime;
    TileVer: PWideChar;
    TileMIME: PWideChar;
    TileBody: PByte;
  end;

  PBDBStorageMetaInfo = ^TBDBStorageMetaInfo;

  TBDBStorageMetaInfo = record
    MetaMagic: array [0..3] of AnsiChar;
    MetaCRC32: Cardinal;
    StorageEPSG: Integer;
  end;

function PBDBDataToMemStream(
    AData: PBDBData;
    out AStream: TMemoryStream
  ): Boolean;
function PRawDataToPBDBData(
    ARawData: PByte;
    ARawDataSize: Cardinal;
    AData: PBDBData
  ): Boolean;

function PBDBMetaInfoToMemStream(
    AMeta: PBDBStorageMetaInfo;
    out AStream: TMemoryStream
  ): Boolean;
function PRawMetaToPBDBMetaInfo(
    ARawData: PByte;
    ARawDataSize: Cardinal;
    AMeta: PBDBStorageMetaInfo
  ): Boolean;

implementation

uses
  SysUtils,
  libdb51,
  CRC32;

const
  CBDBMetaVersion = #01;
  CBDBMetaMagic: array [0..3] of AnsiChar = ('M', 'I', 'D', CBDBMetaVersion);    //Meta Info Data

  CBDBRecVersion = #03;
  CBDBRecMagic: array [0..3] of AnsiChar = ('T', 'L', 'D', CBDBRecVersion);      //TiLe Data

function PBDBDataToMemStream(
  AData: PBDBData;
  out AStream: TMemoryStream
): Boolean;

  procedure WideCharToStream(
    APWideChar: PWideChar;
    AStream: TMemoryStream
  );
  const
    CEndLine: WideChar = #0000;
  begin
    if (APWideChar <> nil) and (Length(APWideChar) > 0) then begin
      AStream.WriteBuffer(APWideChar^, Length(APWideChar) * SizeOf(WideChar));
    end;
    AStream.WriteBuffer(CEndLine, SizeOf(CEndLine));
  end;

begin
  Result := False;
  if (AData <> nil) and Assigned(AStream) then begin
    AStream.Clear;
    AStream.Position := 0;
    AData.RecCRC32 := 0;
    Move(CBDBRecMagic[0], AData.RecMagic[0], Length(AData.RecMagic));
    AStream.WriteBuffer(AData.RecMagic[0], Length(AData.RecMagic));
    AStream.WriteBuffer(AData.RecCRC32, SizeOf(AData.RecCRC32));
    AStream.WriteBuffer(AData.TileSize, SizeOf(AData.TileSize));
    AStream.WriteBuffer(AData.TileDate, SizeOf(AData.TileDate));
    WideCharToStream(AData.TileVer, AStream);
    WideCharToStream(AData.TileMIME, AStream);
    if (AData.TileSize > 0) and (AData.TileBody <> nil) then begin
      AStream.WriteBuffer(AData.TileBody^, AData.TileSize);
    end;
    AStream.Position := 0;
    AData.RecCRC32 := CRC32Buf(AStream.Memory, AStream.Size);
    AStream.Position := Length(AData.RecMagic);
    AStream.WriteBuffer(AData.RecCRC32, SizeOf(AData.RecCRC32));
    AStream.Position := 0;
    Result := True;
  end;
end;

function PRawDataToPBDBData(
  ARawData: PByte;
  ARawDataSize: Cardinal;
  AData: PBDBData
): Boolean;
var
  PRawData: PByte;
  VCRC32: Cardinal;
begin
  Result := False;
  if (AData <> nil) and (ARawData <> nil) then begin
    FillChar(AData^, SizeOf(TBDBData), 0);
    PRawData := ARawData;

    Move(PRawData^, AData.RecMagic[0], Length(AData.RecMagic));
    Inc(PRawData, Length(AData.RecMagic));

    if AData.RecMagic = CBDBRecMagic then begin
      AData.RecCRC32 := PCardinal(PRawData)^;
      PCardinal(PRawData)^ := 0;

      VCRC32 := CRC32Buf(ARawData, ARawDataSize);

      PCardinal(PRawData)^ := AData.RecCRC32;
      if VCRC32 = AData.RecCRC32 then begin
        Inc(PRawData, SizeOf(AData.RecCRC32));

        AData.TileSize := PCardinal(PRawData)^;
        Inc(PRawData, SizeOf(AData.TileSize));

        AData.TileDate := PDateTime(PRawData)^;
        Inc(PRawData, SizeOf(AData.TileDate));

        AData.TileVer := PWideChar(PRawData);
        Inc(PRawData, (Length(AData.TileVer) + 1) * SizeOf(WideChar));

        AData.TileMIME := PWideChar(PRawData);
        Inc(PRawData, (Length(AData.TileMIME) + 1) * SizeOf(WideChar));

        if AData.TileSize > 0 then begin
          AData.TileBody := PRawData;
        end;

        Result := True;
      end;
    end else begin
      //ToDo: BDBRaiseException(
      raise Exception.Create(
        AnsiString('Error [BerkeleyDB]: Bad magic value (') +
        AData.RecMagic[0] +
        AData.RecMagic[1] +
        AData.RecMagic[2] +
        AData.RecMagic[3] +
        ')'
      );
    end;
  end;
end;

function PBDBMetaInfoToMemStream(
  AMeta: PBDBStorageMetaInfo;
  out AStream: TMemoryStream
): Boolean;
begin
  Move(CBDBMetaMagic[0], AMeta.MetaMagic[0], Length(AMeta.MetaMagic));
  AMeta.MetaCRC32 := 0;
  AStream.Clear;
  AStream.Position := 0;
  AStream.WriteBuffer(AMeta.MetaMagic[0], Length(AMeta.MetaMagic));
  AStream.WriteBuffer(AMeta.MetaCRC32, SizeOf(AMeta.MetaCRC32));
  AStream.WriteBuffer(AMeta.StorageEPSG, SizeOf(AMeta.StorageEPSG));
  AStream.Position := 0;
  AMeta.MetaCRC32 := CRC32Buf(AStream.Memory, AStream.Size);
  AStream.Position := Length(AMeta.MetaMagic);
  AStream.WriteBuffer(AMeta.MetaCRC32, SizeOf(AMeta.MetaCRC32));
  AStream.Position := 0;
  Result := True;
end;

function PRawMetaToPBDBMetaInfo(
  ARawData: PByte;
  ARawDataSize: Cardinal;
  AMeta: PBDBStorageMetaInfo
): Boolean;
var
  PRawData: PByte;
  VCRC32: Cardinal;
begin
  Result := False;
  if (AMeta <> nil) and (ARawData <> nil) then begin
    FillChar(AMeta^, SizeOf(TBDBStorageMetaInfo), 0);
    PRawData := ARawData;
    Move(PRawData^, AMeta.MetaMagic[0], Length(AMeta.MetaMagic));
    Inc(PRawData, Length(AMeta.MetaMagic));
    if AMeta.MetaMagic = CBDBMetaMagic then begin
      AMeta.MetaCRC32 := PCardinal(PRawData)^;
      PCardinal(PRawData)^ := 0;
      VCRC32 := CRC32Buf(ARawData, ARawDataSize);
      PCardinal(PRawData)^ := AMeta.MetaCRC32;
      if VCRC32 = AMeta.MetaCRC32 then begin
        Inc(PRawData, SizeOf(AMeta.MetaCRC32));
        AMeta.StorageEPSG := PInteger(PRawData)^;
        Result := True;
      end;
    end;
  end;
end;

end.
