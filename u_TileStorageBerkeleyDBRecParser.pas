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
  PBDBKey = ^TBDBKey;

  TBDBKey = packed record
    TileX: Cardinal;
    TileY: Cardinal;
  end;

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

const
  CBDBMetaKeyX: Cardinal = $FFFFFFFF;
  CBDBMetaKeyY: Cardinal = $FFFFFFFF;

function PointToKey(APoint: TPoint): TBDBKey;

function KeyToPoint(const AKey: TBDBKey): TPoint;

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
  libdb51,
  CRC32,
  u_BerkeleyDBErrorHandler;

const
  CBDBMetaVersion = #01;
  CBDBMetaMagic: array [0..3] of AnsiChar = ('M', 'I', 'D', CBDBMetaVersion);    //Meta Info Data

  CBDBRecVersion = #03;
  CBDBRecMagic: array [0..3] of AnsiChar = ('T', 'L', 'D', CBDBRecVersion);      //TiLe Data

procedure SetBit(
  var ADest: Cardinal;
  const ABit: Integer
); inline;
begin
  ADest := ADest or (Cardinal(1) shl ABit);
end;

function Swap32(Value: Cardinal): Cardinal; assembler;
asm
  bswap eax
end;

function PointToKey(APoint: TPoint): TBDBKey;
var
  I: Integer;
  VSetX, VSetY: Boolean;
begin
  Result.TileX := 0;
  Result.TileY := 0;
  for I := 0 to 31 do begin
    VSetX := ((APoint.X shr I) and 1) = 1;
    VSetY := ((APoint.Y shr I) and 1) = 1;
    if I <= 15 then begin
      if VSetX then begin
        SetBit(Result.TileY, I * 2);
      end;
      if VSetY then begin
        SetBit(Result.TileY, I * 2 + 1);
      end;
    end else begin
      if VSetX then begin
        SetBit(Result.TileX, (I - 16) * 2);
      end;
      if VSetY then begin
        SetBit(Result.TileX, (I - 16) * 2 + 1);
      end;
    end;
  end;
  Result.TileX := Swap32(Result.TileX);
  Result.TileY := Swap32(Result.TileY);
end;

function KeyToPoint(const AKey: TBDBKey): TPoint;

  procedure ValueToPoint(
  const AValue: Cardinal;
    AOffset: Integer;
    out APoint: TPoint
  );
  var
    I: Integer;
    VPoint: TBDBKey;
  begin
    VPoint.TileX := APoint.X;
    VPoint.TileY := APoint.Y;
    for I := 0 to 31 do begin
      if ((AValue shr I) and 1) = 1 then begin
        if (I mod 2 = 0) then begin
          SetBit(VPoint.TileX, (I + AOffset) div 2);
        end else begin
          SetBit(VPoint.TileY, (I + AOffset - 1) div 2);
        end;
      end;
    end;
    APoint.X := VPoint.TileX;
    APoint.Y := VPoint.TileY;
  end;

begin
  Result := Point(0, 0);
  ValueToPoint(Swap32(AKey.TileY), 0, Result);
  ValueToPoint(Swap32(AKey.TileX), 32, Result);
end;

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
      BDBRaiseException(
        'Error [BerkeleyDB]: Bad magic value (' +
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
