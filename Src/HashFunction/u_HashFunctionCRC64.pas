{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2021, SAS.Planet development team.                      *}
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

unit u_HashFunctionCRC64;

interface

uses
  t_Hash,
  i_HashFunctionImpl,
  u_BaseInterfacedObject;

// Source: http://www.delphisources.ru/pages/faq/base/hash_crc64.html

type
  THashFunctionCRC64 = class(TBaseInterfacedObject, IHashFunctionImpl)
  private
    FTable: array [Byte] of UInt64;
  private
    { IHashFunctionImpl }
    function CalcHash(
      const ABuffer: Pointer;
      const ASize: Integer
    ): THashValue;

    function CalcHashWithSeed(
      const ABuffer: Pointer;
      const ASize: Integer;
      const ASeed: THashValue
    ): THashValue;
  public
    constructor Create;
  end;

implementation

{ THashFunctionCRC64 }

constructor THashFunctionCRC64.Create;
var
  I, J: Byte;
  D: UInt64;
begin
  inherited Create;

  for I := 0 to 255 do begin
    D := I;
    for J := 1 to 8 do begin
      if Odd(D) then begin
        D := D shr 1 xor $C96C5795D7870F42;
      end else begin
        D := D shr 1;
      end;
    end;
    FTable[I] := D;
  end;
end;

function THashFunctionCRC64.CalcHash(
  const ABuffer: Pointer;
  const ASize: Integer
): THashValue;
begin
  Result := not THashValue(0);
  Result := CalcHashWithSeed(ABuffer, ASize, Result);
end;

function THashFunctionCRC64.CalcHashWithSeed(
  const ABuffer: Pointer;
  const ASize: Integer;
  const ASeed: THashValue
): THashValue;
var
  I: Integer;
  VData: PByte;
  VCRC64: UInt64;
begin
  VData := ABuffer;
  VCRC64 := ASeed;
  for I := 0 to ASize - 1 do begin
    VCRC64 := VCRC64 shr 8 xor FTable[Cardinal(VCRC64) and $FF xor VData^];
    Inc(VData);
  end;
  Result := VCRC64;
end;

end.
