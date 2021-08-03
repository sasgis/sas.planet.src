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

unit u_HashFunctionCRC64;

interface

uses
  t_Hash,
  i_HashFunctionImpl,
  u_BaseInterfacedObject;

// взято отсюда http://www.delphisources.ru/pages/faq/base/hash_crc64.html
type
  THashFunctionCRC64 = class(TBaseInterfacedObject, IHashFunctionImpl)
  private
    T: array[Byte] of UInt64;
  private
    function CalcHash(
      ABuffer: Pointer;
      ASize: Integer
    ): THashValue;
    function CalcHashWithSeed(
      ABuffer: Pointer;
      ASize: Integer;
      const ASeed: THashValue
    ): THashValue;
  public
    constructor Create;
  end;

implementation

{ THashFunctionCRC64 }

function THashFunctionCRC64.CalcHash(
  ABuffer: Pointer;
  ASize: Integer
): THashValue;
begin
  Result := not THashValue(0);
  Result := CalcHashWithSeed(ABuffer, ASize, Result);
end;

function THashFunctionCRC64.CalcHashWithSeed(
  ABuffer: Pointer;
  ASize: Integer;
  const ASeed: THashValue
): THashValue;
var
  MyCRC64: UInt64;
  I: Cardinal;
  PData: ^Byte;
begin
  PData := ABuffer;
  MyCRC64 := ASeed;
  for I := 1 to ASize do begin
    MyCRC64 := MyCRC64 shr 8 xor T[Cardinal(MyCRC64) and $FF xor PData^];
    Inc(PData);
  end;
  Result := MyCRC64;
end;

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
    T[I] := D;
  end;
end;

end.
