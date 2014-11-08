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

unit u_BerkeleyDBValueZlib;

interface

procedure ZlibCompress(const AInData: Pointer; const AInSize: Integer; out AOutData: Pointer; out AOutSize: Integer);
function ZlibDecompress(const AInData: Pointer; const AInSize: Integer; out AOutData: Pointer; out AOutSize: Integer): Boolean;

implementation

uses
  ALZLibEx;

const
  cBerkeleyValueZlibMagic: Cardinal = $4C41565A; // ZVAL (Zlib VALue)

procedure ZlibCompress(const AInData: Pointer; const AInSize: Integer; out AOutData: Pointer; out AOutSize: Integer);
var
  VPtr: PByte;
  VZlibData: Pointer;
  VZlibDataSize: Integer;
begin
  ZCompress(AInData, AInSize, VZlibData, VZlibDataSize);
  try
    AOutSize := VZlibDataSize + 8;
    AOutData := GetMemory(AOutSize);
    VPtr := AOutData;
    PCardinal(VPtr)^ := cBerkeleyValueZlibMagic;
    Inc(VPtr, 4);
    PInteger(VPtr)^ := AInSize;
    Inc(VPtr, 4);
    Move(VZlibData^, VPtr^, VZlibDataSize);
  finally
    FreeMemory(VZlibData);
  end;
end;


function ZlibDecompress(const AInData: Pointer; const AInSize: Integer; out AOutData: Pointer; out AOutSize: Integer): Boolean;
var
  VPtr: PByte;
  VUnconpressSize: Integer;
begin
  Result := False;
  AOutData := nil;
  AOutSize := 0;
  VPtr := AInData;
  if (AInSize > 8) and (PCardinal(VPtr)^ = cBerkeleyValueZlibMagic) then begin
    Inc(VPtr, 4);
    VUnconpressSize := PInteger(VPtr)^;
    Inc(VPtr, 4);
    ZDecompress(VPtr, (AInSize - 8), AOutData, AOutSize, VUnconpressSize);
    Result := True;
  end else begin
    // error
  end;
end;

end.
