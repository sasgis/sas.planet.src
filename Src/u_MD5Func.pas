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

unit u_MD5Func;

interface

function MD5Buffer(const ABuffer; ASize: Integer): RawByteString; // do not inline
function MD5BufferLower(const ABuffer; ASize: Integer): RawByteString; // do not inline

implementation

uses
  mormot.core.base,
  mormot.core.text,
  mormot.crypt.core;

function MD5Buffer(const ABuffer; ASize: Integer): RawByteString;
var
  VDigest: TMd5Digest;
begin
  VDigest := Md5Buf(ABuffer, ASize);
  Result := BinToHex(@VDigest, SizeOf(VDigest));
end;

function MD5BufferLower(const ABuffer; ASize: Integer): RawByteString;
var
  VDigest: TMd5Digest;
begin
  VDigest := Md5Buf(ABuffer, ASize);
  Result := BinToHexLower(@VDigest, SizeOf(VDigest));
end;

end.
