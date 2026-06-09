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

unit u_Base64Func;

interface

function Base64Encode(const AData: RawByteString): RawByteString; overload; inline;
function Base64UrlEncode(const AData: RawByteString): RawByteString; overload; inline;
function Base64Decode(const AData: RawByteString): RawByteString; overload; inline;

function Base64Encode(const AData: PAnsiChar; const ASize: Integer): RawByteString; overload; // do not inline
function Base64UrlEncode(const AData: PAnsiChar; const ASize: Integer): RawByteString; overload; // do not inline
function Base64Decode(const AData: PAnsiChar; const ASize: Integer): RawByteString; overload; // do not inline

implementation

uses
  mormot.core.buffers;

function Base64Encode(const AData: PAnsiChar; const ASize: Integer): RawByteString;
begin
  Result := BinToBase64(AData, ASize);
end;

function Base64UrlEncode(const AData: PAnsiChar; const ASize: Integer): RawByteString;
begin
  Result := BinToBase64uri(AData, ASize);
end;

function Base64Decode(const AData: PAnsiChar; const ASize: Integer): RawByteString;
begin
  Result := Base64ToBin(AData, ASize);
end;

function Base64Encode(const AData: RawByteString): RawByteString;
begin
  Result := Base64Encode(PAnsiChar(AData), Length(AData));
end;

function Base64UrlEncode(const AData: RawByteString): RawByteString;
begin
  Result := Base64UrlEncode(PAnsiChar(AData), Length(AData));
end;

function Base64Decode(const AData: RawByteString): RawByteString;
begin
  Result := Base64Decode(PAnsiChar(AData), Length(AData));
end;

end.
