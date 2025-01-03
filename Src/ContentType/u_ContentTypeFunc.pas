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

unit u_ContentTypeFunc;

interface

uses
  i_ContentTypeInfo;

function IsBmpContentType(const AContentTypeInfo: IContentTypeInfoBasic): Boolean; overload; inline;
function IsBmpContentType(const AContentType: AnsiString): Boolean; overload;

function IsGifContentType(const AContentTypeInfo: IContentTypeInfoBasic): Boolean; overload; inline;
function IsGifContentType(const AContentType: AnsiString): Boolean; overload;

function IsWebpContentType(const AContentTypeInfo: IContentTypeInfoBasic): Boolean; overload; inline;
function IsWebpContentType(const AContentType: AnsiString): Boolean; overload;

function IsJpegContentType(const AContentTypeInfo: IContentTypeInfoBasic): Boolean; overload; inline;
function IsJpegContentType(const AContentType: AnsiString): Boolean; overload;

function IsPngContentType(const AContentTypeInfo: IContentTypeInfoBasic): Boolean; overload; inline;
function IsPngContentType(const AContentType: AnsiString): Boolean; overload;

implementation

uses
  u_AnsiStr;

function IsBmpContentType(const AContentType: AnsiString): Boolean;
begin
  Result := SameTextA(AContentType, 'image/bmp');
end;

function IsBmpContentType(const AContentTypeInfo: IContentTypeInfoBasic): Boolean;
begin
  Result := IsBmpContentType(AContentTypeInfo.GetContentType);
end;

function IsGifContentType(const AContentType: AnsiString): Boolean;
begin
  Result := SameTextA(AContentType, 'image/gif');
end;

function IsGifContentType(const AContentTypeInfo: IContentTypeInfoBasic): Boolean;
begin
  Result := IsGifContentType(AContentTypeInfo.GetContentType);
end;

function IsWebpContentType(const AContentType: AnsiString): Boolean;
begin
  Result := SameTextA(AContentType, 'image/webp');
end;

function IsWebpContentType(const AContentTypeInfo: IContentTypeInfoBasic): Boolean;
begin
  Result := IsWebpContentType(AContentTypeInfo.GetContentType);
end;

function IsJpegContentType(const AContentType: AnsiString): Boolean;
begin
  Result :=
    SameTextA(AContentType, 'image/jpg') or
    SameTextA(AContentType, 'image/jpeg') or
    SameTextA(AContentType, 'image/pjpeg');
end;

function IsJpegContentType(const AContentTypeInfo: IContentTypeInfoBasic): Boolean;
begin
  Result := IsJpegContentType(AContentTypeInfo.GetContentType);
end;

function IsPngContentType(const AContentType: AnsiString): Boolean;
begin
  Result := SameTextA(AContentType, 'image/png');
end;

function IsPngContentType(const AContentTypeInfo: IContentTypeInfoBasic): Boolean;
begin
  Result := IsPngContentType(AContentTypeInfo.GetContentType);
end;

end.
