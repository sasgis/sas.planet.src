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

unit i_MapAttachmentsInfo;

interface

uses
  i_StringByLanguage;

type
  IMapAttachmentsInfo = interface(IStringByLanguage)
    ['{1147F9EE-1F71-4A53-8EC2-E77820221066}']
    function GetGUID: TGUID; stdcall;
    property GUID: TGUID read GetGUID;

    function GetMaxSubIndex: Integer; stdcall;
    property MaxSubIndex: Integer read GetMaxSubIndex;

    function GetParseNumberAfter: String; stdcall;

    function GetNameInCache(const AIndex: Integer): String; stdcall;
    function GetExt(const AIndex: Integer): String; stdcall;
    function GetEnabled(const AIndex: Integer): Boolean; stdcall;
    function GetDefURLBase(const AIndex: Integer): String; stdcall;
    function GetContentType(const AIndex: Integer): String; stdcall;

    function GetUseDwn: Boolean; stdcall;
    function GetUseDel: Boolean; stdcall;
  end;

implementation

end.
