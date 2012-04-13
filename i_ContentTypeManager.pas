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

unit i_ContentTypeManager;

interface

uses
  i_ContentTypeInfo,
  i_ContentConverter;

type
  IContentTypeManager = interface
    ['{157D7F4C-BBBB-4617-A0D1-250D066B4C2C}']
    function GetInfo(const AType: WideString): IContentTypeInfoBasic;
    function GetInfoByExt(const AExt: WideString): IContentTypeInfoBasic;
    function GetIsBitmapType(const AType: WideString): Boolean;
    function GetIsBitmapExt(const AExt: WideString): Boolean;
    function GetIsKmlType(const AType: WideString): Boolean;
    function GetIsKmlExt(const AExt: WideString): Boolean;
    function GetConverter(const ATypeSource, ATypeTarget: WideString): IContentConverter;
  end;


implementation

end.
