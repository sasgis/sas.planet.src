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

unit i_ContentTypeManager;

interface

uses
  i_StringListStatic,
  i_BitmapTileSaveLoad,
  i_ContentTypeInfo,
  i_ContentConverter;

type
  IContentTypeManagerBitmap = interface
    ['{B7B3EBF5-60DB-434D-B854-383CE0E752E7}']
    function GetIsBitmapType(const AType: AnsiString): Boolean;
    function GetBitmapLoaderByFileName(const AFileName: string): IBitmapTileLoader;
    function GetIsBitmapExt(const AExt: AnsiString): Boolean;
  end;

type
  IContentTypeManager = interface
    ['{157D7F4C-BBBB-4617-A0D1-250D066B4C2C}']
    function GetInfo(const AType: AnsiString): IContentTypeInfoBasic;
    function GetInfoByExt(const AExt: AnsiString): IContentTypeInfoBasic;
    function GetIsBitmapType(const AType: AnsiString): Boolean;
    function GetBitmapLoaderByFileName(const AFileName: string): IBitmapTileLoader;
    function GetIsBitmapExt(const AExt: AnsiString): Boolean;
    function GetIsVectorType(const AType: AnsiString): Boolean;
    function GetIsVectorExt(const AExt: AnsiString): Boolean;
    function GetConverter(const ATypeSource, ATypeTarget: AnsiString): IContentConverter;
    function GetKnownExtList: IStringListStatic;
  end;


implementation

end.
