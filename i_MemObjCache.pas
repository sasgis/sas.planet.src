{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit i_MemObjCache;

interface

uses
  GR32,
  i_VectorDataItemSimple;

type
  IMemObjCacheBitmap = interface
    ['{1CF92025-6BA9-4264-91E8-73766E698A6D}']
    procedure Clear;
    procedure DeleteFileFromCache(AKey: string);
    procedure AddTileToCache(AObj: TCustomBitmap32; AKey: string);
    function TryLoadFileFromCache(AObj: TCustomBitmap32; AKey: string): boolean;
  end;

  IMemObjCacheVector = interface
    ['{0BB1598E-A00C-4BBE-9AA8-08F94974EAB2}']
    procedure Clear;
    procedure DeleteFileFromCache(AKey: string);
    procedure AddTileToCache(AObj: IVectorDataItemList; AKey: string);
    function TryLoadFileFromCache(var AObj: IVectorDataItemList; AKey: string): boolean;
  end;

implementation

end.
 