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

unit i_TileInfoBasicMemCache;

interface

uses
  Types,
  i_MapVersionInfo,
  i_TileInfoBasic;

type
  ITileInfoBasicMemCache = interface
    ['{9D01F466-1089-46D7-B13B-1ED1116A8376}']
    procedure Add(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const ATileInfoBasic: ITileInfoBasic
    );

    procedure Remove(
      const AXY: TPoint;
      const AZoom: Byte
    );

    function Get(
      const AXY: TPoint;
      const AZoom: Byte;
      const AUpdateTTL: Boolean
    ): ITileInfoBasic;

    procedure Clear;
  end;

implementation

end.
