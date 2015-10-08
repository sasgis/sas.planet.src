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

unit i_BitmapTileMatrixBuilder;

interface

uses
  Types,
  i_TileRect,
  i_Bitmap32Static,
  i_BitmapTileMatrix;

type
  IBitmapTileMatrixBuilder = interface
    ['{805E09A4-7B44-4FFF-BC78-B4D3BBE27990}']
    function GetTileRect: ITileRect;
    property TileRect: ITileRect read GetTileRect;

    function GetTile(const ATile: TPoint): IBitmap32Static;
    procedure SetTile(
      const ATile: TPoint;
      const AValue: IBitmap32Static
    );
    property Tiles[const ATile: TPoint]: IBitmap32Static read GetTile write SetTile;

    procedure SetRectWithReset(const ATileRect: ITileRect);
    procedure SetRect(const ATileRect: ITileRect);

    function MakeStatic: IBitmapTileMatrix;
  end;

implementation

end.
