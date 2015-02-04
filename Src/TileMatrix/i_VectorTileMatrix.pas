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

unit i_VectorTileMatrix;

interface

uses
  Types,
  t_Hash,
  i_TileRect,
  i_VectorItemSubset;

type
  IVectorTileMatrix = interface
    ['{3091D77B-D0B9-433D-B7CC-0D850C720AD5}']
    function GetHash: THashValue;
    property Hash: THashValue read GetHash;

    function GetTileRect: ITileRect;
    property TileRect: ITileRect read GetTileRect;

    function GetElementByTile(const ATile: TPoint): IVectorItemSubset;

    function GetItem(AX, AY: Integer): IVectorItemSubset;
    property Items[AX, AY: Integer]: IVectorItemSubset read GetItem;
  end;

implementation

end.
