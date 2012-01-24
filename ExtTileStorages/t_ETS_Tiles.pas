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

unit t_ETS_Tiles;

interface

const
  // tile identifiers' formats (for input)
  TILE_ID_FORMAT_NONE = $00000000; // undefined format
  TILE_ID_FORMAT_XYZ  = $00000001; // simple xyz fields = int,int,byte
  TILE_ID_FORMAT_0123 = $00000002; // divide single tile into 0-1-2-3-numbered parts at every zoom = char(24)
  TILE_ID_FORMAT_ABC4 = $00000004; // combine 0123 by 2 symbols, calc (4i+j) and map by ABCD...XYZ array = char(12)
  TILE_ID_FORMAT_MASK = (TILE_ID_FORMAT_XYZ or TILE_ID_FORMAT_0123 or TILE_ID_FORMAT_ABC4); // mask

  // size in chars
  TILE_ID_FORMAT_0123_SIZE = 24;
  TILE_ID_FORMAT_ABC4_SIZE = 12;

type
  // only numeric xyz
  TTILE_ID_XYZ = packed record
    x: LongInt;
    y: LongInt;
    z: Byte;
  end;
  PTILE_ID_XYZ = ^TTILE_ID_XYZ;

  // aka Quadkey
  TTILE_ID_0123 = packed record
    id0123: packed array [0..TILE_ID_FORMAT_0123_SIZE] of AnsiChar; // +1 char for zero, only '0'-'3' allowed
  end;
  PTILE_ID_0123 = ^TTILE_ID_0123;

  TTILE_ID_ABC4 = packed record
    idABC4: packed array [0..TILE_ID_FORMAT_ABC4_SIZE] of AnsiChar; // +1 char for zero, only '0'-'3' and 'A'-'P' allowed
  end;
  PTILE_ID_ABC4 = ^TTILE_ID_ABC4;

  // routine (from EXE)
  TETS_TileId_Conversion_Routine = function(const ASrcFormat: LongWord;
                                            const ASrcData: Pointer;
                                            const ADstFormat: LongWord;
                                            const ADstData: Pointer): LongInt; stdcall;

const
  // delete tile, tne or both (for "delete" operation)
  ETS_DELETE_TILE = $00000001;
  ETS_DELETE_TNE  = $00000002;

  // input options (for GET)
  ETS_TBI_QUERY_INFO   = $00000001; // get only tile information (do not retrieve tile body)
  ETS_TBI_PREV_VERSION = $00000002; // allow [get tile with prev version] if [no tile with special version found]
  ETS_TBI_LAST_VERSION = $00000004; // allow [get tile with last version] if [request without version] and [no tile without version found]
  ETS_TBI_NO_VERSION   = $00000008; // allow [get tile without version] if [request with version] and [no tile with versions found]

  // output options
  ETS_TBO_TILE_EXISTS = $00000001; // tile found
  ETS_TBO_TNE_EXISTS  = $00000002; // TNE marker found
  ETS_TBO_CRC32       = $00000004; // got CRC32 in corresponding field

type
  // tile information buffer type
  TETS_TILE_BUFFER = packed record
    dwOptionsIn: LongWord;  // ETS_TBI_* constants (for GET)
    dwOptionsOut: LongWord; // ETS_TBO_* constants (for GET)
    ptrTileBuffer: Pointer;
    dwTileSize: LongWord;
    dtLoadedUTC: TDateTime;
    dwCRC32: LongWord;
  end;
  PETS_TILE_BUFFER = ^TETS_TILE_BUFFER;

  // tile version buffer type (unicode version)
  TETS_TILE_VERSION_W = packed record
    szVersion: PWideChar;     // if NULL - no tile or tile without version
    szContentType: PWideChar; // if NULL - use global storage content_type
  end;
  PETS_TILE_VERSION_W = ^TETS_TILE_VERSION_W;

  // tile version buffer type (ansi version)
  TETS_TILE_VERSION_A = packed record
    szVersion: PAnsiChar;     // if NULL - no tile or tile without version
    szContentType: PAnsiChar; // if NULL - use global storage content_type
  end;
  PETS_TILE_VERSION_A = ^TETS_TILE_VERSION_A;


implementation

end.