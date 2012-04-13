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

unit t_ETS_Tiles;

interface

const
  // tile identifiers' formats (for input)
  TILE_ID_FORMAT_NONE = $00000000; // undefined format
  TILE_ID_FORMAT_XYZ  = $00000001; // simple xyz fields = int,int,byte
  TILE_ID_FORMAT_0123 = $00000002; // divide single tile into 0-1-2-3-numbered parts at every zoom = char(24)
  TILE_ID_FORMAT_AHP1 = $00000004; // combine 0123 by 2 symbols, calc (4i+j) and map by ABCD...XYZ array = char(12)
  TILE_ID_FORMAT_MASK = (TILE_ID_FORMAT_XYZ or TILE_ID_FORMAT_0123 or TILE_ID_FORMAT_AHP1); // mask

  // size in chars
  TILE_ID_FORMAT_0123_SIZE = 24;
  TILE_ID_FORMAT_AHP1_SIZE = 12;

  // types of tile_id in underlaying storage
  TILE_ID_TYPE_XYZ_2BYTE = '0'; // z and hiword(x) and hiword(y) go to tablename, loword(x) and loword(y) go to tile_id
  TILE_ID_TYPE_0123_16CH = 'Q'; // up to 16 right chars go to file_id, left chars (<=8) go to tablename
  TILE_ID_TYPE_AHP1_8CHR = 'H'; // up to 8 right chars go to file_id, left chars (<=4) go to tablename

  TILE_VERSION_COMPARE_NONE = '0'; // tile version non-comparable
  TILE_VERSION_COMPARE_NAME = 'A'; // compare using version name (string)
  TILE_VERSION_COMPARE_DATE = 'D'; // compare using version date and time
  TILE_VERSION_COMPARE_INT1 = '1'; // compare using int1 field

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

  TTILE_ID_AHP1 = packed record
    idAHP1: packed array [0..TILE_ID_FORMAT_AHP1_SIZE] of AnsiChar; // +1 char for zero, only '0'-'3' and 'A'-'P' allowed
  end;
  PTILE_ID_AHP1 = ^TTILE_ID_AHP1;

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
  ETS_TBO_TILE_EXISTS  = $00000001; // tile found
  ETS_TBO_TNE_EXISTS   = $00000002; // TNE marker found
  ETS_TBO_DDL_REQUIRED = $00000004; // DLL command required (such as "create table")
  ETS_TBO_CRC32        = $00000008; // got CRC32 in corresponding field

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

  // buffer to set source options to storage
  TETS_SOURCE_STORAGE_OPTIONS = packed record
    tileid_type: AnsiChar; // type of tile_id generator
    ver_comp: AnsiChar; // type of version numbers comparator
  end;
  PETS_SOURCE_STORAGE_OPTIONS = ^TETS_SOURCE_STORAGE_OPTIONS;

procedure Init_TETS_SOURCE_STORAGE_OPTIONS(p: PETS_SOURCE_STORAGE_OPTIONS);

implementation

procedure Init_TETS_SOURCE_STORAGE_OPTIONS(p: PETS_SOURCE_STORAGE_OPTIONS);
begin
  FillChar(p^, sizeof(p^), 0);
  p^.tileid_type:=TILE_ID_TYPE_XYZ_2BYTE;
  p^.ver_comp:=TILE_VERSION_COMPARE_NONE;
end;

end.