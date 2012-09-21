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

uses
  Types;

const
  // success
  ETS_RESULT_OK = 0;

  // repeat command exclusively
  ETS_RESULT_NEED_EXCUSIVE = 1;

  // service storage is suspended (treat as TNE for SHOW and ABORT for EXPORT)
  ETS_RESULT_SUSPENDED = 2;

  // service is in read-only mode (cannot execute insert, update or create)
  ETS_RESULT_READ_ONLY = 3;

  // auth errors (4-7)
  ETS_RESULT_AUTH_REQUIRED  = 4; // no auth info but required
  ETS_RESULT_AUTH_CANCELLED = 5; // user cancels auth
  ETS_RESULT_AUTH_FAILED    = 6; // auth error

  // connectivity and transport errors (8-16)
  ETS_RESULT_NOT_CONNECTED = 9;
  ETS_RESULT_DISCONNECTED  = 10;

  // invalid values (32-63)
  // if id is smallint and set greater than smallint
  ETS_RESULT_ID_RANGE_ERROR = 33;
  // length of Nst string parameter too big
  ETS_RESULT_STRING1_LEN = 34;
  ETS_RESULT_STRING2_LEN = 35;
  ETS_RESULT_STRING3_LEN = 36;
  ETS_RESULT_STRING4_LEN = 37;

  // unknown value (64-127)
  ETS_RESULT_UNKNOWN_SERVICE     = 65;
  ETS_RESULT_UNKNOWN_VERSION     = 66;
  ETS_RESULT_UNKNOWN_VER_COMP    = 67;
  ETS_RESULT_UNKNOWN_DIV_MODE    = 68;
  ETS_RESULT_UNKNOWN_CONTENTTYPE = 69;


const
  TILE_VERSION_COMPARE_NONE   = '0'; // tile version non-comparable
  TILE_VERSION_COMPARE_ID     = 'I'; // compare using version identifier (smallint or int)
  TILE_VERSION_COMPARE_VALUE  = 'V'; // compare using version value (string)
  TILE_VERSION_COMPARE_DATE   = 'D'; // compare using version date (datetime)
  TILE_VERSION_COMPARE_NUMBER = 'N'; // compare using version number (int)

  // how to divide tiles between tables (mask width)
  TILE_DIV_NONE  = 'Z'; // all-in-one       (0) - reserved!
  TILE_DIV_1024  = 'D'; // div by mod 1024 (10) - internal only
  TILE_DIV_2048  = 'F'; // div by mod 2048 (11) - internal only
  TILE_DIV_4096  = 'G'; // div by mod 4096 (12) - internal only
  TILE_DIV_8192  = 'J'; // div by mod 8192 (13) - internal only
  TILE_DIV_16384 = 'N'; // div by mod 16384 (14) - internal only
  TILE_DIV_32768 = 'Q'; // div by mod 32768 (15) - internal only

  // how to load tiles with another version or without version
  ETS_TLM_WITHOUT_VERSION = $00000001; // allow (without version) if (request with version) and (no tiles with any version)
  ETS_TLM_PREV_VERSION    = $00000002; // allow (prev version) if (request with version) and (tile not found)
  ETS_TLM_LAST_VERSION    = $00000004; // allow (last version) if (request without version) and (tile without version not found)

  // how to save tiles
  ETS_TSM_MAKE_VERSIONS   = $00000001; // allow to create new version on saving tile with unknown version

  // how to work with service
  ETS_SWM_DEFAULT   = '0'; // default mode
  ETS_SWM_SUSPENDED = 'S'; // temporary suspended
  ETS_SWM_READ_ONLY = 'R'; // read-only (insert update and create are denied)

  // allow to use common tiles
  ETS_UCT_YES = '1';
  ETS_UCT_NO  = '0';

type
  // only numeric xyz
  TTILE_ID_XYZ = packed record
    xy: TPoint;
    z: Byte;
  end;
  PTILE_ID_XYZ = ^TTILE_ID_XYZ;

const
  // delete tile, tne or both (for DEL)
  ETS_DELETE_ALL  = $00000000; // same as BOTH
  ETS_DELETE_TILE = $00000001;
  ETS_DELETE_TNE  = $00000002;
  ETS_DELETE_BOTH = $00000004; // same as ALL

  // input options (for GET)
  ETS_TBI_WITH_BODY  = $00000001; // with tile body (or query info only)

  // output options
  ETS_TBO_TILE_EXISTS  = $00000001; // tile found
  ETS_TBO_TNE_EXISTS   = $00000002; // TNE marker found
  ETS_TBO_COMMON       = $00000004; // common tile
  ETS_TBO_CRC32        = $00000008; // actual CRC32

type
  // tile information buffer type
  TETS_TILE_BUFFER = packed record
    dwOptionsIn: LongWord;  // ETS_TBI_* constants (for GET)
    dwOptionsOut: LongWord; // ETS_TBO_* constants (for GET)
    ptrTileBuffer: Pointer; // Optional
    dwTileSize: LongWord; // Mandatory (treat <0 as TNE too!)
    dtLoadedUTC: TDateTime; // Mandatory (UTC only!)
    dwCRC32: LongWord; // Optional
    wVersionIn: LongInt; // Mandatory (Int or SmallInt)
    wVersionOut: LongInt; // Mandatory (Int or SmallInt)
    wContentType: LongInt; // Mandatory (Int or SmallInt)
  end;
  PETS_TILE_BUFFER = ^TETS_TILE_BUFFER;

  // buffers for callbacks
  
  // version buffer type
  // ver_date, ver_number and other fields are omitted here
  TETS_VERSION_W = packed record
    id_ver: LongInt; // version identifier  (Int or SmallInt)
    ver_value: PWideChar; // version value (unique string, not NULL)
    ver_comment: PWideChar; // version comment (any string, allow NULL)
  end;
  PETS_VERSION_W = ^TETS_VERSION_W;

  TETS_VERSION_A = packed record
    id_ver: LongInt; // version identifier  (Int or SmallInt)
    ver_value: PAnsiChar; // version value (unique string, not NULL)
    ver_comment: PAnsiChar; // version comment (any string, allow NULL)
  end;
  PETS_VERSION_A = ^TETS_VERSION_A;


  // contenttype buffer type
  TETS_CONTENTTYPE_W = packed record
    id_contenttype: LongInt; // version identifier  (Int or SmallInt)
    contenttype_text: PWideChar; // version value (unique string, not NULL)
  end;
  PETS_CONTENTTYPE_W = ^TETS_CONTENTTYPE_W;
  
  TETS_CONTENTTYPE_A = packed record
    id_contenttype: LongInt; // version identifier  (Int or SmallInt)
    contenttype_text: PAnsiChar; // version value (unique string, not NULL)
  end;
  PETS_CONTENTTYPE_A = ^TETS_CONTENTTYPE_A;


  // version_compare buffer type
  TETS_VER_COMP_W = packed record
    id_ver_comp: AnsiChar; // ver_comp identifier  (always Ansi!)
    ver_comp_field: PWideChar; // ver_comp field value (unique string, not NULL)
    ver_comp_name: PWideChar; // ver_comp name for GUI (unique string, not NULL)
  end;
  PETS_VER_COMP_W = ^TETS_VER_COMP_W;

  TETS_VER_COMP_A = packed record
    id_ver_comp: AnsiChar; // ver_comp identifier  (always Ansi!)
    ver_comp_field: PAnsiChar; // ver_comp field value (unique string, not NULL)
    ver_comp_name: PAnsiChar; // ver_comp name for GUI (unique string, not NULL)
  end;
  PETS_VER_COMP_A = ^TETS_VER_COMP_A;


  // tile_div_mode buffer type
  // div_mask_width is omitted here
  TETS_DIV_MODE_W = packed record
    id_div_mode: AnsiChar; // div_mode identifier  (always Ansi!)
    div_mode_name: PWideChar; // div_mode name for GUI (unique string, not NULL)
  end;
  PETS_DIV_MODE_W = ^TETS_DIV_MODE_W;

  TETS_DIV_MODE_A = packed record
    id_div_mode: AnsiChar; // div_mode identifier  (always Ansi!)
    div_mode_name: PAnsiChar; // div_mode name for GUI (unique string, not NULL)
  end;
  PETS_DIV_MODE_A = ^TETS_DIV_MODE_A;


  // buffer to set service options to storage
  TETS_SERVICE_STORAGE_OPTIONS = packed record
    tile_load_mode: LongWord; // ETS_TLM_* constants
    tile_save_mode: LongWord; // ETS_TSM_* constants
    id_div_mode: AnsiChar; // how to divide tiles into tables
    id_ver_comp: AnsiChar; // type of version numbers comparator
    work_mode: AnsiChar; //  ETS_SWM_* constants
    use_common_tiles: AnsiChar; //  ETS_UCT_* constants
  public
    procedure Clear;
  end;
  PETS_SERVICE_STORAGE_OPTIONS = ^TETS_SERVICE_STORAGE_OPTIONS;

implementation

{ TETS_SERVICE_STORAGE_OPTIONS }

procedure TETS_SERVICE_STORAGE_OPTIONS.Clear;
begin
  FillChar(Self, sizeof(Self), 0);
  id_div_mode := TILE_DIV_NONE;
  id_ver_comp := TILE_VERSION_COMPARE_NONE;
  work_mode := ETS_SWM_DEFAULT;
  use_common_tiles := ETS_UCT_NO;
end;

end.
