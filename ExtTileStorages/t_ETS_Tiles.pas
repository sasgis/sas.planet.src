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
  ETS_RESULT_NEED_EXCLUSIVE = 1;

  // not implemented
  ETS_RESULT_NOT_IMPLEMENTED = 2;


  
  // access to storage is restricted (4-7)
  // access to storage is disabled (at host!)
  ETS_RESULT_NO_ACCESS = 5;
  // service storage is suspended (treat as TNE for SHOW and ABORT for EXPORT)
  ETS_RESULT_SUSPENDED = 6;
  // service is in read-only mode (cannot execute insert, update or create)
  ETS_RESULT_READ_ONLY = 7;
  

  // auth errors (8-15)
  ETS_RESULT_AUTH_REQUIRED  = 10; // no auth info but required
  ETS_RESULT_AUTH_CANCELLED = 11; // user cancels auth
  ETS_RESULT_AUTH_FAILED    = 12; // auth error

  // connectivity and transport errors (16-31)
  ETS_RESULT_INCOMPLETE     = 18; // should call Complete routine
  ETS_RESULT_CANNOT_CONNECT = 19; // failed to create or initialize connection object
  ETS_RESULT_NOT_CONNECTED  = 20;
  ETS_RESULT_DISCONNECTED   = 21;
  // abstract host exception in provider
  ETS_RESULT_PROVIDER_EXCEPTION = 30;
  // abstract host exception in callback
  ETS_RESULT_CALLBACK_EXCEPTION = 31;

  // invalid values (32-63)
  ETS_RESULT_INVALID_HOST_PTR     = 32;
  ETS_RESULT_INVALID_CALLBACK_PTR = 33;
  ETS_RESULT_INVALID_PROVIDER_PTR = 34;
  ETS_RESULT_INVALID_TILEENUM_PTR = 35;
  ETS_RESULT_INVALID_INPUT_BUFFER = 36;

  // if tile position is not in rect
  ETS_RESULT_INVALID_TILE_POS = 41;
  // if id is smallint and set greater than smallint
  ETS_RESULT_ID_RANGE_ERROR   = 42;
  // if size of data is invalid
  ETS_RESULT_INVALID_BUFFER_SIZE = 43;
  // storage path is invalid
  ETS_RESULT_INVALID_PATH     = 44;
  // storage structure is invalid
  ETS_RESULT_INVALID_STRUCTURE = 45;
  // if code of service is invalid
  ETS_RESULT_INVALID_SERVICE_CODE = 46;

  // length of Nst string parameter too big
  ETS_RESULT_STRING1_LEN = 51;
  ETS_RESULT_STRING2_LEN = 52;
  ETS_RESULT_STRING3_LEN = 53;
  ETS_RESULT_STRING4_LEN = 54;

  // mandatory pointer is NULL
  ETS_RESULT_POINTER1_NIL = 61;
  ETS_RESULT_POINTER2_NIL = 62;

  // unknown value (64-127)
  ETS_RESULT_UNKNOWN_SERVICE     = 65;
  ETS_RESULT_UNKNOWN_VERSION     = 66;
  ETS_RESULT_UNKNOWN_VER_COMP    = 67;
  ETS_RESULT_UNKNOWN_DIV_MODE    = 68;
  ETS_RESULT_UNKNOWN_CONTENTTYPE = 69;
  ETS_RESULT_UNKNOWN_INFOCLASS   = 70;


const
  TILE_VERSION_COMPARE_NONE   = '0'; // tile version non-comparable
  TILE_VERSION_COMPARE_ID     = 'I'; // compare using version identifier (smallint or int)
  TILE_VERSION_COMPARE_VALUE  = 'V'; // compare using version value (string)
  TILE_VERSION_COMPARE_DATE   = 'D'; // compare using version date (datetime)
  TILE_VERSION_COMPARE_NUMBER = 'N'; // compare using version number (int)

  // how to divide tiles between tables (mask width)
  TILE_DIV_NONE  = 'Z'; // all-in-one       (0) - reserved!
  TILE_DIV_1024  = 'F'; // div by mod 1024 (10) - internal only
  TILE_DIV_2048  = 'G'; // div by mod 2048 (11) - internal only
  TILE_DIV_4096  = 'H'; // div by mod 4096 (12) - internal only
  TILE_DIV_8192  = 'I'; // div by mod 8192 (13) - internal only
  TILE_DIV_16384 = 'J'; // div by mod 16384 (14) - internal only
  TILE_DIV_32768 = 'K'; // div by mod 32768 (15) - internal only
  TILE_DIV_ERROR = '0'; // unknown value

  // how to load tiles with another version or without version
  ETS_TLM_WITHOUT_VERSION = $00000001; // allow (without version) if (request with version) and (no tiles with any prev version)
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

  // flags for Initialize
  ETS_INIT_ISOLATE_ENV = $00000001; // make single isolated environment and connection

type
  // only numeric xyz
  TTILE_ID_XYZ = packed record
    xy: TPoint;
    z: Byte;
  end;
  PTILE_ID_XYZ = ^TTILE_ID_XYZ;

const
  // requests options

  // input
  ETS_ROI_ANSI_VERSION_IN      = $00000001; // PAnsiChar for VersionIn
  ETS_ROI_ANSI_VERSION_OUT     = $00000002; // PAnsiChar for VersionOut
  ETS_ROI_ANSI_CONTENTTYPE_IN  = $00000004; // PAnsiChar for ContentTypeIn
  ETS_ROI_ANSI_CONTENTTYPE_OUT = $00000008; // PAnsiChar for ContentTypeOut
  ETS_ROI_ANSI_SET_INFORMATION = $00000010; // PAnsiChar for SetInformation
  ETS_ROI_SELECT_TILE_BODY     = $00000020; // with tile body (or query info only)
  ETS_ROI_EXCLUSIVELY          = $00000040; // if set - exclusive access
  ETS_ROI_CHECK_EXISTS_ONLY    = $00000080; // just check if tile exists

  // output
  ETS_ROO_TILE_EXISTS  = $00000001; // tile found
  ETS_ROO_TNE_EXISTS   = $00000002; // TNE found
  ETS_ROO_COMMON       = $00000004; // common tile (just flag)

  // ETS_STO_HASH         = $00000008; // actual HASH (if provider stores HASH)
  // MD5             - 128 bit = 16 byte
  // SHA-1           - 160 bit = 20 byte
  // ГОСТ Р 34.11-94 - 256 bit = 32 byte
  // Tiger/192       - 192 bit = 24 byte
  // MurmurHash 2A   -  32 bit =  4 byte
  // Adler-32        -  32 bit =  4 byte
  // PJW-32          -  32 bit =  4 byte

type
  // tile info for SELECT
  TETS_SELECT_TILE_IN = packed record
    XYZ: PTILE_ID_XYZ;
    szVersionIn: Pointer;  // requested version (PAnsiChar or PWideChar)
    dwOptionsIn: LongWord; // ETS_ROI_* constants
  end;
  PETS_SELECT_TILE_IN = ^TETS_SELECT_TILE_IN;
  
  TETS_SELECT_TILE_OUT = packed record
    dwOptionsOut: LongWord;    // ETS_ROO_* constants
    szVersionOut: Pointer;     // Optional output version (PAnsiChar or PWideChar)
    szContentTypeOut: Pointer; // Mandatory output contenttype (PAnsiChar or PWideChar)
    dtLoadedUTC: TDateTime;    // Mandatory (UTC only!)
    dwTileSize: LongInt;       // Mandatory (treat <=0 as TNE)
    ptTileBuffer: Pointer;     // Optional
  end;
  PETS_SELECT_TILE_OUT = ^TETS_SELECT_TILE_OUT;

  // tile info for INSERT and UPDATE (tile or TNE)
  TETS_INSERT_TILE_IN = packed record
    XYZ: PTILE_ID_XYZ;
    szVersionIn: Pointer;   // Optional version (PAnsiChar or PWideChar)
    dwOptionsIn: LongWord;  // ETS_ROI_* constants
    szContentType: Pointer; // Optional contenttype (PAnsiChar or PWideChar)
    dtLoadedUTC: TDateTime; // Mandatory (UTC only!)
    dwTileSize: LongInt;    // Mandatory (treat <=0 as TNE)
    ptTileBuffer: Pointer;  // Optional
  end;
  PETS_INSERT_TILE_IN = ^TETS_INSERT_TILE_IN;

  // tile info for DELETE (tile or TNE)
  TETS_DELETE_TILE_IN = packed record
    XYZ: PTILE_ID_XYZ;
    szVersionIn: Pointer;  // Optional version (PAnsiChar or PWideChar)
    dwOptionsIn: LongWord; // ETS_ROI_* constants
  end;
  PETS_DELETE_TILE_IN = ^TETS_DELETE_TILE_IN;

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

  TETS_ENUM_TILE_VERSION_OUT = packed record
    // sequence
    ResponseCount: LongInt;
    ResponseIndex: LongInt;
    ResponseValue: Pointer // PETS_VERSION_W or PETS_VERSION_A
  end;
  PETS_ENUM_TILE_VERSION_OUT = ^TETS_ENUM_TILE_VERSION_OUT;



  // for GetTileRectInfo
  TETS_GET_TILE_RECT_IN = packed record
    ptTileRect: PRect;
    btTileZoom: Byte;
    szVersionIn: Pointer;  // Optional version (PAnsiChar or PWideChar)
    dwOptionsIn: LongWord; // ETS_ROI_* constants
    dwInfoMode: LongWord; // reserved (use 0)
  end;
  PETS_GET_TILE_RECT_IN = ^TETS_GET_TILE_RECT_IN;

  TETS_GET_TILE_RECT_OUT = packed record
    TilePos: TPoint;                // Mandatory tile position
    TileInfo: TETS_SELECT_TILE_OUT; // Mandatory tile info
  end;
  PETS_GET_TILE_RECT_OUT = ^TETS_GET_TILE_RECT_OUT;


  // output buffer for tile enumerator
  TETS_NEXT_TILE_ENUM_OUT = packed record
    TileFull: PTILE_ID_XYZ;         // Mandatory tile full identifier
    TileInfo: TETS_SELECT_TILE_OUT; // Mandatory tile info
  end;
  PETS_NEXT_TILE_ENUM_OUT = ^TETS_NEXT_TILE_ENUM_OUT;


  // set provider identification
  TETS_SET_IDENTIFIER_INFO = packed record
    dwOptionsIn: LongWord; // ETS_ROI_* constants
    szGlobalStorageIdentifier: Pointer;  // Optional GlobalStorageIdentifier (PAnsiChar or PWideChar)
    szServiceName: Pointer; // Mandatory ServiceName (PAnsiChar or PWideChar)
  end;
  PETS_SET_IDENTIFIER_INFO = ^TETS_SET_IDENTIFIER_INFO;



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
    // service fields
    wSize: SmallInt;
    wReserved: SmallInt;  // use 0
    // from host to storage
    tile_load_mode: LongWord; // ETS_TLM_* constants
    tile_save_mode: LongWord; // ETS_TSM_* constants
    // from storage to host
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
  wSize := sizeof(Self);
  id_div_mode := TILE_DIV_NONE;
  id_ver_comp := TILE_VERSION_COMPARE_NONE;
  work_mode := ETS_SWM_DEFAULT;
  use_common_tiles := ETS_UCT_NO;
end;

end.
