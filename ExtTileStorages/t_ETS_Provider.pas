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

unit t_ETS_Provider;

interface

uses
  t_ETS_Tiles;

type
  // provider handle
  TETS_Provider_Handle = Pointer;
  PETS_Provider_Handle = ^TETS_Provider_Handle;

  // provider functions:

  // set "path" to provider
  // name 'ETS_SetStorageIdentifier_A' (ANSI) or 'ETS_SetStorageIdentifier_W' (UNICODE)
  // returns TRUE if it allows access to storage, or FALSE otherwise
  TETS_SetStorageIdentifier_A = function(
    const AProvider_Handle: PETS_Provider_Handle;
    const AGlobalStorageIdentifier: PAnsiChar;
    const AServiceName: PAnsiChar;
    const AFlagsOut: PCardinal
  ): Boolean; stdcall;
  TETS_SetStorageIdentifier_W = function(
    const AProvider_Handle: PETS_Provider_Handle;
    const AGlobalStorageIdentifier: PWideChar;
    const AServiceName: PWideChar;
    const AFlagsOut: PCardinal
  ): Boolean; stdcall;

  // Sync storage provider
  // name 'ETS_Sync'
  TETS_Sync = function(
    const AProvider_Handle: PETS_Provider_Handle;
    const AFlags: Cardinal
  ): Boolean; stdcall;

  // Set primary content type
  // name 'ETS_SetPrimaryContentType_A' (ANSI) or 'ETS_SetPrimaryContentType_W' (UNICODE)
  TETS_SetPrimaryContentType_A = function(
    const AProvider_Handle: PETS_Provider_Handle;
    const ADefaultExt: PAnsiChar;
    const AContentType: PAnsiChar
  ): Boolean; stdcall;
  TETS_SetPrimaryContentType_W = function(
    const AProvider_Handle: PETS_Provider_Handle;
    const ADefaultExt: PWideChar;
    const AContentType: PWideChar
  ): Boolean; stdcall;

  // callbacks
  TETS_QueryAllContentTypes_Callback_A = function(
    const AHostPointer: Pointer;
    const AQueryPointer: Pointer;
    const AIndex: Integer;
    const AExtensions: PAnsiChar;
    const AContentType: PAnsiChar
    ): Boolean; stdcall;
  TETS_QueryAllContentTypes_Callback_W = function(
    const AHostPointer: Pointer;
    const AQueryPointer: Pointer;
    const AIndex: Integer;
    const AExtensions: PWideChar;
    const AContentType: PWideChar
    ): Boolean; stdcall;

  // Query all content types (supported by provider)
  // name 'ETS_QueryAllContentTypes_A' (ANSI) or 'ETS_QueryAllContentTypes_W' (UNICODE)
  TETS_QueryAllContentTypes_A = function(
    const AProvider_Handle: PETS_Provider_Handle;
    const AQueryPointer: Pointer;
    const AQueryCallback: TETS_QueryAllContentTypes_Callback_A
  ): Boolean; stdcall;
  TETS_QueryAllContentTypes_W = function(
    const AProvider_Handle: PETS_Provider_Handle;
    const AQueryPointer: Pointer;
    const AQueryCallback: TETS_QueryAllContentTypes_Callback_W
  ): Boolean; stdcall;

  // generic types for common routines
  // ANSI with 'A' and UNICODE with 'W'
  // returns TRUE if ok, or FALSE otherwise
  // check for special names bellow
  TETS_Single_Tile_Command_A = function(
    const AProvider_Handle: PETS_Provider_Handle;
    const AXYZ: PTILE_ID_XYZ;
    const AVersionString: PAnsiChar;
    const AData: Pointer
  ): Boolean; stdcall;
  TETS_Single_Tile_Command_W = function(
    const AProvider_Handle: PETS_Provider_Handle;
    const AXYZ: PTILE_ID_XYZ;
    const AVersionString: PWideChar;
    const AData: Pointer
  ): Boolean; stdcall;

  // insert TNE marker
  // name 'ETS_Insert_TNE_A' (ANSI) or 'ETS_Insert_TNE_W' (UNICODE)

  // insert TILE
  // name 'ETS_Insert_Tile_A' (ANSI) or 'ETS_Insert_Tile_W' (UNICODE)

  // delete TNE marker
  // name 'ETS_Delete_TNE_A' (ANSI) or 'ETS_Delete_TNE_W' (UNICODE)

  // delete TILE and TNE marker
  // name 'ETS_Delete_Tile_TNE_A' (ANSI) or 'ETS_Delete_Tile_TNE_W' (UNICODE)

  // query TILE
  // name 'ETS_Query_Tile_A' (ANSI) or 'ETS_Query_Tile_W' (UNICODE)

  // struct
  TETS_STATUS_BUFFER = packed record
    wSize: SmallInt;
    Status_Current: Byte;  // 0 by default
    Status_MaxOK: Byte; // 0 by default
    NoInsert: Byte; // 0 or 1
    NoDelete: Byte; // 0 or 1
    NoSelect: Byte; // 0 or 1
    //NoVerions: Byte; // 0 or 1
    //LastError: LongInt;
  end;
  PETS_STATUS_BUFFER = ^TETS_STATUS_BUFFER;

  // initialize storage provider
  // name 'ETS_Initialize'
  TETS_Initialize = function(
    const AProvider_Handle: PETS_Provider_Handle;
    const AStatusBuffer: PETS_STATUS_BUFFER;
    const AFlags: Cardinal;
    const AHostPointer: Pointer
  ): Boolean; stdcall;

  // completely initialized storage provider
  // name 'ETS_Complete'
  TETS_Complete = function(
    const AProvider_Handle: PETS_Provider_Handle;
    const AFlags: Cardinal
  ): Boolean; stdcall;

  // uninitialize storage provider
  // name 'ETS_Uninitialize'
  TETS_Uninitialize = function(
    const AProvider_Handle: PETS_Provider_Handle;
    const AFlags: Cardinal
  ): Boolean; stdcall;


  // set storage provider information
  // name 'ETS_SetInformation'
  TETS_SetInformation = function(
    const AProvider_Handle: PETS_Provider_Handle;
    const AInfoClass: Byte; // see ETS_INFOCLASS_* constants
    const AInfoSize: Cardinal;
    const AInfoData: Pointer;
    const AInfoResult: PCardinal
  ): Boolean; stdcall;


  // provider structures:
  
  TETS_INSERT_TILE_DATA = packed record
    wSize: SmallInt;
    wContentType: Word;   // IN
    dwFlagsInp: Cardinal; // IN
    dwFlagsOut: Cardinal; // OUT
    TileSize: Cardinal;   // IN
    TileBuffer: Pointer;  // IN
    TileCRC: UInt64;      // IN
    TileDate: TDateTime;  // IN
  end;
  PETS_INSERT_TILE_DATA = ^TETS_INSERT_TILE_DATA;

  TETS_QUERY_TILE_DATA = packed record
    wSize: SmallInt;
    wContentType: Word;     // OUT
    dwFlagsInp: Cardinal;   // IN
    dwFlagsOut: Cardinal;   // OUT
    TileSize: Cardinal;     // OUT
    TileCRC: UInt64;        // OUT
    TileDate: TDateTime;    // OUT
    TileHolder: Pointer;    // opaque for storage provider
    VersionHolder: Pointer; // opaque for storage provider
  end;
  PETS_QUERY_TILE_DATA = ^TETS_QUERY_TILE_DATA;

const
  // input flags
  ETS_QTI_CRC_ACTUAL      = $00010000; // crc has actual value

  // output flags
  ETS_QTO_TILE_NOT_FOUND  = $00000001; // no info about tile (TNE: _has_ info and size=0)
  ETS_QTO_SOURCE_VERSION  = $00000002; // output version is the same as input
  ETS_QTO_CRC_ACTUAL      = $00010000; // crc has actual value

  // AInfoClass values for TETS_SetInformation
  ETS_INFOCLASS_QUERY_TILE_CALLBACK_A = $00; // set callback for ETS_Query_Tile_A(pointer)
  ETS_INFOCLASS_QUERY_TILE_CALLBACK_W = $01; // set callback for ETS_Query_Tile_W (pointer)
  ETS_INFOCLASS_AUTH_NOTIFIER         = $02; // set auth notifier (pointer)
  ETS_INFOCLASS_AUTH_DATA             = $03; // set auth data (struct)
  ETS_INFOCLASS_EXCEPTION_HANDLER_A   = $04; // set exception handler (pointer)
  ETS_INFOCLASS_EXCEPTION_HANDLER_W   = $05; // set exception handler (pointer)
  ETS_INFOCLASS_LOST_NOTIFIER         = $06; // set lost connection notifier (pointer)
  ETS_INFOCLASS_REST_NOTIFIER         = $07; // set restore connection notifier (pointer)

  // flags for auth
  ETS_AUTH_UNICODE = $00000001;
  
type
  // callbacks
  TETS_Query_Tile_Callback_A = function(
    const AHostPointer: Pointer;
    const ATileBuffer: Pointer;
    const AVersionBuffer: PAnsiChar;
    const AData: PETS_QUERY_TILE_DATA
    ): Boolean; stdcall;

  TETS_Query_Tile_Callback_W = function(
    const AHostPointer: Pointer;
    const ATileBuffer: Pointer;
    const AVersionBuffer: PWideChar;
    const AData: PETS_QUERY_TILE_DATA
    ): Boolean; stdcall;

implementation

end.