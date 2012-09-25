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

unit t_ETS_Provider;

interface

uses
  t_ETS_Tiles;

type
  // provider handle (single handle for single tile storage)
  TETS_Provider_Handle = Pointer;
  PETS_Provider_Handle = ^TETS_Provider_Handle;

  // tiles enumerator handle
  TETS_EnumTiles_Handle = Pointer;
  PETS_EnumTiles_Handle = ^TETS_EnumTiles_Handle;

  // provider functions:

  // set "path" to provider
  // name 'ETS_SetStorageIdentifier'
  TETS_SetStorageIdentifier = function(
    const AProvider_Handle: PETS_Provider_Handle;
    const AFlagsOut: PLongWord
  ): Byte; stdcall;

  // Sync storage provider
  // name 'ETS_Sync'
  TETS_Sync = function(
    const AProvider_Handle: PETS_Provider_Handle;
    const AFlags: LongWord // ETS_ROI_EXCLUSIVELY
  ): Byte; stdcall;


  // tile routines

  // callback (set by special routine)
  TETS_SelectTile_Callback = function(
    const AHostPointer: Pointer;
    const ACallbackPointer: Pointer;
    const ASelectBufferInp: PETS_SELECT_TILE_IN;
    const ASelectBufferOut: PETS_SELECT_TILE_OUT
  ): Byte; stdcall;

  // get tile or get tile info (supported by provider)
  // name 'ETS_SelectTile'
  TETS_SelectTile = function(
    const AProvider_Handle: PETS_Provider_Handle;
    const ACallbackPointer: Pointer;
    const ASelectBufferIn: PETS_SELECT_TILE_IN
  ): Byte; stdcall;

  // insert tile or update tile of set TNE marker (supported by provider)
  // name 'ETS_InsertTile' (MANDATORY)
  // name 'ETS_InsertTNE'  (OPTIONAL)
  TETS_InsertTile = function(
    const AProvider_Handle: PETS_Provider_Handle;
    const AInsertBuffer: PETS_INSERT_TILE_IN
  ): Byte; stdcall;
  
  // delete tile or TNE marker (supported by provider)
  // name 'ETS_DeleteTile' (MANDATORY)
  // name 'ETS_DeleteTNE' (OPTIONAL)
  TETS_DeleteTile = function(
    const AProvider_Handle: PETS_Provider_Handle;
    const ADeleteBuffer: PETS_DELETE_TILE_IN
  ): Byte; stdcall;


  // callback (set by special routine)
  TETS_EnumTileVersions_Callback = function(
    const AHostPointer: Pointer;
    const ACallbackPointer: Pointer;
    const AEnumTileVerBufferInp: PETS_SELECT_TILE_IN;
    const AEnumTileVerBufferOut: PETS_ENUM_TILE_VERSION_OUT
  ): Byte; stdcall;

  // enum tile versions (supported by provider)
  // name 'ETS_EnumTileVersions'
  TETS_EnumTileVersions = function(
    const AProvider_Handle: PETS_Provider_Handle;
    const ACallbackPointer: Pointer;
    const ASelectBufferIn: PETS_SELECT_TILE_IN
  ): Byte; stdcall;



  // callback (set by special routine)
  TETS_GetTileRectInfo_Callback = function(
    const AHostPointer: Pointer;
    const ACallbackPointer: Pointer;
    const ATileRectInfoInp: PETS_GET_TILE_RECT_IN;
    const ATileRectInfoOut: PETS_GET_TILE_RECT_OUT
  ): Byte; stdcall;

  // get tile in rect info (supported by provider)
  // name 'ETS_GetTileRectInfo'
  TETS_GetTileRectInfo = function(
    const AProvider_Handle: PETS_Provider_Handle;
    const ACallbackPointer: Pointer;
    const ATileRectInfoIn: PETS_GET_TILE_RECT_IN
  ): Byte; stdcall;


  // struct
  TETS_STATIC_BUFFER = packed record
    wSize: SmallInt;
    wReserved: SmallInt;
    //dwNoVersionId: LongInt; // special id_version for requests without version (Int or SmallInt)
    //dwPriVersionId: LongInt; // primary id_version for service (Int or SmallInt)
    //dwPriContentType: LongInt; // primary contenttype for service (Int or SmallInt)
    //Status_Current: Byte;  // 0 by default
    //Status_MaxOK: Byte; // 0 by default
    //NoInsert: Byte; // 0 or 1
    //NoDelete: Byte; // 0 or 1
    //NoSelect: Byte; // 0 or 1
    //NoVerions: Byte; // 0 or 1
    //LastError: LongInt;
  public
    procedure Clear;
  end;
  PETS_STATIC_BUFFER = ^TETS_STATIC_BUFFER;

  // initialize storage provider
  // name 'ETS_Initialize'
  TETS_Initialize = function(
    const AProvider_Handle: PETS_Provider_Handle;
    const AStatusBuffer: PETS_STATIC_BUFFER; // MANDATORY
    const AFlags: LongWord;  // see ETS_INIT_* constants
    const AHostPointer: Pointer // MANDATORY
  ): Byte; stdcall;

  // completely initialized storage provider
  // name 'ETS_Complete'
  TETS_Complete = function(
    const AProvider_Handle: PETS_Provider_Handle;
    const AFlags: LongWord
  ): Byte; stdcall;

  // uninitialize storage provider
  // name 'ETS_Uninitialize'
  TETS_Uninitialize = function(
    const AProvider_Handle: PETS_Provider_Handle;
    const AFlags: LongWord
  ): Byte; stdcall;

  // set storage provider information
  // name 'ETS_SetInformation'
  TETS_SetInformation = function(
    const AProvider_Handle: PETS_Provider_Handle;
    const AInfoClass: Byte; // see ETS_INFOCLASS_* constants
    const AInfoSize: LongWord;
    const AInfoData: Pointer;
    const AInfoResult: PLongWord
  ): Byte; stdcall;

  // initialize tile enumerator
  // name 'ETS_MakeTileEnum'
  TETS_MakeTileEnum = function(
    const AProvider_Handle: PETS_Provider_Handle;  // IN
    const AEnumTilesHandle: PETS_EnumTiles_Handle; // OUT
    const AFlags: LongWord;  // reserved
    const AHostPointer: Pointer // MANDATORY
  ): Byte; stdcall;

  // uninitialize tile enumerator
  // name 'ETS_KillTileEnum'
  TETS_KillTileEnum = function(
    const AEnumTilesHandle: PETS_EnumTiles_Handle; // IN OUT
    const AFlags: LongWord
  ): Byte; stdcall;

  // uninitialize tile enumerator
  // name 'ETS_NextTileEnum'
  TETS_NextTileEnum = function(
    const AEnumTilesHandle: PETS_EnumTiles_Handle; // IN
    const ACallbackPointer: Pointer; // MANDATORY
    const ANextBufferIn: PETS_GET_TILE_RECT_IN
  ): Byte; stdcall;

  // callback for ETS_NextTileEnum
  TETS_NextTileEnum_Callback = function(
    const AHostPointer: Pointer;
    const ACallbackPointer: Pointer;
    const ANextBufferInp: PETS_GET_TILE_RECT_IN;
    const ANextBufferOut: PETS_NEXT_TILE_ENUM_OUT
  ): Byte; stdcall;

const
  // AInfoClass values for TETS_SetInformation
  ETS_INFOCLASS_SetStorageIdentifier      = $01; // set GlobalStorageIdentifier and ServiceName

  ETS_INFOCLASS_SelectTile_Callback       = $10; // set callback for ETS_SelectTile (pointer)
  ETS_INFOCLASS_EnumTileVersions_Callback = $11; // set callback for ETS_EnumTileVersions (pointer)
  ETS_INFOCLASS_GetTileRectInfo_Callback  = $12; // set callback for ETS_GetTileRectInfo (pointer)
  ETS_INFOCLASS_NextTileEnum_Callback     = $13; // set callback for ETS_NextTileEnum (pointer)

  ETS_INFOCLASS_Disconnect_Notifier       = $20; // set lost connection notifier (pointer)
  ETS_INFOCLASS_Reconnect_Notifier        = $21; // set restore connection notifier (pointer)
  ETS_INFOCLASS_Messages_Notifier         = $22; // set messages, warnings, errors ... notifier (pointer)


implementation

{ TETS_STATIC_BUFFER }

procedure TETS_STATIC_BUFFER.Clear;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.wSize := SizeOf(Self);
end;

end.