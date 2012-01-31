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

  // open provider
  // {in}  AHostProvPtr - host-defined specific pointer (helper object)
  // {out} APtrProvHandle^ - Provider Handle (opaque)
  // {in}  AProvOptions - Provider options (reserved - use 0)
  TETS_Provider_Open = function(const AHostProvPtr: Pointer;
                                const APtrProvHandle: PETS_Provider_Handle;
                                const AProvOptions: LongWord): LongInt; stdcall;

  // close provider
  // {in}  AProvHandle - Provider Handle (opaque)
  TETS_Provider_Close = function(const AProvHandle: TETS_Provider_Handle): LongInt; stdcall;

  // set provider information
  TETS_Provider_Set_Info = function(const AProvHandle: TETS_Provider_Handle;
                                    const AProvSetInfoClass: LongWord;
                                    const AProvSetInfoSize: LongWord;
                                    const AProvSetInfoData: Pointer): LongInt; stdcall;

  // query provider information
  TETS_Provider_Query_Info = function(const AProvHandle: TETS_Provider_Handle;
                                      const AProvQueryInfoClass: LongWord;
                                      const AProvQueryInfoSize: LongWord;
                                      const AProvQueryInfoData: Pointer): LongInt; stdcall;

  // link handle (from storage provider to underlaying storage)
  TETS_Link_Handle = Pointer;
  PETS_Link_Handle = ^TETS_Link_Handle;

  // open new link from provider to storage
  TETS_Link_Open = function(const AHostLinkPtr: Pointer;
                            const AProvHandle: TETS_Provider_Handle;
                            const APtrLinkHandle: PETS_Link_Handle): LongInt; stdcall;

  // close link from provider to storage
  TETS_Link_Close = function(const ALinkHandle: TETS_Link_Handle): LongInt; stdcall;

  // flush buffers and close connections (but keep saved auths and keep storage opened)
  TETS_Link_Flush = function(const ALinkHandle: TETS_Link_Handle): LongInt; stdcall;

  // sync (called periodically)
  TETS_Link_Sync = function(const ALinkHandle: TETS_Link_Handle;
                            const ASyncPointer: Pointer): LongInt; stdcall;

  // manual transactions (same for begintran, rollback and commit)
  TETS_Link_SetTran = function(const ALinkHandle: TETS_Link_Handle): LongInt; stdcall;
  TETS_Link_GetTran = function(const ALinkHandle: TETS_Link_Handle; const APtrValue: PLongWord): LongInt; stdcall;

  // set link information
  TETS_Link_Set_Info = function(const ALinkHandle: TETS_Link_Handle;
                                const ALinkSetInfoClass: LongWord;
                                const ALinkSetInfoSize: LongWord;
                                const ALinkSetInfoData: Pointer): LongInt; stdcall;

  // query provider information
  TETS_Link_Query_Info = function(const ALinkHandle: TETS_Link_Handle;
                                  const ALinkQueryInfoClass: LongWord;
                                  const ALinkQueryInfoSize: LongWord;
                                  const ALinkQueryInfoData: Pointer): LongInt; stdcall;


  // struct to transmit auth info
  PETS_AUTH_INFO = Pointer; // any type of authentication information
  PPETS_AUTH_INFO = ^PETS_AUTH_INFO;

  // domain-login-password (unicode version) 
  TETS_AUTH_LOGIN_PWD_W = packed record
    szDomain: PWideChar;
    szLogin: PWideChar;
    szPassword: PWideChar;
  end;
  PETS_AUTH_LOGIN_PWD_W = ^TETS_AUTH_LOGIN_PWD_W;
  
  // domain-login-password (ansi version) 
  TETS_AUTH_LOGIN_PWD_A = packed record
    szDomain: PAnsiChar;
    szLogin: PAnsiChar;
    szPassword: PAnsiChar;
  end;
  PETS_AUTH_LOGIN_PWD_A = ^TETS_AUTH_LOGIN_PWD_A;

  // prototype for ETS_LSIC_AUTH_FUNC and ETS_LSIC_AUTH_FREE callbacks
  TETS_Link_Auth_Func = function(const ALinkHandle: TETS_Link_Handle;
                                 const AAuthPtrValue: Pointer; // host-defined callback pointer ETS_LSIC_CALLBACK_POINTER
                                 const AAuthKind: LongWord; // ETS_AK_* and ETS_AF_* constants
                                 const APtrAuthType: PLongWord; // ETS_AT_* and ETS_RF_* constants
                                 const APtrAuthSize: PLongWord; // size of AAuthData buffer in bytes
                                 const AAuthData: PPETS_AUTH_INFO): LongInt; stdcall;

  // struct for loose and restore connection handlers
  TETS_LINK_RECONNECT = packed record
    dwReconnectInOptions: LongWord;
    dwReconnectOutOptions: LongWord;
    dwWaitOptions: LongWord;
    dwWaitValue: LongWord;
  end;
  PETS_LINK_RECONNECT = ^TETS_LINK_RECONNECT;

  // prototypes for loose and restore connection callbacks
  TETS_Link_Reconn_Func = function(const ALinkHandle: TETS_Link_Handle;
                                   const AConnPtrValue: Pointer; // host-defined callback pointer
                                   const APtrConnSize: LongWord; // size of APtrConnData^ buffer
                                   const APtrConnData: PETS_LINK_RECONNECT): LongInt; stdcall;

  // tile routines (if AServiceName=NIL - use value from ETS_LSIC_SERVICENAME_*)

  // delete tile (or tne) from storage (see ETS_DELETE_* constants for AOptions)
  TETS_Tile_Del_W = function(const ALinkHandle: TETS_Link_Handle;
                             const AServiceName: PWideChar;
                             const APtrTileID: PTILE_ID_XYZ;
                             const AVersion: PWideChar;
                             const AOptions: LongWord): LongInt; stdcall;
                             
  TETS_Tile_Del_A = function(const ALinkHandle: TETS_Link_Handle;
                             const AServiceName: PAnsiChar;
                             const APtrTileID: PTILE_ID_XYZ;
                             const AVersion: PAnsiChar;
                             const AOptions: LongWord): LongInt; stdcall;

  // get tile from storage (SELECT) or get tile information from storage (QUERY) - free pointers in buffers after it
  TETS_Tile_Get_W = function(const ALinkHandle: TETS_Link_Handle;
                             const AServiceName: PWideChar;
                             const APtrTileID: PTILE_ID_XYZ;
                             const AVersion: PWideChar;
                             const APtrTileInfo: PETS_TILE_BUFFER;
                             const APtrVersionW: PETS_TILE_VERSION_W): LongInt; stdcall;

  TETS_Tile_Get_A = function(const ALinkHandle: TETS_Link_Handle;
                             const AServiceName: PAnsiChar;
                             const APtrTileID: PTILE_ID_XYZ;
                             const AVersion: PAnsiChar;
                             const APtrTileInfo: PETS_TILE_BUFFER;
                             const APtrVersionA: PETS_TILE_VERSION_A): LongInt; stdcall;

  // put (insert) tile into storage
  TETS_Tile_Put_W = function(const ALinkHandle: TETS_Link_Handle;
                             const AServiceName: PWideChar;
                             const APtrTileID: PTILE_ID_XYZ;
                             const APtrTileInfo: PETS_TILE_BUFFER;
                             const APtrVersionW: PETS_TILE_VERSION_W): LongInt; stdcall;
                                 
  TETS_Tile_Put_A = function(const ALinkHandle: TETS_Link_Handle;
                             const AServiceName: PAnsiChar;
                             const APtrTileID: PTILE_ID_XYZ;
                             const APtrTileInfo: PETS_TILE_BUFFER;
                             const APtrVersionA: PETS_TILE_VERSION_A): LongInt; stdcall;

  // put (insert) TNE marker into storage
  TETS_TNE_Put_W = function(const ALinkHandle: TETS_Link_Handle;
                            const AServiceName: PWideChar;
                            const APtrTileID: PTILE_ID_XYZ;
                            const APtrVersionW: PETS_TILE_VERSION_W): LongInt; stdcall;

  TETS_TNE_Put_A = function(const ALinkHandle: TETS_Link_Handle;
                            const AServiceName: PAnsiChar;
                            const APtrTileID: PTILE_ID_XYZ;
                            const APtrVersionA: PETS_TILE_VERSION_A): LongInt; stdcall;

  // exec DDL commands for storage
  TETS_DDL_Exec_W = function(const ALinkHandle: TETS_Link_Handle;
                             const AServiceName: PWideChar;
                             const APtrTileID: PTILE_ID_XYZ;
                             const APtrVersionW: PETS_TILE_VERSION_W): LongInt; stdcall;

  TETS_DDL_Exec_A = function(const ALinkHandle: TETS_Link_Handle;
                             const AServiceName: PAnsiChar;
                             const APtrTileID: PTILE_ID_XYZ;
                             const APtrVersionA: PETS_TILE_VERSION_A): LongInt; stdcall;

  // free pointer in tile buffer (use after SELECT)
  TETS_Tile_Free = function(const ALinkHandle: TETS_Link_Handle;
                            const APtrTileInfo: PETS_TILE_BUFFER): LongInt; stdcall;

  // free pointers in tile version buffer (use after SELECT and QUERY)
  TETS_Version_Free_W = function(const ALinkHandle: TETS_Link_Handle;
                                 const APtrVersionW: PETS_TILE_VERSION_W): LongInt; stdcall;

  TETS_Version_Free_A = function(const ALinkHandle: TETS_Link_Handle;
                                 const APtrVersionA: PETS_TILE_VERSION_A): LongInt; stdcall;
  
const
  // function name (for DLLs)
  c_ETS_Provider_Query_Info = 'ETS_Provider_Query_Info';

  // values for AProvQueryInfoClass
  ETS_PQIC_PROV_FUNC    = $00000000; // get other provider functions
  ETS_PQIC_PROV_CAPS    = $00000001; // get capabilities of storage provider
  ETS_PQIC_NAME_W       = $00000002; // get name of storage provider (library itself) (only for identification in application)
  ETS_PQIC_NAME_A       = $00000003; // same as ETS_PQIC_NAME_W (but ansi version)
  ETS_PQIC_PROTO_VER    = $00000004; // get protocol version (treat as build number, so values starting from 1) used for build provider
  ETS_PQIC_LINK_FUNC    = $00000005; // get link functions (basic)
  //ETS_PQIC_LINK_AUX_W   = $00000006; // get link functions (auxillary, unicode version)
  //ETS_PQIC_LINK_AUX_A   = $00000007; // get link functions (auxillary, ansi version)

  // values for AProvSetInfoClass
  ETS_PSIC_GLOBALID_W     = $00000001; // set global identifier of storage within host (unicode version)
  ETS_PSIC_GLOBALID_A     = $00000002; // set global identifier of storage within host (ansi version)
  ETS_PSIC_CONVERSION     = $00000003; // set conversion routine(s)
  ETS_PSIC_PROTO_VER      = $00000004; // set protocol version (treat as build number, so values starting from 1) used for build host
  ETS_PSIC_EXCEPT_HANDLER = $00000005; // set exception handler (to transmit exceptions from provider to host)
  ETS_PSIC_PROGRESS_FUNC  = $00000006; // set callback function from host (allowed for some operations)

  // values for ALinkSetInfoClass
  ETS_LSIC_SERVICENAME_W    = $00000001; // set service name for identification in storage (unicode version) - use PChar as buffer pointer
  ETS_LSIC_SERVICENAME_A    = $00000002; // set service name for identification in storage (ansi version) - use PChar as buffer pointer
  ETS_LSIC_CONNECTIONINFO_W = $00000003; // set connection info (unicode version) - use PChar as buffer pointer
  ETS_LSIC_CONNECTIONINFO_A = $00000004; // set connection info (ansi version) - use PChar as buffer pointer
  ETS_LSIC_CALLBACK_POINTER = $00000005; // set pointer to use with callback function - use the pointer as buffer pointer
  ETS_LSIC_AUTH_FUNC        = $00000006; // set authentication callback function (to get auth info)
  ETS_LSIC_AUTH_FREE        = $00000007; // set authentication callback function (to free buffers)
  ETS_LSIC_LOST_CONNECT     = $00000008; // set callback to call when loose connection
  ETS_LSIC_RESTORE_CONNECT  = $00000009; // set callback to call when restore connection (only after loose)
  ETS_LSIC_CONTENTTYPE_W    = $0000000A; // set content type for service in storage (unicode version) - use PChar as buffer pointer
  ETS_LSIC_CONTENTTYPE_A    = $0000000B; // set content type for service in storage (ansi version) - use PChar as buffer pointer
  ETS_LSIC_SRC_STORAGE_OPT  = $0000000C; // set storage options for service in storage - use TETS_SOURCE_STORAGE_OPTIONS as buffer

  // values for ALinkQueryInfoClass
  ETS_LQIC_TILE_ROUTINES_W = $00000001; // get routines to work with tiles (unicode version)
  ETS_LQIC_TILE_ROUTINES_A = $00000002; // get routines to work with tiles (ansi version)
  ETS_LQIC_CONTENT_TYPE_W  = $00000003; // get content_type and other params from underlaying storage (unicode version)
  ETS_LQIC_CONTENT_TYPE_A  = $00000004; // get content_type and other params from underlaying storage (ansi version)
  ETS_LQIC_TILE_ID_FORMAT  = $00000005; // get tile_id format, that really used in underlaying storage (PLongWord)

  ETS_CONTENT_TYPE_MIXED = $00000001; // storage allows tiles with 2 or more different ContentTypes (see ETS_LQIC_CONTENT_TYPE_*)

  // storage provider and storage supports:
  ETS_SUPPORT_TNE             = $00000001; // can use TNE markers
  ETS_SUPPORT_WITH_VERSION    = $00000002; // can use tiles with version
  ETS_SUPPORT_WITHOUT_VERSION = $00000004; // can use tiles without version
  ETS_SUPPORT_TRANSACTIONS    = $00000008; // can use manual (explicit) transctions
  ETS_SUPPORT_GET             = $00000010; // can GET tiles (and TNE if defined)
  ETS_SUPPORT_PUT             = $00000020; // can PUT tiles (and TNE if defined)
  ETS_SUPPORT_DELETE          = $00000040; // can DELETE tiles (and TNE if defined)
  ETS_SUPPORT_UPDATE          = $00000080; // can UPDATE tiles (and TNE if defined) - can overwrite existing tiles
  ETS_SUPPORT_CRC32           = $00000100; // can put crc32 to storage and get it from storage for every tile (without checking)
  // mask
  ETS_SUPPORT_ALL_ACCESS = (ETS_SUPPORT_GET or ETS_SUPPORT_PUT or ETS_SUPPORT_DELETE or ETS_SUPPORT_UPDATE);
  ETS_SUPPORT_COMMON     = (ETS_SUPPORT_ALL_ACCESS or ETS_SUPPORT_WITHOUT_VERSION or ETS_SUPPORT_TNE);
  ETS_SUPPORT_FULL       = (ETS_SUPPORT_COMMON or ETS_SUPPORT_WITH_VERSION or ETS_SUPPORT_TRANSACTIONS);

type
  // ETS_PQIC_PROV_FUNC
  TETS_PQI_PROV_FUNC = packed record
    p_Provider_Open: TETS_Provider_Open;
    p_Provider_Close: TETS_Provider_Close;
    p_Provider_Set_Info: TETS_Provider_Set_Info;
    p_Link_Open: TETS_Link_Open; // connect on demand
    p_Link_Close: TETS_Link_Close; // clear saved auths and close all opened connections
  end;
  PETS_PQI_PROV_FUNC = ^TETS_PQI_PROV_FUNC;

  // ETS_PQIC_PROV_CAPS
  TETS_PQI_PROV_CAPS = packed record
    dwSupport: LongWord; // basic (see ETS_SUPPORT_* constants)
    dwExtend: LongWord; // restrictions and other flags - reserved (use 0)
    dwAuths: LongWord; // supported authentications (what kinds of auth can ask) - see ETS_AK_* and ETS_AF_* constants
    dwTileIdFormats: LongWord; // Allowed (supported) TileID Formats
  end;
  PETS_PQI_PROV_CAPS = ^TETS_PQI_PROV_CAPS;

  // ETS_PQIC_NAME_W
  TETS_PQI_NAME_W = packed record
    szName: PWideChar; // never free this pointer!
  end;
  PETS_PQI_NAME_W = ^TETS_PQI_NAME_W;

  // ETS_PQIC_NAME_A
  TETS_PQI_NAME_A = packed record
    szName: PAnsiChar; // never free this pointer!
  end;
  PETS_PQI_NAME_A = ^TETS_PQI_NAME_A;

  // ETS_PQIC_LINK_FUNC
  TETS_PQI_LINK_FUNC = packed record
    // runtime routines
    p_Link_Flush: TETS_Link_Flush;
    p_Link_Sync: TETS_Link_Sync;
    // transaction routines
    p_Link_BeginTran: TETS_Link_SetTran;
    p_Link_Commit: TETS_Link_SetTran;
    p_Link_Rollback: TETS_Link_SetTran;
    p_Link_TranCount: TETS_Link_GetTran;
    p_Link_TranOptions: TETS_Link_GetTran;
    // set and query info
    p_Link_Set_Info: TETS_Link_Set_Info;
    p_Link_Query_Info: TETS_Link_Query_Info;
  end;
  PETS_PQI_LINK_FUNC = ^TETS_PQI_LINK_FUNC;

  // ETS_LQIC_TILE_ROUTINES_W
  TETS_LQI_TILE_ROUTINES_W = packed record
    p_Tile_Select_W: TETS_Tile_Get_W;
    p_Tile_Query_W: TETS_Tile_Get_W;
    p_Tile_Delete_W: TETS_Tile_Del_W;
    p_Tile_Insert_W: TETS_Tile_Put_W;
    p_Tne_Create_W: TETS_TNE_Put_W;
    p_Ddl_Exec_W: TETS_DDL_Exec_W;
    p_Ver_Free_W: TETS_Version_Free_W;
    p_Tile_Free: TETS_Tile_Free;
  end;
  PETS_LQI_TILE_ROUTINES_W = ^TETS_LQI_TILE_ROUTINES_W;

  // ETS_LQIC_TILE_ROUTINES_A
  TETS_LQI_TILE_ROUTINES_A = packed record
    p_Tile_Select_A: TETS_Tile_Get_A;
    p_Tile_Query_A: TETS_Tile_Get_A;
    p_Tile_Delete_A: TETS_Tile_Del_A;
    p_Tile_Insert_A: TETS_Tile_Put_A;
    p_Tne_Create_A: TETS_TNE_Put_A;
    p_Ddl_Exec_A: TETS_DDL_Exec_A;
    p_Ver_Free_A: TETS_Version_Free_A;
    p_Tile_Free: TETS_Tile_Free;
  end;
  PETS_LQI_TILE_ROUTINES_A = ^TETS_LQI_TILE_ROUTINES_A;

  // ETS_LQIC_CONTENT_TYPE_W
  TETS_LQI_CONTENT_TYPE_W = packed record
    szContentType: PWideChar; // Never free this pointer!
    szDefaultExt: PWideChar; // Never free this pointer!
    dwOptions: LongWord; // ETS_CONTENT_TYPE_* constants  
  end;
  PETS_LQI_CONTENT_TYPE_W = ^TETS_LQI_CONTENT_TYPE_W;

  // ETS_LQIC_CONTENT_TYPE_A
  TETS_LQI_CONTENT_TYPE_A = packed record
    szContentType: PAnsiChar; // Never free this pointer!
    szDefaultExt: PAnsiChar; // Never free this pointer!
    dwOptions: LongWord; // ETS_CONTENT_TYPE_* constants
  end;
  PETS_LQI_CONTENT_TYPE_A = ^TETS_LQI_CONTENT_TYPE_A;
  
implementation

end.