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

unit u_GECrypt;

interface

uses
  Types,
  Classes,
  i_MapVersionInfo;

const
  // QueryTile - Input
  DLLCACHE_QTI_LOAD_TILE  = $00000001;

  // QueryTile - Output
  DLLCACHE_QTO_SAME_VERSION = $00000001;
  DLLCACHE_QTO_TNE_EXISTS   = $00000002;
  DLLCACHE_QTO_ERR_FORMAT   = $00000004; // invalid result tile format (but tile exists)
  
  // set callback for storage state notifications
  DLLCACHE_SIC_STATE_CHANGED = $00000001;
  // set exif reader proc by SetInformation
  DLLCACHE_SIC_EXIF_READER   = $00000002;

type
  THostExifReaderProc = function (const AContext: Pointer;
                                  const ABuffer: Pointer;
                                  const ASize: Cardinal;
                                  const AExifBufPtr: PPointer;
                                  const AExifSizPtr: PCardinal): Boolean; stdcall;
  
  THostStateChangedProc = function (const AContext: Pointer;
                                    const AEnabled: Boolean): Boolean; stdcall;
  
  TDLLCacheHandle = Pointer;
  PDLLCacheHandle = ^TDLLCacheHandle;

  // DLLCache_Init - initialize
  TDLLCache_Init = function(const ADLLCacheHandle: PDLLCacheHandle;
                            const AFlagsInp: Cardinal;
                            const AContext: Pointer): Boolean; stdcall;

  // DLLCache_Uninit - uninitialize
  TDLLCache_Uninit = function(const ADLLCacheHandle: PDLLCacheHandle): Boolean; stdcall;
  
  // DLLCache_SetPath - set path
  TDLLCache_SetPath = function (const ADLLCacheHandle: PDLLCacheHandle; const APath: PAnsiChar): Boolean; stdcall;

  TEnumTileVersionsInfo = packed record
    // common params
    Size: SmallInt;
    Cancelled: Byte;
    Zoom: Byte;
    XY: TPoint;
    VersionInp: PAnsiChar; // original version
    FlagsInp: Cardinal;
    FlagsOut: Cardinal;
    // enum params
    Counter: Cardinal; // incremented by DLL (based on result of callback)
    ListOfVersions: TStringList; // opaque for DLL
  end;
  PEnumTileVersionsInfo = ^TEnumTileVersionsInfo;

  TDLLCache_EnumTileVersions_Callback = function (const AContext: Pointer;
                                                  const AEnumInfo: PEnumTileVersionsInfo;
                                                  const AVersionString: PAnsiChar): Boolean; stdcall;

  // DLLCache_EnumTileVersions - get list of tile versions
  TDLLCache_EnumTileVersions = function(const ADLLCacheHandle: PDLLCacheHandle;
                                        const AEnumInfo: PEnumTileVersionsInfo;
                                        const AEnumCallback: TDLLCache_EnumTileVersions_Callback): Boolean; stdcall;

  TQueryTileInfo = packed record
    // common params
    Size: SmallInt;
    Cancelled: Byte;
    Zoom: Byte;
    XY: TPoint;
    VersionInp: PAnsiChar; // original version
    FlagsInp: Cardinal; // DLLCACHE_QTI_* constants
    FlagsOut: Cardinal; // DLLCACHE_QTO_* constants
    // result params
    TileSize: Cardinal;
    TileStream: TStream; // opaque for DLL
    VersionOut: IMapVersionInfo; // opaque for DLL - returned version (or NIL)
    DateOut: TDateTime; // returned tile date
  end;
  PQueryTileInfo = ^TQueryTileInfo;

  TDLLCache_QueryTile_Callback = function(const AContext: Pointer;
                                          const ATileInfo: PQueryTileInfo;
                                          const ATileBuffer: Pointer;
                                          const AVersionString: PAnsiChar): Boolean; stdcall;

  // DLLCache_QueryTile - get tile information (with or without loading tile)
  TDLLCache_QueryTile = function(const ADLLCacheHandle: PDLLCacheHandle;
                                 const ATileInfo: PQueryTileInfo;
                                 const AQueryTile_Callback: TDLLCache_QueryTile_Callback): Boolean; stdcall;


  // DLLCache_SetInformation - set information (params, functions,...)
  TDLLCache_SetInformation = function(const ADLLCacheHandle: PDLLCacheHandle;
                                      const ASetInfoClass: Byte;
                                      const ASetInfoSize: Cardinal;
                                      const ASetInfoData: Pointer): Boolean; stdcall;

implementation

end.
