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

unit t_DLLCache;

interface

uses
  Windows;

const
  // Init - FlagsInp
  DLLCACHE_IFI_IMG_FORCE_PRI_FORMAT = $00000001; // convert any images to primary format
  DLLCACHE_IFI_IMG_FORCE_NO_INDEX   = $00000002; // do not use index for showing images
  DLLCACHE_IFI_VER_FORCE_NO_INDEX   = $00000004; // do not use index for enumerating versions
  //DLLCACHE_IFI_IMG_ACQUISITION_DATE = $00000008; // return date of image acquisition (instead of tile loading date)

  // QueryTile - Input
  DLLCACHE_QTI_LOAD_TILE    = $00000001;

  // QueryTile - Output
  DLLCACHE_QTO_SAME_VERSION = $00000001;
  DLLCACHE_QTO_TNE_EXISTS   = $00000002;
  DLLCACHE_QTO_ERR_FORMAT   = $00000004; // invalid result tile format (but tile exists, time to say wtf?)
  DLLCACHE_QTO_SEC_FORMAT   = $00000008; // secondary format (can convert to primary)

  // set callback for storage state notifications
  DLLCACHE_SIC_STATE_CHANGED = $00000001;
  // set exif reader proc by SetInformation
  DLLCACHE_SIC_EXIF_READER   = $00000002;

  // output formats
  DLLCACHE_IMG_PRIMARY  = 0; // JPEG
  DLLCACHE_IMG_SEC_DXT1 = 1;

(*
  all params in version corresponds to bits.
  so unique combination of params' existance gives the unique numbers (and independent flags).
  if date = 0th bit
     ver = 1st bit
     server = 2nd bit
  then:
     date\ver[server] = Flag7 (111)
     date[server]     = Flag5 (101)
     ver[server]      = Flag3 (011)
     [server]         = Flag1 (001)

  THEN parse FlagN value

  if original version is:
     date\ver[server]
  then additional versions (always with [server]) are:
  0. [server]
  1. date[server]
  2. ver[server]
  and switch by Flag7 values:
  0 (000) - return as defined in DLL (or in storage) by ISV
  1 (001) - return only [server]
  2 (010) - return only date[server]
  3 (011) - return [server] and date[server]
  4 (100) - return only ver[server]
  5 (101) - return [server] and ver[server]
  6 (110) - return date[server] and ver[server]
  7 (111) - return [server], date[server] and ver[server]

  if original version is:
     ver[server]
  then additional versions (always with [server]) are:
  0. [server]
  and switch by Flag3 values:
*)

type
  TDLLCacheHandle = Pointer;
  PDLLCacheHandle = ^TDLLCacheHandle;

  PRect = Windows.PRect;

  // DLLCache_Init - initialize
  TDLLCache_Init = function(
    const ADLLCacheHandle: PDLLCacheHandle;
    const AFlagsInp: LongWord;
    const AContext: Pointer
  ): Boolean; stdcall;

  // DLLCache_Uninit - uninitialize
  TDLLCache_Uninit = function(
    const ADLLCacheHandle: PDLLCacheHandle
  ): Boolean; stdcall;

  // DLLCache_SetPath - set path
  TDLLCache_SetPath = function (
    const ADLLCacheHandle: PDLLCacheHandle;
    const APath: PAnsiChar
  ): Boolean; stdcall;

  // DLLCache_SetInformation - set information (params, functions,...)
  TDLLCache_SetInformation = function(
    const ADLLCacheHandle: PDLLCacheHandle;
    const ASetInfoClass: Byte;
    const ASetInfoSize: LongWord;
    const ASetInfoData: Pointer
  ): Boolean; stdcall;

  // common params (same for TQueryTileInfo + TEnumTileVersionsInfo + TUrlOfTileInfo)
  TCommonTileOperInfo = packed record
    Size: SmallInt; // FULL size!
    Cancelled: Byte;
    Zoom: Byte;
    XY: TPoint;
    VersionInp: PAnsiChar; // original version
    FlagsInp: LongWord; // DLLCACHE_QTI_* constants (and others)
    FlagsOut: LongWord; // DLLCACHE_QTO_* constants (and others)
  end;
  PCommonTileOperInfo = ^TCommonTileOperInfo;

  TEnumTileVersionsInfo = packed record
    // common params
    Common: TCommonTileOperInfo;
    // enum params
    Counter: LongWord; // incremented by DLL (based on result of callback)
    ListOfVersions: Pointer; // opaque for DLL, use TStringList in host
  end;
  PEnumTileVersionsInfo = ^TEnumTileVersionsInfo;

  TQueryTileInfo = packed record
    // common params
    Common: TCommonTileOperInfo;
    // result params
    TileSize: LongWord;
    TileStream: Pointer; // opaque for DLL - use TMemoryStream in host
    VersionOut: Pointer; // opaque for DLL - returned version - use IMapVersionInfo or NIL
    DateOut: TDateTime;  // returned tile date
  end;
  PQueryTileInfo = ^TQueryTileInfo;

  // use it only if need for original data
  TQueryTileInfo_V2 = packed record
    V1: TQueryTileInfo;
    FormatOut: LongWord; // format identifier (0 for primary format)
  end;
  PQueryTileInfo_V2 = ^TQueryTileInfo_V2;

  // callback to write image to host (stream or buffer)
  TDLLCache_ConvertImage_Callback = function(
    const AConvertImage_Context: Pointer;
    const AFormatOut: LongWord;
    const AOutputBuffer: Pointer;
    const AOutputSize: LongWord
  ): Boolean; stdcall;

  // DLLCache_ConvertImage - convert image (from secondary format to primary format)
  TDLLCache_ConvertImage = function(
    const AConvertImage_Context: Pointer;
    const ABuffer: Pointer;
    const ASize: LongWord;
    const AFormatInp: LongWord;
    const AFormatOut: LongWord;
    const AConvertImage_Callback: TDLLCache_ConvertImage_Callback
  ): Boolean; stdcall;

  // DLLCache_ConvertImageEx - convert image (from secondary format to primary format) with COM and APPN
  TDLLCache_ConvertImageEx = function(
    const AConvertImage_Context: Pointer;
    const ABuffer: Pointer;
    const ASize: LongWord;
    const AFormatInp: LongWord;
    const AFormatOut: LongWord;
    const AComment: PAnsiChar;
    const AReserved: Pointer;
    const AConvertImage_Callback: TDLLCache_ConvertImage_Callback
  ): Boolean; stdcall;

  // Host routine to read exif from jpeg
  THostExifReaderProc = function(
    const AContext: Pointer;
    const ABuffer: Pointer;
    const ASize: LongWord;
    const AExifBufPtr: PPointer;
    const AExifSizPtr: PLongWord
  ): Boolean; stdcall;

  // Host routine to notify about storage state changes
  THostStateChangedProc = function(
    const AContext: Pointer;
    const AEnabled: Boolean
  ): Boolean; stdcall;

  // callback from enum tile versions
  TDLLCache_EnumTileVersions_Callback = function(
    const AContext: Pointer;
    const AEnumInfo: PEnumTileVersionsInfo;
    const AVersionString: PAnsiChar
  ): Boolean; stdcall;

  // DLLCache_EnumTileVersions - get list of tile versions
  TDLLCache_EnumTileVersions = function(
    const ADLLCacheHandle: PDLLCacheHandle;
    const AEnumInfo: PEnumTileVersionsInfo;
    const AEnumCallback: TDLLCache_EnumTileVersions_Callback
  ): Boolean; stdcall;

  // callback to write tile to stream
  TDLLCache_QueryTile_Callback = function(
    const AContext: Pointer;
    const ATileInfo: PQueryTileInfo;
    const ATileBuffer: Pointer;
    const AVersionString: PAnsiChar
  ): Boolean; stdcall;

  TDLLCache_QueryTerrainTile_Callback = function(
    const AContext: Pointer;
    const ATileInfo: PQueryTileInfo;
    const ATileBuffer: Pointer
  ): Boolean; stdcall;

  // DLLCache_QueryTile - get tile information (with or without loading tile)
  TDLLCache_QueryTile = function(
    const ADLLCacheHandle: PDLLCacheHandle;
    const ATileInfo: PQueryTileInfo;
    const AQueryTile_Callback: TDLLCache_QueryTile_Callback
  ): Boolean; stdcall;

  TDLLCache_QueryTerrainTile = function(
    const ADLLCacheHandle: PDLLCacheHandle;
    const ATileInfo: PQueryTileInfo;
    const AQueryTerrainTile_Callback: TDLLCache_QueryTerrainTile_Callback
  ): Boolean; stdcall;

  // get and set memory manager for DLL
  TDLLCache_SetMemoryManager = function(
    const ASet: Boolean;
    const AMgrPtr: PMemoryManagerEx
  ): Boolean; stdcall;

  (*
  THostInstanceParams = packed record
    SizeOfMe: SmallInt;
    Multithreaded: Byte;
    Reserved: Byte;
    MainInstance: LongWord;
    MainThreadID: LongWord;
  end;
  PHostInstanceParams = ^THostInstanceParams;

  // set global host params (allow call twice, but should set same values)
  TDLLCache_SetHostInstanceParams = function(
    const AHostInstanceParams: PHostInstanceParams
  ): Boolean; stdcall;
  *)

const
  // UrlOfTile - Input
  DLLCACHE_UTI_GET_VERSION = $00000001; // return version of file (not implemented yet)

  // UrlOfTile - Output
  DLLCACHE_UTO_QTREE       = $00000001; // url for Qtree file
  DLLCACHE_UTO_IMAGE       = $00000002; // url for Image file
  DLLCACHE_UTO_HIDDEN      = $00000004; // special flag for another default server (to be renamed!)

  DLLCACHE_UTPI_HOST       = $00000001; // set special proxy host (address)

type
  TUrlOfTileInfo = packed record
    // common params
    Common: TCommonTileOperInfo;
    // result params
    Counter: LongWord;    // opaque for DLL
    ResultURLs: Pointer;  // opaque for DLL - use TStringList
    ProxyParams: Pointer; // opaque for DLL
  end;
  PUrlOfTileInfo = ^TUrlOfTileInfo;

  TUrlOfTileProxyInfo = packed record
    Size: SmallInt;
    Flags: Word; // see DLLCACHE_UTPI_* constants
    ProxyAddr: PAnsiChar;
    Reserved1: Pointer;
    Reserved2: Pointer;
  end;
  PUrlOfTileProxyInfo = ^TUrlOfTileProxyInfo;

  TDLLCache_UrlOfTile_Callback = function(
    const AContext: Pointer;
    const AUrlOfTileType: LongWord; {reserved - use 0}
    const AUrlOfTileInfo: PUrlOfTileInfo;
    const AUrlOfTileText: PAnsiChar;
    const AVersionString: PAnsiChar;
    const AUrlOfTileProxy: PUrlOfTileProxyInfo
  ): Boolean; stdcall;

  // DLLCache_UrlOfTile - get full url to download tile
  TDLLCache_UrlOfTile = function(
    const ADLLCacheHandle: PDLLCacheHandle;
    const AUrlOfTileType: LongWord; {reserved - use 0}
    const AUrlOfTileInfo: PUrlOfTileInfo;
    const AUrlOfTile_Callback: TDLLCache_UrlOfTile_Callback
  ): Boolean; stdcall;

  //
  // for DLLCache_QueryTileRectInfo
  TQueryTileRectInfo = packed record
    // common params
    Common: TCommonTileOperInfo; // fill all except XY
    TileRect: TRect; // pointer to original rectangle
    // output
    TileSize: LongWord; // tile size
    DateOut: TDateTime; // tile date
  end;
  PQueryTileRectInfo = ^TQueryTileRectInfo;

  // callback to write tile info to array
  TDLLCache_QueryTileRectInfo_Callback = function(
    const AContext: Pointer;
    const ATileRectInfo: PQueryTileRectInfo
  ): Boolean; stdcall;

  // DLLCache_QueryTileRectInfo - get tile information for rectangle (for filling map)
  TDLLCache_QueryTileRectInfo = function(
    const ADLLCacheHandle: PDLLCacheHandle;
    const ATileRectInfo: PQueryTileRectInfo;
    const AQueryTileRectInfo_Callback: TDLLCache_QueryTileRectInfo_Callback
  ): Boolean; stdcall;


implementation

end.
