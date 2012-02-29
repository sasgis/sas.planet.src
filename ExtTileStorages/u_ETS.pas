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

unit u_ETS;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils,
  Classes,
  t_ETS_AuthKind,
  t_ETS_AuthFunc,
  t_ETS_Provider,
  t_ETS_Tiles,
  t_ETS_List,
  t_ETS_Result,
  u_ETS_Tiles,
  i_MapVersionInfo,
  i_TileInfoBasic,
  i_ContentTypeInfo;

type
  TETS_Host_Provider_Basic = class;

  TETS_ConnLostStatus = (ets_cls_None, ets_cls_OnSyncOnly, ets_cls_OnSyncAndOnDemand, ets_cls_ByProvider);

  TETS_Host_Link = class(TObject)
  private
    function GetContentTypeMixed: Boolean;
  protected
    FHostProvider: TETS_Host_Provider_Basic;
    FLinkHandle: TETS_Link_Handle;
    FSynchronizer: TMultiReadExclusiveWriteSynchronizer;
    FETS_LQI_TILE_ROUTINES_W: TETS_LQI_TILE_ROUTINES_W;
    FETS_LQI_TILE_ROUTINES_A: TETS_LQI_TILE_ROUTINES_A;
    FServiceName: String;
    FConnectionInfo: String;
    FContentType: WideString;
    FDefaultExt: WideString;
    FUnderlayingTileIdFormat: LongWord;
    FContentTypeOptions: LongWord;
    FContentTypeBasic: IContentTypeInfoBasic;
    FQueryTileOptions: LongWord;
    // flags (partially)
    FInitialized: Boolean;
    FConnected: Boolean;
    FUnderlaying: Boolean;
    FConnLostStatus: TETS_ConnLostStatus;
    function Internal_Uninitialize_ETS: LongInt;
    procedure Internal_Initialize_Zero;
    function Internal_Initialize_ETS: LongInt;
    function Internal_Connect_ETS(const AServiceName, AConnectionInfo: String): LongInt;
    function Internal_Apply_Source_Params(const ABaseContentType: String;
                                          const AServiceOptions: PETS_SOURCE_STORAGE_OPTIONS): LongInt;
    function Internal_Underlaying: LongInt;
  protected
    // helpers
    function Internal_Create_TileInfo(const AAnsiVersion: Boolean;
                                      const AVersionInfo: IMapVersionInfo;
                                      const APtrTileBuf: PETS_TILE_BUFFER;
                                      const APtrVersionBufW: PETS_TILE_VERSION_W): ITileInfoBasic;

    // force connection to ubderlaying storage and create structure (optional)
    function Internal_Force_Underlaying: LongInt;

    // check critical errors from underlaying storage
    function Internal_Critical_Underlaying_Errors(const AErrorResult: LongInt): Boolean;
  protected
    // callbacks
    function GetLinkAuth(const AAuthKind: LongWord; // ETS_AK_* and ETS_AF_* constants
                         const APtrAuthType: PLongWord; // ETS_AT_* and ETS_RF_* constants
                         const APtrAuthSize: PLongWord; // size of AAuthData buffer in bytes
                         const AAuthData: PPETS_AUTH_INFO): LongInt;

    function NotifyConnLost(const APtrConnSize: LongWord;
                            const APtrConnData: PETS_LINK_RECONNECT): LongInt;

    function NotifyConnRest(const APtrConnSize: LongWord;
                            const APtrConnData: PETS_LINK_RECONNECT): LongInt;

  public
    constructor Create;
    destructor Destroy; override;

    // underlaying storage use multiple ContentTypes
    property ContentTypeMixed: Boolean read GetContentTypeMixed;

    // default get/query tile options (never use ETS_TBI_QUERY_INFO here!)
    property QueryTileOptions: LongWord read FQueryTileOptions write FQueryTileOptions;

    // state of link object
    property Initialized: Boolean read FInitialized;
    property Connected: Boolean read FConnected;
    property Underlaying: Boolean read FUnderlaying;

    // periodically sync handler
    function Sync(Sender: TObject): LongInt;

    // delete tile (and/or tne) from storage
    function Delete_Tile_TNE(const AXYZ: PTILE_ID_XYZ;
                             const AVersionInfo: IMapVersionInfo;
                             const ADeleteOptions: LongWord): LongInt;

    // get tile (with tile body)
    function Select_Tile(const AXYZ: PTILE_ID_XYZ;
                         const AVersionInfo: IMapVersionInfo;
                         out ATileInfo: ITileInfoBasic;
                         const AStream: TStream): LongInt;

    // get tile information (without reading tile body)
    function Query_Tile(const AXYZ: PTILE_ID_XYZ;
                        const AVersionInfo: IMapVersionInfo;
                        out ATileInfo: ITileInfoBasic): LongInt;

    // insert tile into storage (and delete tne from storage)
    function Insert_Tile(const AXYZ: PTILE_ID_XYZ;
                         const AVersionInfo: IMapVersionInfo;
                         const ATileBuffer: Pointer;
                         const ATileSize: LongWord;
                         const APtrLoadedUTC: PDateTime): LongInt;

    // insert tne into storage (and delete tile from storage)
    function Insert_TNE(const AXYZ: PTILE_ID_XYZ;
                        const AVersionInfo: IMapVersionInfo): LongInt;

    // execute DDL commands (for tile, for version,...)
    function Execute_DDL(const AXYZ: PTILE_ID_XYZ;
                         const AVersionInfo: IMapVersionInfo): LongInt;

  end;

  TETS_Host_Provider_Basic = class(TObject)
  private
    FExtTileStorage_Func: TETS_Provider_Query_Info;
    FAuthFunc: TExtStorageAuthFunc;
    FProviderHandle: TETS_Provider_Handle;
    FLinks: T_ETS_ObjectList;
    FGlobalStorageIdentifier: String;
    FProviderInternalName: WideString;
    FCS: TRTLCriticalSection;
    // provider functions
    FETS_PQI_PROV_FUNC: TETS_PQI_PROV_FUNC;
    FETS_PQI_PROV_CAPS: TETS_PQI_PROV_CAPS;
    FETS_PQI_LINK_FUNC: TETS_PQI_LINK_FUNC;
    FInitialized: Boolean;
  protected
    function Internal_Set_ETS_Func(const AExtTileStorage_Func: TETS_Provider_Query_Info): LongInt;
    function Internal_Set_GlobalIdentifier(const AGlobalStorageIdentifier: String): LongInt;
    function Internal_Set_Initialized: LongInt;
    function Internal_Uninitialize_ETS: LongInt;
    procedure Internal_Initialize_Zero;
    function Internal_Initialize_ETS: LongInt;
    procedure InternalDelLinkFromList(const ALinkObj: TETS_Host_Link);
  protected
    procedure EnterCS;
    procedure LeaveCS;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    // create new link for storage provider
    function CreateNewLink(const AServiceName: String;
                           const AConnectionInfo: String;
                           const ABaseContentType: String;
                           const AServiceOptions: PETS_SOURCE_STORAGE_OPTIONS): TETS_Host_Link;

    property ProviderInternalName: WideString read FProviderInternalName;

    // request authentication handler (set by EXE)
    property AuthFunc: TExtStorageAuthFunc read FAuthFunc write FAuthFunc;
  end;

  TETS_Host_Provider_EXE = class(TETS_Host_Provider_Basic)
  end;

  TETS_Host_Provider_DLL = class(TETS_Host_Provider_Basic)
  protected
    FDLLHandle: THandle;
  public
    destructor Destroy; override;
  end;

// helpers
function Create_Tile_Storage_EXE(const AExtTileStorage_Func: TETS_Provider_Query_Info;
                                 const AGlobalStorageIdentifier: String): TETS_Host_Provider_EXE;

function Create_Tile_Storage_DLL(const ATile_Storage_DLLName: String;
                                 const AGlobalStorageIdentifier: String): TETS_Host_Provider_DLL;

implementation

uses
  Variants,
  u_TileInfoBasic,
  u_MapVersionInfo,
  u_ContentTypeInfo;

function Create_Tile_Storage_EXE(const AExtTileStorage_Func: TETS_Provider_Query_Info;
                                 const AGlobalStorageIdentifier: String): TETS_Host_Provider_EXE;
begin
  Result:=nil;
  try
    if Assigned(AExtTileStorage_Func) then begin
      Result:=TETS_Host_Provider_EXE.Create;
      Result.Internal_Set_ETS_Func(AExtTileStorage_Func);
      Result.Internal_Set_GlobalIdentifier(AGlobalStorageIdentifier);
      Result.Internal_Set_Initialized;
    end
  except
    FreeAndNil(Result);
  end;
end;

function Create_Tile_Storage_DLL(const ATile_Storage_DLLName: String;
                                 const AGlobalStorageIdentifier: String): TETS_Host_Provider_DLL;
var
  hDLL: THandle;
  proc_addr: Pointer;
begin
  hDLL:=0;
  Result:=nil;
  try
    if FileExists(ATile_Storage_DLLName) then begin
      hDLL:=LoadLibrary(PChar(ATile_Storage_DLLName));
      if (0<>hDLL) then begin
        // loaded
        proc_addr:=GetProcAddress(hDLL, c_ETS_Provider_Query_Info);
        if Assigned(proc_addr) then begin
          // main proc found
          Result:=TETS_Host_Provider_DLL.Create;
          Result.FDLLHandle:=hDLL;
          hDLL:=0;
          Result.Internal_Set_ETS_Func(proc_addr);
          Result.Internal_Set_GlobalIdentifier(AGlobalStorageIdentifier);
          Result.Internal_Set_Initialized;
        end else begin
          // no main proc
          SysUtils.Abort;
        end;
      end;
    end;
  except
    FreeAndNil(Result);
    if (0<>hDLL) then
      FreeLibrary(hDLL);
  end;
end;

procedure r_ETS_Free_Buffer(const p: Pointer); inline;
begin
  FreeMemory(p)
end;

function r_ETS_Alloc_Buffer(const ASize: Integer): Pointer; inline;
begin
  Result:=GetMemory(ASize)
end;

function r_ETS_Alloc_StrA(const ASource: AnsiString): PAnsiChar;
var L: Integer;
begin
  L:=Length(ASource)+1;
  Result:=GetMemory(L);
  CopyMemory(Result, PAnsiChar(ASource), L);
end;

function r_ETS_Alloc_StrW(const ASource: WideString): PWideChar; inline;
var L: Integer;
begin
  L:=(Length(ASource)+1)*sizeof(WideChar);
  Result:=GetMemory(L);
  CopyMemory(Result, PWideChar(ASource), L);
end;

// see ETS_LSIC_AUTH_FUNC
function r_ETS_Link_Auth_Get(const ALinkHandle: TETS_Link_Handle;
                             const AAuthPtrValue: Pointer; // host-defined auth pointer
                             const AAuthKind: LongWord; // ETS_AK_* and ETS_AF_* constants
                             const APtrAuthType: PLongWord; // ETS_AT_* and ETS_RF_* constants
                             const APtrAuthSize: PLongWord; // size of AAuthData buffer in bytes
                             const AAuthData: PPETS_AUTH_INFO): LongInt; stdcall;
begin
  try
    if (nil=AAuthPtrValue) or (nil=ALinkHandle) then begin
      // not defined
      Result:=ETSR_NO_MANDATORY_PARAMETER;
    end else if (ALinkHandle=TETS_Host_Link(AAuthPtrValue).FLinkHandle) then begin
      // storage provider requests authentication information
      Result:=TETS_Host_Link(AAuthPtrValue).GetLinkAuth(AAuthKind,APtrAuthType,APtrAuthSize,AAuthData);
    end else begin
      // invalid pointer(s)
      Result:=ETSR_INVALID_OBJECT_POINTER;
    end;
  except
    Result:=ETSR_HOST_EXCEPTION;
  end;
end;

// see ETS_LSIC_AUTH_FREE
function r_ETS_Link_Auth_Free(const ALinkHandle: TETS_Link_Handle;
                              const AAuthPtrValue: Pointer; // host-defined auth pointer
                              const AAuthKind: LongWord; // ETS_AK_* and ETS_AF_* constants
                              const APtrAuthType: PLongWord; // ETS_AT_* and ETS_RF_* constants
                              const APtrAuthSize: PLongWord; // size of AAuthData buffer in bytes
                              const AAuthData: PPETS_AUTH_INFO): LongInt; stdcall;
begin
  if (nil=APtrAuthType) or (nil=AAuthData) or (nil=AAuthData^) then begin
    Result:=ETSR_NO_MANDATORY_PARAMETER;
    Exit;
  end;

  try
    // free buffers
    if (ETS_AT_LOGIN_PWD=(APtrAuthType^ and ETS_AT_LOGIN_PWD))
       OR
       (ETS_AT_DOMAIN_LOGIN_PWD=(APtrAuthType^ and ETS_AT_DOMAIN_LOGIN_PWD)) then begin
      // free login and password, then buffer
      if (ETS_AF_ANSI_VERSION=(AAuthKind and ETS_AF_ANSI_VERSION)) then begin
        // TETS_AUTH_LOGIN_PWD_A - ansi if requested
        with PETS_AUTH_LOGIN_PWD_A(AAuthData^)^ do begin
          r_ETS_Free_Buffer(szDomain);
          szDomain:=nil;
          r_ETS_Free_Buffer(szLogin);
          szLogin:=nil;
          r_ETS_Free_Buffer(szPassword);
          szPassword:=nil;
        end;
      end else begin
        // TETS_AUTH_LOGIN_PWD_W - unicode by default
        with PETS_AUTH_LOGIN_PWD_W(AAuthData^)^ do begin
          r_ETS_Free_Buffer(szDomain);
          szDomain:=nil;
          r_ETS_Free_Buffer(szLogin);
          szLogin:=nil;
          r_ETS_Free_Buffer(szPassword);
          szPassword:=nil;
        end;
      end;
      r_ETS_Free_Buffer(AAuthData^);
      AAuthData^:=nil;
      Result:=ETSR_OK;
    end else begin
      // others: just free buffer if exists
      r_ETS_Free_Buffer(AAuthData^);
      AAuthData^:=nil;
      Result:=ETSR_OK;
    end;
  except
    Result:=ETSR_HOST_EXCEPTION;
  end;
end;

function r_ETS_Link_Connection_Lost(const ALinkHandle: TETS_Link_Handle;
                                    const AConnPtrValue: Pointer; // host-defined callback pointer
                                    const APtrConnSize: LongWord; // size of APtrConnData^ buffer
                                    const APtrConnData: PETS_LINK_RECONNECT): LongInt; stdcall;
begin
  // if (nil=APtrConnData) - storage provider cannot reconnect - so should disconnect manually
  // do not check it here
  if (nil=AConnPtrValue) or (nil=ALinkHandle) then begin
    Result:=ETSR_NO_MANDATORY_PARAMETER;
    Exit;
  end;

  try
    if (ALinkHandle=TETS_Host_Link(AConnPtrValue).FLinkHandle) then begin
      // lost connection handler
      Result:=TETS_Host_Link(AConnPtrValue).NotifyConnLost(APtrConnSize, APtrConnData);
    end else begin
      // invalid pointer(s)
      Result:=ETSR_INVALID_OBJECT_POINTER;
    end;
  except
    Result:=ETSR_HOST_EXCEPTION;
  end;
end;

function r_ETS_Link_Connection_Rest(const ALinkHandle: TETS_Link_Handle;
                                    const AConnPtrValue: Pointer; // host-defined callback pointer
                                    const APtrConnSize: LongWord; // size of APtrConnData^ buffer
                                    const APtrConnData: PETS_LINK_RECONNECT): LongInt; stdcall;
begin
  // here APtrConnData is mandatory!
  if (nil=AConnPtrValue) or (nil=ALinkHandle) or (nil=APtrConnData) then begin
    Result:=ETSR_NO_MANDATORY_PARAMETER;
    Exit;
  end;

  try
    if (ALinkHandle=TETS_Host_Link(AConnPtrValue).FLinkHandle) then begin
      // restored connection handler
      Result:=TETS_Host_Link(AConnPtrValue).NotifyConnRest(APtrConnSize, APtrConnData);
    end else begin
      // invalid pointer(s)
      Result:=ETSR_INVALID_OBJECT_POINTER;
    end;
  except
    Result:=ETSR_HOST_EXCEPTION;
  end;
end;


{ TETS_Host_Provider_DLL }

destructor TETS_Host_Provider_DLL.Destroy;
begin
  inherited;
  // unload library
  if (0<>FDLLHandle) then begin
    FreeLibrary(FDLLHandle);
    FDLLHandle:=0;
  end;
end;

{ TETS_Host_Provider_Basic }

constructor TETS_Host_Provider_Basic.Create;
begin
  FInitialized:=FALSE;
  FProviderHandle:=nil;
  FExtTileStorage_Func:=nil;
  FAuthFunc:=nil;
  FLinks.SetZero;
  InitializeCriticalSection(FCS);
  Internal_Initialize_Zero;
end;

function TETS_Host_Provider_Basic.CreateNewLink(const AServiceName: String;
                                                const AConnectionInfo: String;
                                                const ABaseContentType: String;
                                                const AServiceOptions: PETS_SOURCE_STORAGE_OPTIONS): TETS_Host_Link;
begin
  Result:=nil;
  EnterCS;
  try
    // not initialized
    if (not Assigned(FExtTileStorage_Func)) then
      Exit;

    // other checks
    if (not FInitialized) then
      Exit;
      
    // create link in several steps
    Result:=TETS_Host_Link.Create;
    Result.FHostProvider:=Self;
    FLinks.AddItem(Result);
    // initialize basic parameters
    Result.Internal_Initialize_ETS;
    // initialize connection parameters (after this step storage provider can connect to storage)
    Result.Internal_Connect_ETS(AServiceName, AConnectionInfo);
    // apply source params to storage
    Result.Internal_Apply_Source_Params(ABaseContentType, AServiceOptions);
    // and finally get parameters from underlaying storage
    Result.Internal_Underlaying;
  finally
    LeaveCS;
  end;
end;

destructor TETS_Host_Provider_Basic.Destroy;
begin
  Internal_Uninitialize_ETS;
  DeleteCriticalSection(FCS);
  inherited;
end;

procedure TETS_Host_Provider_Basic.EnterCS;
begin
  EnterCriticalSection(FCS);
end;

procedure TETS_Host_Provider_Basic.InternalDelLinkFromList(const ALinkObj: TETS_Host_Link);
begin
  EnterCS;
  try
    FLinks.DelItem(ALinkObj);
  finally
    LeaveCS;
  end;
end;

function TETS_Host_Provider_Basic.Internal_Initialize_ETS: LongInt;
var
  VETS_PQI_NAME_W: TETS_PQI_NAME_W;
  s: AnsiString;
begin
  // get provider functions
  Result := FExtTileStorage_Func(nil, ETS_PQIC_PROV_FUNC, sizeof(FETS_PQI_PROV_FUNC), @FETS_PQI_PROV_FUNC);
  if (ETSR_OK<>Result) then
    Exit;

  // check all functions
  if (not Assigned(FETS_PQI_PROV_FUNC.p_Provider_Open)) or
     (not Assigned(FETS_PQI_PROV_FUNC.p_Provider_Close)) or
     (not Assigned(FETS_PQI_PROV_FUNC.p_Provider_Set_Info)) or
     (not Assigned(FETS_PQI_PROV_FUNC.p_Link_Open)) or
     (not Assigned(FETS_PQI_PROV_FUNC.p_Link_Close)) then begin
    Result:=ETSR_NO_MANDATORY_FUNCTIONS;
    Exit;
  end;

  // open provider
  Result := FETS_PQI_PROV_FUNC.p_Provider_Open(Pointer(Self), @FProviderHandle, 0);
  if (ETSR_OK<>Result) then
    Exit;

  // get capabilities
  Result := FExtTileStorage_Func(FProviderHandle, ETS_PQIC_PROV_CAPS, sizeof(FETS_PQI_PROV_CAPS), @FETS_PQI_PROV_CAPS);
  if (ETSR_OK<>Result) then
    Exit;

  // check at least one tile_id format supported
  if (0=(FETS_PQI_PROV_CAPS.dwTileIdFormats and TILE_ID_FORMAT_MASK)) then begin
    Result:=ETSR_NO_TILEID_FORMATS;
    Exit;
  end;

  // get internal storage name
  Result := FExtTileStorage_Func(FProviderHandle, ETS_PQIC_NAME_W, sizeof(VETS_PQI_NAME_W), @VETS_PQI_NAME_W);
  if (ETSR_OK=Result) then begin
    FProviderInternalName:=VETS_PQI_NAME_W.szName;
  end else begin
    // try ansi version (same struct)
    Result := FExtTileStorage_Func(FProviderHandle, ETS_PQIC_NAME_A, sizeof(VETS_PQI_NAME_W), @VETS_PQI_NAME_W);
    if (ETSR_OK=Result) then begin
      s:=(TETS_PQI_NAME_A(VETS_PQI_NAME_W).szName);
      FProviderInternalName:=s;
    end else begin
      // internal name not supported
      FProviderInternalName:='';
    end;
  end;

  // get link functions
  Result := FExtTileStorage_Func(FProviderHandle, ETS_PQIC_LINK_FUNC, sizeof(FETS_PQI_LINK_FUNC), @FETS_PQI_LINK_FUNC);
  if (ETSR_OK<>Result) then
    Exit;

  // check mandatory functions exist
  if (not Assigned(FETS_PQI_LINK_FUNC.p_Link_Set_Info)) or (not Assigned(FETS_PQI_LINK_FUNC.p_Link_Query_Info)) then begin
    Result:=ETSR_NO_MANDATORY_FUNCTIONS;
    Exit;
  end;

  // check group of routines (for transactions)
  if (ETS_SUPPORT_TRANSACTIONS=(ETS_SUPPORT_TRANSACTIONS and FETS_PQI_PROV_CAPS.dwSupport)) then begin
    // supports transactions - all "set" routines (except trancount) should be defined
    if (not Assigned(FETS_PQI_LINK_FUNC.p_Link_BeginTran)) or
       (not Assigned(FETS_PQI_LINK_FUNC.p_Link_Commit)) or
       (not Assigned(FETS_PQI_LINK_FUNC.p_Link_Rollback)) then begin
      Result:=ETSR_NULL_TRANSACTION_ROUTINES;
      Exit;
    end;
  end else begin
    // no transactions supported - just clear links to ALL transactions' routines
    FETS_PQI_LINK_FUNC.p_Link_BeginTran:=nil;
    FETS_PQI_LINK_FUNC.p_Link_Commit:=nil;
    FETS_PQI_LINK_FUNC.p_Link_Rollback:=nil;
    FETS_PQI_LINK_FUNC.p_Link_TranCount:=nil;
  end;

  // set conversion routine
  Result:=FETS_PQI_PROV_FUNC.p_Provider_Set_Info(FProviderHandle, ETS_PSIC_CONVERSION, 0, @ETS_TileId_Conversion_Routine);
  if (ETSR_OK<>Result) then
    Exit;
end;

procedure TETS_Host_Provider_Basic.Internal_Initialize_Zero;
begin
  FGlobalStorageIdentifier:='';
  FProviderInternalName:='';
  ZeroMemory(@FETS_PQI_PROV_FUNC, sizeof(FETS_PQI_PROV_FUNC));
  ZeroMemory(@FETS_PQI_PROV_CAPS, sizeof(FETS_PQI_PROV_CAPS));
  ZeroMemory(@FETS_PQI_LINK_FUNC, sizeof(FETS_PQI_LINK_FUNC));
end;

function TETS_Host_Provider_Basic.Internal_Set_ETS_Func(const AExtTileStorage_Func: TETS_Provider_Query_Info): LongInt;
begin
  EnterCS;
  try
    // cleanup existing
    Internal_Uninitialize_ETS;

    // apply (save) new function
    FExtTileStorage_Func:=AExtTileStorage_Func;
    if (not Assigned(AExtTileStorage_Func)) then begin
      Result:=ETSR_NO_ROOT_FUNCTION;
      Exit;
    end;

    // initialize - zero mem
    Internal_Initialize_Zero;

    // initialize - create objects
    Result:=Internal_Initialize_ETS;
  finally
    LeaveCS;
  end;
end;

function TETS_Host_Provider_Basic.Internal_Set_GlobalIdentifier(const AGlobalStorageIdentifier: String): LongInt;
var
  Vws: WideString;
  Vas: AnsiString;
begin
  EnterCS;
  try
    if (FGlobalStorageIdentifier<>AGlobalStorageIdentifier) then begin
      FGlobalStorageIdentifier:=AGlobalStorageIdentifier;
      if (nil<>FProviderHandle) and Assigned(FETS_PQI_PROV_FUNC.p_Provider_Set_Info) then begin
        // try unicode version
        Vws:=AGlobalStorageIdentifier;
        Result:=FETS_PQI_PROV_FUNC.p_Provider_Set_Info(FProviderHandle, ETS_PSIC_GLOBALID_W, 0, PWideChar(Vws));
        if (ETSR_OK<>Result) then begin
          // try ansi version
          Vas:=AGlobalStorageIdentifier;
          Result:=FETS_PQI_PROV_FUNC.p_Provider_Set_Info(FProviderHandle, ETS_PSIC_GLOBALID_A, 0, PAnsiChar(Vas));
        end;
      end else begin
        // not opened
        Result:=ETSR_NOT_CONNECTED;
      end;
    end else begin
      // same value - nothing to do
      Result:=ETSR_OK;
    end;
  finally
    LeaveCS;
  end;
end;

function TETS_Host_Provider_Basic.Internal_Set_Initialized: LongInt;
begin
  FInitialized:=TRUE;
  Result:=ETSR_OK;
end;

function TETS_Host_Provider_Basic.Internal_Uninitialize_ETS: LongInt;
begin
  EnterCS;
  try
    FInitialized:=FALSE;
    Result:=ETSR_OK;

    FLinks.FreeALL;

    if (nil<>FProviderHandle) then begin
      if Assigned(FETS_PQI_PROV_FUNC.p_Provider_Close) then
        Result:=FETS_PQI_PROV_FUNC.p_Provider_Close(FProviderHandle);
      FProviderHandle:=nil;
    end;

    ZeroMemory(@FETS_PQI_PROV_FUNC, sizeof(FETS_PQI_PROV_FUNC));

    FExtTileStorage_Func:=nil;
  finally
    LeaveCS;
  end;
end;

procedure TETS_Host_Provider_Basic.LeaveCS;
begin
  LeaveCriticalSection(FCS);
end;

{ TETS_Host_Link }

constructor TETS_Host_Link.Create;
begin
  FHostProvider:=nil;
  FQueryTileOptions:=0;
  FSynchronizer := TMultiReadExclusiveWriteSynchronizer.Create;
  Internal_Initialize_Zero;
end;

function TETS_Host_Link.Delete_Tile_TNE(const AXYZ: PTILE_ID_XYZ;
                                        const AVersionInfo: IMapVersionInfo;
                                        const ADeleteOptions: LongWord): LongInt;
var
  VVersionW: WideString;
  VVersionA: AnsiString;
begin
  if (not FConnected) then begin
    Result:=ETSR_NOT_CONNECTED;
    Exit;
  end;

  if (not FUnderlaying) then begin
    Result:=Internal_Force_Underlaying;
    if (ETSR_OK<>Result) then
      Exit;
  end;

  FSynchronizer.BeginRead;
  try
    if Assigned(FETS_LQI_TILE_ROUTINES_W.p_Tile_Delete_W) then begin
      // use unicode version
      VVersionW:=AVersionInfo.StoreString;
      Result:=FETS_LQI_TILE_ROUTINES_W.p_Tile_Delete_W(FLinkHandle, nil, AXYZ, PWideChar(VVersionW), ADeleteOptions);
    end else if Assigned(FETS_LQI_TILE_ROUTINES_A.p_Tile_Delete_A) then begin
      // use ansi version
      VVersionA:=AVersionInfo.StoreString;
      Result:=FETS_LQI_TILE_ROUTINES_A.p_Tile_Delete_A(FLinkHandle, nil, AXYZ, PAnsiChar(VVersionA), ADeleteOptions);
    end else begin
      // not supported
      Result:=ETSR_NOT_SUPPORTED;
    end;
  finally
    FSynchronizer.EndRead;
  end;
end;

destructor TETS_Host_Link.Destroy;
begin
  FUnderlaying:=FALSE;
  FConnected:=FALSE;
  if Assigned(FHostProvider) then
    FHostProvider.InternalDelLinkFromList(Self);
  Internal_Uninitialize_ETS;
  FContentTypeBasic:=nil;
  FreeAndNil(FSynchronizer);
  inherited;
end;

function TETS_Host_Link.Execute_DDL(const AXYZ: PTILE_ID_XYZ;
                                    const AVersionInfo: IMapVersionInfo): LongInt;
var
  VVersionW: WideString;
  VVersionA: AnsiString;
  VVersionBufW: TETS_TILE_VERSION_W;
  VVerPtr: Pointer;
begin
  if (not FConnected) then begin
    Result:=ETSR_NOT_CONNECTED;
    Exit;
  end;

  FSynchronizer.BeginWrite;
  try
    VVerPtr:=nil;
    ZeroMemory(@VVersionBufW, sizeof(VVersionBufW));

    if Assigned(FETS_LQI_TILE_ROUTINES_W.p_Ddl_Exec_W) then begin
      // use unicode version
      if Assigned(AVersionInfo) then begin
        VVersionW:=AVersionInfo.StoreString;
        VVersionBufW.szVersion:=PWideChar(VVersionW);
        VVerPtr:=@VVersionBufW;
      end;
      Result:=FETS_LQI_TILE_ROUTINES_W.p_Ddl_Exec_W(FLinkHandle, nil, AXYZ, VVerPtr);
    end else if Assigned(FETS_LQI_TILE_ROUTINES_A.p_Ddl_Exec_A) then begin
      // use ansi version
      if Assigned(AVersionInfo) then begin
        VVersionA:=AVersionInfo.StoreString;
        TETS_TILE_VERSION_A(VVersionBufW).szVersion:=PAnsiChar(VVersionA);
        VVerPtr:=@VVersionBufW;
      end;
      Result:=FETS_LQI_TILE_ROUTINES_A.p_Ddl_Exec_A(FLinkHandle, nil, AXYZ, VVerPtr);
    end else begin
      // not supported
      Result:=ETSR_NOT_SUPPORTED;
    end;
  finally
    FSynchronizer.EndWrite;
  end;
end;

function TETS_Host_Link.GetContentTypeMixed: Boolean;
begin
  Result := (ETS_CONTENT_TYPE_MIXED = (FContentTypeOptions and ETS_CONTENT_TYPE_MIXED));
end;

function TETS_Host_Link.GetLinkAuth(const AAuthKind: LongWord;
                                    const APtrAuthType: PLongWord;
                                    const APtrAuthSize: PLongWord;
                                    const AAuthData: PPETS_AUTH_INFO): LongInt;
var
  VAuthResult: Boolean;
  VDomain, VLogin, VPassword: WideString;
begin
  // check no target pointer to return type of auth
  if (nil=APtrAuthType) then begin
    Result:=ETSR_NO_MANDATORY_PARAMETER;
    Exit;
  end;

  // not initialized - error - but actually finished by user
  if (not FInitialized) then begin
    APtrAuthType^:=ETS_AT_NO;
    Result:=ETSR_ABORTED_BY_USER;
    Exit;
  end;

  //
  if Assigned(FHostProvider) then
  if Assigned(FHostProvider.FAuthFunc) then begin
    // call host
    VDomain:='';
    VLogin:='';
    VPassword:='';
    VAuthResult:=FHostProvider.FAuthFunc(Self,
                                         FHostProvider.FGlobalStorageIdentifier,
                                         FServiceName,
                                         FConnectionInfo,
                                         AAuthKind,
                                         VDomain,
                                         VLogin,
                                         VPassword,
                                         APtrAuthType);
    if (VAuthResult) then begin
      // ok - make response to storage
      if (ETS_AT_LOGIN_PWD=(APtrAuthType^ and ETS_AT_LOGIN_PWD))
         OR
         (ETS_AT_DOMAIN_LOGIN_PWD=(APtrAuthType^ and ETS_AT_DOMAIN_LOGIN_PWD)) then begin
        // make buffer for domain, login and password
        if (ETS_AF_ANSI_VERSION=(AAuthKind and ETS_AF_ANSI_VERSION)) then begin
          // ansi version - if flag is set (make TETS_AUTH_LOGIN_PWD_A)
          AAuthData^:=r_ETS_Alloc_Buffer(sizeof(TETS_AUTH_LOGIN_PWD_A));
          ZeroMemory(AAuthData^, sizeof(TETS_AUTH_LOGIN_PWD_A));
          if (ETS_RF_USE_DOMAIN=(APtrAuthType^ and ETS_RF_USE_DOMAIN)) then
            PETS_AUTH_LOGIN_PWD_A(AAuthData^)^.szDomain:=r_ETS_Alloc_StrA(VDomain);
          if (ETS_RF_USE_LOGIN=(APtrAuthType^ and ETS_RF_USE_LOGIN)) then
            PETS_AUTH_LOGIN_PWD_A(AAuthData^)^.szLogin:=r_ETS_Alloc_StrA(VLogin);
          if (ETS_RF_USE_PASSWORD=(APtrAuthType^ and ETS_RF_USE_PASSWORD)) then
            PETS_AUTH_LOGIN_PWD_A(AAuthData^)^.szPassword:=r_ETS_Alloc_StrA(VPassword);
          Result:=ETSR_OK;
        end else begin
          // unicode version - by default (make TETS_AUTH_LOGIN_PWD_W)
          AAuthData^:=r_ETS_Alloc_Buffer(sizeof(TETS_AUTH_LOGIN_PWD_W));
          ZeroMemory(AAuthData^, sizeof(TETS_AUTH_LOGIN_PWD_W));
          if (ETS_RF_USE_DOMAIN=(APtrAuthType^ and ETS_RF_USE_DOMAIN)) then
            PETS_AUTH_LOGIN_PWD_W(AAuthData^)^.szDomain:=r_ETS_Alloc_StrW(VDomain);
          if (ETS_RF_USE_LOGIN=(APtrAuthType^ and ETS_RF_USE_LOGIN)) then
            PETS_AUTH_LOGIN_PWD_W(AAuthData^)^.szLogin:=r_ETS_Alloc_StrW(VLogin);
          if (ETS_RF_USE_PASSWORD=(APtrAuthType^ and ETS_RF_USE_PASSWORD)) then
            PETS_AUTH_LOGIN_PWD_W(AAuthData^)^.szPassword:=r_ETS_Alloc_StrW(VPassword);
          Result:=ETSR_OK;
        end;
      end else begin
        // no additional authentication info
        Result:=ETSR_OK;
      end;
    end else begin
      // cancelled
      APtrAuthType^:=ETS_AT_NO;
      Result:=ETSR_ABORTED_BY_USER;
    end;
    Exit;
  end;

  // not supported
  APtrAuthType^:=ETS_AT_NO;
  Result:=ETSR_NOT_SUPPORTED;
end;

function TETS_Host_Link.Insert_Tile(const AXYZ: PTILE_ID_XYZ;
                                    const AVersionInfo: IMapVersionInfo;
                                    const ATileBuffer: Pointer;
                                    const ATileSize: LongWord;
                                    const APtrLoadedUTC: PDateTime): LongInt;
var
  VVersionW,VContentTypeW: WideString;
  VVersionA,VContentTypeA: AnsiString;
  VTileBuf: TETS_TILE_BUFFER;
  VVersionBufW: TETS_TILE_VERSION_W;
begin
  if (not FConnected) then begin
    Result:=ETSR_NOT_CONNECTED;
    Exit;
  end;

  if (not FUnderlaying) then begin
    Result:=Internal_Force_Underlaying;
    if (ETSR_OK<>Result) then
      Exit;
  end;

  FSynchronizer.BeginRead;
  try
    // common params
    ZeroMemory(@VVersionBufW, sizeof(VVersionBufW));
    ZeroMemory(@VTileBuf, sizeof(VTileBuf));
    if (nil=APtrLoadedUTC) then
      VTileBuf.dtLoadedUTC:=Now
    else
      VTileBuf.dtLoadedUTC:=APtrLoadedUTC^; // for exact copy of tile from one storage to another
    VTileBuf.ptrTileBuffer:=ATileBuffer;
    VTileBuf.dwTileSize:=ATileSize;

    if Assigned(FETS_LQI_TILE_ROUTINES_W.p_Tile_Insert_W) then begin
      // use unicode version
      VVersionW:=AVersionInfo.StoreString;
      VVersionBufW.szVersion:=PWideChar(VVersionW);
      VContentTypeW:=FContentTypeBasic.GetContentType;
      VVersionBufW.szContentType:=PWideChar(VContentTypeW);
      Result:=FETS_LQI_TILE_ROUTINES_W.p_Tile_Insert_W(FLinkHandle, nil, AXYZ, @VTileBuf, @VVersionBufW);
    end else if Assigned(FETS_LQI_TILE_ROUTINES_A.p_Tile_Insert_A) then begin
      // use ansi version
      VVersionA:=AVersionInfo.StoreString;
      TETS_TILE_VERSION_A(VVersionBufW).szVersion:=PAnsiChar(VVersionA);
      VContentTypeA:=FContentTypeBasic.GetContentType;
      TETS_TILE_VERSION_A(VVersionBufW).szContentType:=PAnsiChar(VContentTypeA);
      Result:=FETS_LQI_TILE_ROUTINES_A.p_Tile_Insert_A(FLinkHandle, nil, AXYZ, @VTileBuf, PETS_TILE_VERSION_A(@VVersionBufW));
    end else begin
      // not supported
      Result:=ETSR_NOT_SUPPORTED;
    end;
  finally
    FSynchronizer.EndRead;
  end;
end;

function TETS_Host_Link.Insert_TNE(const AXYZ: PTILE_ID_XYZ;
                                   const AVersionInfo: IMapVersionInfo): LongInt;
var
  VVersionW: WideString;
  VVersionA: AnsiString;
  VVersionBufW: TETS_TILE_VERSION_W;
begin
  if (not FConnected) then begin
    Result:=ETSR_NOT_CONNECTED;
    Exit;
  end;

  if (not FUnderlaying) then begin
    Result:=Internal_Force_Underlaying;
    if (ETSR_OK<>Result) then
      Exit;
  end;

  FSynchronizer.BeginRead;
  try
    ZeroMemory(@VVersionBufW, sizeof(VVersionBufW));

    if Assigned(FETS_LQI_TILE_ROUTINES_W.p_Tne_Create_W) then begin
      // use unicode version
      VVersionW:=AVersionInfo.StoreString;
      VVersionBufW.szVersion:=PWideChar(VVersionW);
      Result:=FETS_LQI_TILE_ROUTINES_W.p_Tne_Create_W(FLinkHandle, nil, AXYZ, @VVersionBufW);
    end else if Assigned(FETS_LQI_TILE_ROUTINES_A.p_Tne_Create_A) then begin
      // use ansi version
      VVersionA:=AVersionInfo.StoreString;
      TETS_TILE_VERSION_A(VVersionBufW).szVersion:=PAnsiChar(VVersionA);
      Result:=FETS_LQI_TILE_ROUTINES_A.p_Tne_Create_A(FLinkHandle, nil, AXYZ, PETS_TILE_VERSION_A(@VVersionBufW));
    end else begin
      // not supported
      Result:=ETSR_NOT_SUPPORTED;
    end;
  finally
    FSynchronizer.EndRead;
  end;
end;

function TETS_Host_Link.Internal_Apply_Source_Params(const ABaseContentType: String;
                                                     const AServiceOptions: PETS_SOURCE_STORAGE_OPTIONS): LongInt;
var
  Vws: WideString;
  Vas: AnsiString;
begin
  FSynchronizer.BeginWrite;
  try
    // no storage
    if (nil=FHostProvider) then begin
      Result:=ETSR_NO_ROOT_FUNCTION;
      Exit;
    end;

    // not initialized
    if (not FInitialized) then begin
      Result:=ETSR_NOT_INITIALIZED;
      Exit;
    end;

    // already connected
    if FConnected then begin
      Result:=ETSR_ALREADY_CONNECTED;
      Exit;
    end;

    // apply base content type
    if Assigned(FHostProvider.FETS_PQI_LINK_FUNC.p_Link_Set_Info) then begin
      // unicode version
      Vws:=ABaseContentType;
      Result:=FHostProvider.FETS_PQI_LINK_FUNC.p_Link_Set_Info(FLinkHandle, ETS_LSIC_CONTENTTYPE_W, 0, PWideChar(Vws));
      if (ETSR_OK<>Result) then begin
        // try ansi version
        Vas:=ABaseContentType;
        Result:=FHostProvider.FETS_PQI_LINK_FUNC.p_Link_Set_Info(FLinkHandle, ETS_LSIC_CONTENTTYPE_A, 0, PAnsiChar(Vas));
        if (ETSR_OK<>Result) then
          Exit;
      end;

      // apply service options
      Result:=FHostProvider.FETS_PQI_LINK_FUNC.p_Link_Set_Info(FLinkHandle, ETS_LSIC_SRC_STORAGE_OPT, sizeof(AServiceOptions^), AServiceOptions);
    end else begin
      // not supported
      Result:=ETSR_NOT_SUPPORTED;
    end;
  finally
    FSynchronizer.EndWrite;
  end;
end;

function TETS_Host_Link.Internal_Connect_ETS(const AServiceName, AConnectionInfo: String): LongInt;
var
  Vws: WideString;
  Vas: AnsiString;
begin
  FSynchronizer.BeginWrite;
  try
    // no storage
    if (nil=FHostProvider) then begin
      Result:=ETSR_NO_ROOT_FUNCTION;
      Exit;
    end;

    // not initialized
    if (not FInitialized) then begin
      Result:=ETSR_NOT_INITIALIZED;
      Exit;
    end;

    // already connected
    if FConnected then begin
      Result:=ETSR_ALREADY_CONNECTED;
      Exit;
    end;  

    // TODO: notify about changes in cache and storage params (AGlobalStorageIdentifier, AServiceName, AConnectionInfo)
    // and (maybe) reconnect to another underlaying storage
    // OR force kill current link to storage and create new one
  
    // keep values
    FServiceName:=AServiceName;
    FConnectionInfo:=AConnectionInfo;

    // apply service name
    if Assigned(FHostProvider.FETS_PQI_LINK_FUNC.p_Link_Set_Info) then begin
      // unicode version
      Vws:=AServiceName;
      Result:=FHostProvider.FETS_PQI_LINK_FUNC.p_Link_Set_Info(FLinkHandle, ETS_LSIC_SERVICENAME_W, 0, PWideChar(Vws));
      if (ETSR_OK<>Result) then begin
        // try ansi version
        Vas:=AServiceName;
        Result:=FHostProvider.FETS_PQI_LINK_FUNC.p_Link_Set_Info(FLinkHandle, ETS_LSIC_SERVICENAME_A, 0, PAnsiChar(Vas));
        if (ETSR_OK<>Result) then
          Exit;
      end;
    end;

    // apply connection info
    if Assigned(FHostProvider.FETS_PQI_LINK_FUNC.p_Link_Set_Info) then begin
      // unicode version
      Vws:=AConnectionInfo;
      Result:=FHostProvider.FETS_PQI_LINK_FUNC.p_Link_Set_Info(FLinkHandle, ETS_LSIC_CONNECTIONINFO_W, 0, PWideChar(Vws));
      if (ETSR_OK<>Result) then begin
        // try ansi version
        Vas:=AConnectionInfo;
        Result:=FHostProvider.FETS_PQI_LINK_FUNC.p_Link_Set_Info(FLinkHandle, ETS_LSIC_CONNECTIONINFO_A, 0, PAnsiChar(Vas));
        if (ETSR_OK<>Result) then
          Exit;
      end;
    end;

    // after ETS_LSIC_CONNECTIONINFO_* storage provider can connect to storage (and ask about authentication information)
    FConnected:=TRUE;
    Result:=ETSR_OK;
  finally
    FSynchronizer.EndWrite;
  end;
end;

function TETS_Host_Link.Internal_Create_TileInfo(const AAnsiVersion: Boolean;
                                                 const AVersionInfo: IMapVersionInfo;
                                                 const APtrTileBuf: PETS_TILE_BUFFER;
                                                 const APtrVersionBufW: PETS_TILE_VERSION_W): ITileInfoBasic;
var
  VCurrentVersionInfo: IMapVersionInfo;
  VContentType: IContentTypeInfoBasic;
  Vws: WideString;
  Vas: AnsiString;
begin
  // requested version by default
  VContentType:=nil;
  VCurrentVersionInfo:=AVersionInfo;

  // may be another version
  if (nil<>APtrVersionBufW^.szVersion) then begin
    if AAnsiVersion then begin
      // ansi
      Vas:=PETS_TILE_VERSION_A(APtrVersionBufW)^.szVersion;
      VCurrentVersionInfo:=TMapVersionInfo.Create(Vas);
    end else begin
      // unicode
      Vws:=APtrVersionBufW^.szVersion;
      VCurrentVersionInfo:=TMapVersionInfo.Create(Vws);
    end;
  end;

  if (ETS_TBO_TILE_EXISTS = (APtrTileBuf^.dwOptionsOut and ETS_TBO_TILE_EXISTS)) then begin
    // tile exists - make content_type
    if (nil=APtrVersionBufW^.szContentType) then begin
      // use default content_type
      VContentType:=FContentTypeBasic;
    end else begin
      // make from current value
      if AAnsiVersion then begin
        // ansi version
        Vas:=PETS_TILE_VERSION_A(APtrVersionBufW)^.szContentType;
        VContentType:=TContentTypeInfoBase.Create(Vas, '');
      end else begin
        // unicode version
        Vws:=APtrVersionBufW^.szContentType;
        VContentType:=TContentTypeInfoBase.Create(Vws, '');
      end;
    end;
    Result:=TTileInfoBasicExists.Create(APtrTileBuf^.dtLoadedUTC,
                                        APtrTileBuf^.dwTileSize,
                                        VCurrentVersionInfo,
                                        VContentType);
  end else if (ETS_TBO_TNE_EXISTS = (APtrTileBuf^.dwOptionsOut and ETS_TBO_TNE_EXISTS)) then begin
    // tne exists, tile not found
    Result:=TTileInfoBasicTNE.Create(APtrTileBuf^.dtLoadedUTC, VCurrentVersionInfo);
  end else begin
    // neither tile nor tne
    Result:=TTileInfoBasicNotExists.Create(0, VCurrentVersionInfo);
  end;
end;

function TETS_Host_Link.Internal_Critical_Underlaying_Errors(const AErrorResult: Integer): Boolean;
begin
  Result:=(ETSR_UNDERLAYING_UNAVAILABLE=AErrorResult) or
          (ETSR_UNDERLAYING_UNATHORIZED=AErrorResult) or
          (ETSR_UNDERLAYING_RESTRICTED=AErrorResult) or
          (ETSR_UNDERLAYING_WITHOUT_STRUCTURE=AErrorResult) or
          (ETSR_UNDERLAYING_INVALID_STRUCTURE=AErrorResult) or
          (ETSR_UNDERLAYING_CRITICAL_ERROR=AErrorResult)
end;

function TETS_Host_Link.Internal_Force_Underlaying: LongInt;
begin
  // probably no connection to underlaying storage
  // or underlaying storage has invalid structure (or without structure)

  // try to connect and obtain information from storage
  Result:=Internal_Underlaying;

  // if ok
  if (ETSR_OK=Result) then
    Exit;

  // problems: a) allow correct by demand and b) cannot correct
  if (ETSR_UNDERLAYING_UNAVAILABLE=Result) then begin
    // check storage address
    Exit;
  end;

  if (ETSR_UNDERLAYING_UNATHORIZED=Result) then begin
    // check authentication information
    Exit;
  end;

  if (ETSR_UNDERLAYING_RESTRICTED=Result) then begin
    // just restricted by any other reasons
    // cannot corrent
    Exit;
  end;

  if (ETSR_UNDERLAYING_WITHOUT_STRUCTURE=Result) then begin
    // ask to create new structure for underlaying storage
    Exit;
  end;

  if (ETSR_UNDERLAYING_INVALID_STRUCTURE=Result) then begin
    // storage has invalid structure
    // cannot corrent
    Exit;
  end;

  if (ETSR_UNDERLAYING_CRITICAL_ERROR=Result) then begin
    // any other critical errors
    // cannot corrent
    Exit;
  end;

  // any other errors?
end;

function TETS_Host_Link.Internal_Initialize_ETS: LongInt;
begin
  if (nil=FHostProvider) then begin
    Result:=ETSR_NO_ROOT_FUNCTION;
    Exit;
  end;

  // cleanup (only if supports reinitialization)
  //Internal_Uninitialize_ETS;
  //Internal_Initialize_Zero;
  
  // open link
  Result:=FHostProvider.FETS_PQI_PROV_FUNC.p_Link_Open(Pointer(Self), FHostProvider.FProviderHandle, @FLinkHandle);
  if (ETSR_OK<>Result) then
    Exit;

  // set auth and others (callbacks)
  if Assigned(FHostProvider.FETS_PQI_LINK_FUNC.p_Link_Set_Info) then begin
    // set auth pointer to self object
    FHostProvider.FETS_PQI_LINK_FUNC.p_Link_Set_Info(FLinkHandle, ETS_LSIC_CALLBACK_POINTER, 0, Pointer(Self));
    // set auth function - get auth info
    FHostProvider.FETS_PQI_LINK_FUNC.p_Link_Set_Info(FLinkHandle, ETS_LSIC_AUTH_FUNC, 0, @r_ETS_Link_Auth_Get);
    // set auth function - free auth info
    FHostProvider.FETS_PQI_LINK_FUNC.p_Link_Set_Info(FLinkHandle, ETS_LSIC_AUTH_FREE, 0, @r_ETS_Link_Auth_Free);
    // loose connection
    FHostProvider.FETS_PQI_LINK_FUNC.p_Link_Set_Info(FLinkHandle, ETS_LSIC_LOST_CONNECT, 0, @r_ETS_Link_Connection_Lost);
    // restore connection
    FHostProvider.FETS_PQI_LINK_FUNC.p_Link_Set_Info(FLinkHandle, ETS_LSIC_RESTORE_CONNECT, 0, @r_ETS_Link_Connection_Rest);
  end;

  // get tile routines
  if Assigned(FHostProvider.FETS_PQI_LINK_FUNC.p_Link_Query_Info) then begin
    // unicode version
    Result:=FHostProvider.FETS_PQI_LINK_FUNC.p_Link_Query_Info(FLinkHandle, ETS_LQIC_TILE_ROUTINES_W, sizeof(FETS_LQI_TILE_ROUTINES_W), @FETS_LQI_TILE_ROUTINES_W);
    if (ETSR_OK<>Result) then begin
      // try ansi version
      Result:=FHostProvider.FETS_PQI_LINK_FUNC.p_Link_Query_Info(FLinkHandle, ETS_LQIC_TILE_ROUTINES_A, sizeof(FETS_LQI_TILE_ROUTINES_A), @FETS_LQI_TILE_ROUTINES_A);
      if (ETSR_OK<>Result) then
        Exit;
    end;
  end;

  FInitialized:=TRUE;
end;

procedure TETS_Host_Link.Internal_Initialize_Zero;
begin
  FLinkHandle:=nil;
  ZeroMemory(@FETS_LQI_TILE_ROUTINES_W, sizeof(FETS_LQI_TILE_ROUTINES_W));
  ZeroMemory(@FETS_LQI_TILE_ROUTINES_A, sizeof(FETS_LQI_TILE_ROUTINES_A));
  FInitialized:=FALSE;
  FConnected:=FALSE;
  FUnderlaying:=FALSE;
  FUnderlayingTileIdFormat:=0;
  FConnLostStatus:=ets_cls_None;
  FServiceName:='';
  FConnectionInfo:='';
  FContentType:='';
  FDefaultExt:='';
  FContentTypeOptions:=0;
  FContentTypeBasic:=nil;
end;

function TETS_Host_Link.Internal_Underlaying: LongInt;
var
  Vas: AnsiString;
  VctW: TETS_LQI_CONTENT_TYPE_W;
begin
  FSynchronizer.BeginWrite;
  try
    // no storage
    if (nil=FHostProvider) then begin
      Result:=ETSR_NO_ROOT_FUNCTION;
      Exit;
    end;

    // not initialized
    if (not FInitialized) then begin
      Result:=ETSR_NOT_INITIALIZED;
      Exit;
    end;

    // not connected
    if (not FConnected) then begin
      Result:=ETSR_NOT_CONNECTED;
      Exit;
    end;

    // query information about underlaying storage
    if Assigned(FHostProvider.FETS_PQI_LINK_FUNC.p_Link_Query_Info) then begin
      // unicode version
      ZeroMemory(@VctW, sizeof(VctW));
      Result:=FHostProvider.FETS_PQI_LINK_FUNC.p_Link_Query_Info(FLinkHandle, ETS_LQIC_CONTENT_TYPE_W, sizeof(VctW), @VctW);
      if (ETSR_OK<>Result) then begin
        // check critical errors
        if Internal_Critical_Underlaying_Errors(Result) then
          Exit;
        // try ansi version (into same struct)
        ZeroMemory(@VctW, sizeof(VctW));
        Result:=FHostProvider.FETS_PQI_LINK_FUNC.p_Link_Query_Info(FLinkHandle, ETS_LQIC_CONTENT_TYPE_A, sizeof(VctW), @VctW);
        if (ETSR_OK<>Result) then
          Exit
        else begin
          // ok - ansi version
          Vas:=TETS_LQI_CONTENT_TYPE_A(VctW).szContentType;
          FContentType:=Vas;
          Vas:=TETS_LQI_CONTENT_TYPE_A(VctW).szDefaultExt;
          FDefaultExt:=Vas;
          FContentTypeOptions:=VctW.dwOptions;
          FContentTypeBasic:=TContentTypeInfoBase.Create(FContentType,FDefaultExt);
        end;
      end else begin
        // ok - unicode version
        FContentType:=VctW.szContentType;
        FDefaultExt:=VctW.szDefaultExt;
        FContentTypeOptions:=VctW.dwOptions;
        FContentTypeBasic:=TContentTypeInfoBase.Create(FContentType,FDefaultExt);
      end;
    end;

    // get tile_id format, that really used by current underlaying storage
    if Assigned(FHostProvider.FETS_PQI_LINK_FUNC.p_Link_Query_Info) then begin
      FUnderlayingTileIdFormat:=0;
      Result:=FHostProvider.FETS_PQI_LINK_FUNC.p_Link_Query_Info(FLinkHandle, ETS_LQIC_TILE_ID_FORMAT, sizeof(FUnderlayingTileIdFormat), @FUnderlayingTileIdFormat);
      if (ETSR_OK<>Result) then
        Exit;
    end;

    if (FConnLostStatus<>ets_cls_None) then begin
      // reset lost connection flag
      FConnLostStatus:=ets_cls_None;
    end;

    // done
    FUnderlaying:=TRUE;
    Result:=ETSR_OK;
  finally
    FSynchronizer.EndWrite;
  end;
end;

function TETS_Host_Link.Internal_Uninitialize_ETS: LongInt;
begin
  FUnderlaying:=FALSE;
  FConnected:=FALSE;
  FInitialized:=FALSE;
  Result:=ETSR_NO_ROOT_FUNCTION;
  if (nil<>FLinkHandle) then begin
    if (nil<>FHostProvider) and Assigned(FHostProvider.FETS_PQI_PROV_FUNC.p_Link_Close) then
      Result:=FHostProvider.FETS_PQI_PROV_FUNC.p_Link_Close(FLinkHandle);
    FLinkHandle:=nil;
  end;
end;

function TETS_Host_Link.NotifyConnLost(const APtrConnSize: LongWord;
                                       const APtrConnData: PETS_LINK_RECONNECT): LongInt;
begin
  FUnderlaying:=FALSE;
  // default value
  FConnLostStatus:=ets_cls_OnSyncOnly;
  // if storage provider can reconnect - do it
  if (nil<>APtrConnData) then begin
    // TODO: set autoreconnect internally by storage provider
  end;
  Result:=ETSR_OK;
end;

function TETS_Host_Link.NotifyConnRest(const APtrConnSize: LongWord;
                                       const APtrConnData: PETS_LINK_RECONNECT): LongInt;
begin
  Internal_Underlaying;
  FConnLostStatus:=ets_cls_None;
  // if from provider - APtrConnData<>nil
  // if from connect by EXE - APtrConnData=nil
  Result:=ETSR_OK;
end;

function TETS_Host_Link.Query_Tile(const AXYZ: PTILE_ID_XYZ;
                                   const AVersionInfo: IMapVersionInfo;
                                   out ATileInfo: ITileInfoBasic): LongInt;
var
  VVersionW: WideString;
  VVersionA: AnsiString;
  VTileBuf: TETS_TILE_BUFFER;
  VVersionBufW: TETS_TILE_VERSION_W;

  procedure _MakeTileInfo(const AAnsiVersion: Boolean);
  begin
    ATileInfo:=Internal_Create_TileInfo(FALSE, AVersionInfo, @VTileBuf, @VVersionBufW);
  end;
begin
  ATileInfo:=nil;

  if (not FConnected) then begin
    Result:=ETSR_NOT_CONNECTED;
    Exit;
  end;

  if (not FUnderlaying) then begin
    Result:=Internal_Force_Underlaying;
    if (ETSR_OK<>Result) then
      Exit;
  end;

  FSynchronizer.BeginRead;
  try
    // init
    ZeroMemory(@VVersionBufW, sizeof(VVersionBufW));
    ZeroMemory(@VTileBuf, sizeof(VTileBuf));
    // use default options
    VTileBuf.dwOptionsIn:=(FQueryTileOptions or ETS_TBI_QUERY_INFO);

    if Assigned(FETS_LQI_TILE_ROUTINES_W.p_Tile_Query_W) then begin
      // use unicode version
      VVersionW:=AVersionInfo.StoreString;
      Result:=FETS_LQI_TILE_ROUTINES_W.p_Tile_Query_W(FLinkHandle, nil, AXYZ, PWideChar(VVersionW), @VTileBuf, @VVersionBufW);
      try
        if (ETSR_OK=Result) then begin
          _MakeTileInfo(FALSE);
        end;
      finally
        // free created items in buffer
        FETS_LQI_TILE_ROUTINES_W.p_Tile_Free(FLinkHandle, @VTileBuf);
        FETS_LQI_TILE_ROUTINES_W.p_Ver_Free_W(FLinkHandle, @VVersionBufW);
      end;
    end else if Assigned(FETS_LQI_TILE_ROUTINES_A.p_Tile_Query_A) then begin
      // use ansi version
      VVersionA:=AVersionInfo.StoreString;
      Result:=FETS_LQI_TILE_ROUTINES_A.p_Tile_Query_A(FLinkHandle, nil, AXYZ, PAnsiChar(VVersionA), @VTileBuf, @VVersionBufW);
      try
        if (ETSR_OK=Result) then begin
          _MakeTileInfo(TRUE);
        end;
      finally
        // free created items in buffer
        FETS_LQI_TILE_ROUTINES_A.p_Tile_Free(FLinkHandle, @VTileBuf);
        FETS_LQI_TILE_ROUTINES_A.p_Ver_Free_A(FLinkHandle, @VVersionBufW);
      end;
    end else begin
      // not supported
      Result:=ETSR_NOT_SUPPORTED;
    end;
  finally
    FSynchronizer.EndRead;
  end;
end;

function TETS_Host_Link.Select_Tile(const AXYZ: PTILE_ID_XYZ;
                                    const AVersionInfo: IMapVersionInfo;
                                    out ATileInfo: ITileInfoBasic;
                                    const AStream: TStream): LongInt;
var
  VVersionW: WideString;
  VVersionA: AnsiString;
  VTileBuf: TETS_TILE_BUFFER;
  VVersionBufW: TETS_TILE_VERSION_W;

  procedure _MakeTileInfo(const AAnsiVersion: Boolean);
  begin
    ATileInfo:=Internal_Create_TileInfo(FALSE, AVersionInfo, @VTileBuf, @VVersionBufW);
    // read tile from buffer to stream
    AStream.Size:=0;
  end;
begin
  ATileInfo:=nil;

  if (not FConnected) then begin
    Result:=ETSR_NOT_CONNECTED;
    Exit;
  end;

  if (nil=AStream) then begin
    Result:=ETSR_NO_MANDATORY_PARAMETER;
    Exit;
  end;

  if (not FUnderlaying) then begin
    Result:=Internal_Force_Underlaying;
    if (ETSR_OK<>Result) then
      Exit;
  end;

  FSynchronizer.BeginRead;
  try
    // init
    ZeroMemory(@VVersionBufW, sizeof(VVersionBufW));
    ZeroMemory(@VTileBuf, sizeof(VTileBuf));
    // use default options
    VTileBuf.dwOptionsIn:=FQueryTileOptions;

    if Assigned(FETS_LQI_TILE_ROUTINES_W.p_Tile_Select_W) then begin
      // use unicode version
      VVersionW:=AVersionInfo.StoreString;
      Result:=FETS_LQI_TILE_ROUTINES_W.p_Tile_Select_W(FLinkHandle, nil, AXYZ, PWideChar(VVersionW), @VTileBuf, @VVersionBufW);
      try
        if (ETSR_OK=Result) then begin
          _MakeTileInfo(FALSE);
        end;
      finally
        // free created items in buffer
        FETS_LQI_TILE_ROUTINES_W.p_Tile_Free(FLinkHandle, @VTileBuf);
        FETS_LQI_TILE_ROUTINES_W.p_Ver_Free_W(FLinkHandle, @VVersionBufW);
      end;
    end else if Assigned(FETS_LQI_TILE_ROUTINES_A.p_Tile_Select_A) then begin
      // use ansi version
      VVersionA:=AVersionInfo.StoreString;
      Result:=FETS_LQI_TILE_ROUTINES_A.p_Tile_Select_A(FLinkHandle, nil, AXYZ, PAnsiChar(VVersionA), @VTileBuf, @VVersionBufW);
      try
        if (ETSR_OK=Result) then begin
          _MakeTileInfo(TRUE);
        end;
      finally
        // free created items in buffer
        FETS_LQI_TILE_ROUTINES_A.p_Tile_Free(FLinkHandle, @VTileBuf);
        FETS_LQI_TILE_ROUTINES_A.p_Ver_Free_A(FLinkHandle, @VVersionBufW);
      end;
    end else begin
      // not supported
      Result:=ETSR_NOT_SUPPORTED;
    end;
  finally
    FSynchronizer.EndRead;
  end;
end;

function TETS_Host_Link.Sync(Sender: TObject): LongInt;
begin
  FSynchronizer.BeginWrite;
  try
    // catch sync event
    if (nil<>FHostProvider) and Assigned(FHostProvider.FETS_PQI_LINK_FUNC.p_Link_Sync) then
      Result:=FHostProvider.FETS_PQI_LINK_FUNC.p_Link_Sync(FLinkHandle, nil)
    else
      Result:=ETSR_NOT_SUPPORTED;

    // TODO: try to reconnect (on lost connection)
  finally
    FSynchronizer.EndWrite;
  end;
end;

end.