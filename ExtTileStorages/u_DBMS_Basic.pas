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

unit u_DBMS_Basic;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils,
  t_ETS_Result,
  t_ETS_Tiles,
  t_ETS_List,
  t_ETS_Provider;

type
  TDBMS_Environment_Basic = class(TObject)
  
  end;

  TDBMS_Environment_Basic_Class = class of TDBMS_Environment_Basic;

  TDBMS_Connection_Basic = class(TObject)

  end;

  TDBMS_Connection_Basic_Class = class of TDBMS_Connection_Basic;

  TDBMS_Provider_Basic = class;

  TDBMS_Link_Basic = class(TObject)
  protected
    FProvider: TDBMS_Provider_Basic;
    FConnection: TDBMS_Connection_Basic;
    FSynchronizer: TMultiReadExclusiveWriteSynchronizer;
    FHostLinkPtr: Pointer;
    FCallbackPointer: Pointer;
    FServiceName: String;
    FConnectionInfo: String;
    FUnderlayingContentType: WideString;
    FUnderlayingDefaultExt: WideString;
    FUnderlayingOptions: LongWord;
    FUnderlayingTileIdFormat: LongWord;
    FAuth_Get: TETS_Link_Auth_Func;
    FAuth_Free: TETS_Link_Auth_Func;
    FLostConn: TETS_Link_Reconn_Func;
    FRestConn: TETS_Link_Reconn_Func;
    // all params defined and can connect to underlaying storage
    FCanConnectToUnderlaying: Boolean;
    // connected to underlaying storage
    FConnectedToUnderlaying: Boolean;
    // cannot work because of destruction
    FTerminating: Boolean;
    // result if cannot connect to underlaying storage
    FCannotConnectResult: LongInt;
    // result of checking underlaying storage structure
    FUnderlayingStructureResult: LongInt;
  protected
    function Internal_Initialize_Link: LongInt;
    function Internal_Uninitialize_Link: LongInt;
    function Internal_Save_Connect_or_Exit(var AResult: LongInt): Boolean;
    function Internal_Create_Environment_Object(var AResult: LongInt): TDBMS_Environment_Basic;
    function Internal_Create_Connection_Object(var AResult: LongInt): TDBMS_Connection_Basic;
  protected
    // overridable threadsafe routines
    procedure Internal_Underlaying_Disconnect; virtual;
    function Internal_Underlaying_Connect: LongInt; virtual;
    function Internal_Begin_Tran: LongInt; virtual;
    function Internal_Commit_Tran: LongInt; virtual;
    function Internal_Rollback_Tran: LongInt; virtual;
  public
    constructor Create(const AHostLinkPtr: Pointer); virtual;
    destructor Destroy; override;

    function Flush_Obj: LongInt;

    function Sync_Obj(const ASyncPointer: Pointer): LongInt;

    function Begin_Tran: LongInt;
    function Commit_Tran: LongInt;
    function Rollback_Tran: LongInt;
    function Tran_Count(const APtrValue: PLongWord): LongInt;

    function Query_Info(const ALinkQueryInfoClass: LongWord;
                        const ALinkQueryInfoSize: LongWord;
                        const ALinkQueryInfoData: Pointer): LongInt;

    function Set_Info(const ALinkSetInfoClass: LongWord;
                      const ALinkSetInfoSize: LongWord;
                      const ALinkSetInfoData: Pointer): LongInt;

    // tile routines
    function Tne_Create_A(const AServiceName: PAnsiChar;
                          const APtrTileID: PTILE_ID_XYZ;
                          const APtrVersionA: PETS_TILE_VERSION_A): LongInt;

    function Tile_Insert_A(const AServiceName: PAnsiChar;
                           const APtrTileID: PTILE_ID_XYZ;
                           const APtrTileInfo: PETS_TILE_BUFFER;
                           const APtrVersionA: PETS_TILE_VERSION_A): LongInt;

    function Tile_Delete_A(const AServiceName: PAnsiChar;
                           const APtrTileID: PTILE_ID_XYZ;
                           const AVersion: PAnsiChar;
                           const AOptions: LongWord): LongInt;
                           
    function Tile_Query_A(const AServiceName: PAnsiChar;
                          const APtrTileID: PTILE_ID_XYZ;
                          const AVersion: PAnsiChar;
                          const APtrTileInfo: PETS_TILE_BUFFER;
                          const APtrVersionA: PETS_TILE_VERSION_A): LongInt;
                           
    function Tile_Select_A(const AServiceName: PAnsiChar;
                           const APtrTileID: PTILE_ID_XYZ;
                           const AVersion: PAnsiChar;
                           const APtrTileInfo: PETS_TILE_BUFFER;
                           const APtrVersionA: PETS_TILE_VERSION_A): LongInt;
  end;

  TDBMS_Link_Basic_Class = class of TDBMS_Link_Basic;

  TDBMS_Provider_Basic = class(TObject)
  protected
    FHostProvPtr: Pointer;
    FEnvironment: TDBMS_Environment_Basic;
    FConnection: TDBMS_Connection_Basic;
    FLinks: T_ETS_ObjectList;
    FInternalProviderName: WideString;
    FGlobalStorageId: String;
    FTileIdConverter: TETS_TileId_Conversion_Routine;
    FCSLink: TRTLCriticalSection;
    FDBMS_Environment_Basic_Class: TDBMS_Environment_Basic_Class;
    FDBMS_Connection_Basic_Class: TDBMS_Connection_Basic_Class;
    FDBMS_Link_Basic_Class: TDBMS_Link_Basic_Class;
    FGlobalInitialized: Boolean;
    FMultipleConnections: Boolean;
  protected
    function Internal_Initialize_Prov: LongInt;
    function Internal_Uninitialize_Prov: LongInt;
    // del link object
    procedure InternalDelLinkFromList(const ALinkObj: TDBMS_Link_Basic);
    // link cs
    procedure EnterLinkCS;
    procedure LeaveLinkCS;
  public
    constructor Create(const AHostProvPtr: Pointer;
                       const AProvOptions: LongWord);
    destructor Destroy; override;
    
    function Query_Info(const AProvQueryInfoClass: LongWord;
                        const AProvQueryInfoSize: LongWord;
                        const AProvQueryInfoData: Pointer): LongInt;

    function Set_Info(const AProvSetInfoClass: LongWord;
                      const AProvSetInfoSize: LongWord;
                      const AProvSetInfoData: Pointer): LongInt;

    function Create_Link(const AHostLinkPtr: Pointer): TDBMS_Link_Basic;

    property DBMS_Environment_Basic_Class: TDBMS_Environment_Basic_Class read FDBMS_Environment_Basic_Class write FDBMS_Environment_Basic_Class;
    property DBMS_Connection_Basic_Class: TDBMS_Connection_Basic_Class read FDBMS_Connection_Basic_Class write FDBMS_Connection_Basic_Class;
    property DBMS_Link_Basic_Class: TDBMS_Link_Basic_Class read FDBMS_Link_Basic_Class write FDBMS_Link_Basic_Class;

    property InternalProviderName: WideString read FInternalProviderName write FInternalProviderName;
  end;

implementation

function r_Tile_Select_A(const ALinkHandle: TETS_Link_Handle;
                         const AServiceName: PAnsiChar;
                         const APtrTileID: PTILE_ID_XYZ;
                         const AVersion: PAnsiChar;
                         const APtrTileInfo: PETS_TILE_BUFFER;
                         const APtrVersionA: PETS_TILE_VERSION_A): LongInt; stdcall;
begin
  try
    if (nil=ALinkHandle) then begin
      // no pointer
      Result:=ETSR_INVALID_OBJECT_POINTER
    end else begin
      // just call object
      Result:=TDBMS_Link_Basic(ALinkHandle).Tile_Select_A(AServiceName,APtrTileID,AVersion,APtrTileInfo,APtrVersionA);
    end;
  except
    Result:=ETSR_STORAGE_EXCEPTION;
  end;
end;

function r_Tile_Query_A(const ALinkHandle: TETS_Link_Handle;
                        const AServiceName: PAnsiChar;
                        const APtrTileID: PTILE_ID_XYZ;
                        const AVersion: PAnsiChar;
                        const APtrTileInfo: PETS_TILE_BUFFER;
                        const APtrVersionA: PETS_TILE_VERSION_A): LongInt; stdcall;
begin
  try
    if (nil=ALinkHandle) then begin
      // no pointer
      Result:=ETSR_INVALID_OBJECT_POINTER
    end else begin
      // just call object
      Result:=TDBMS_Link_Basic(ALinkHandle).Tile_Query_A(AServiceName,APtrTileID,AVersion,APtrTileInfo,APtrVersionA);
    end;
  except
    Result:=ETSR_STORAGE_EXCEPTION;
  end;
end;

function r_Tile_Delete_A(const ALinkHandle: TETS_Link_Handle;
                         const AServiceName: PAnsiChar;
                         const APtrTileID: PTILE_ID_XYZ;
                         const AVersion: PAnsiChar;
                         const AOptions: LongWord): LongInt; stdcall;
begin
  try
    if (nil=ALinkHandle) then begin
      // no pointer
      Result:=ETSR_INVALID_OBJECT_POINTER
    end else begin
      // just call object
      Result:=TDBMS_Link_Basic(ALinkHandle).Tile_Delete_A(AServiceName,APtrTileID,AVersion,AOptions);
    end;
  except
    Result:=ETSR_STORAGE_EXCEPTION;
  end;
end;

function r_Tile_Insert_A(const ALinkHandle: TETS_Link_Handle;
                         const AServiceName: PAnsiChar;
                         const APtrTileID: PTILE_ID_XYZ;
                         const APtrTileInfo: PETS_TILE_BUFFER;
                         const APtrVersionA: PETS_TILE_VERSION_A): LongInt; stdcall;
begin
  try
    if (nil=ALinkHandle) then begin
      // no pointer
      Result:=ETSR_INVALID_OBJECT_POINTER
    end else begin
      // just call object
      Result:=TDBMS_Link_Basic(ALinkHandle).Tile_Insert_A(AServiceName,APtrTileID,APtrTileInfo,APtrVersionA);
    end;
  except
    Result:=ETSR_STORAGE_EXCEPTION;
  end;
end;

function r_Tne_Create_A(const ALinkHandle: TETS_Link_Handle;
                        const AServiceName: PAnsiChar;
                        const APtrTileID: PTILE_ID_XYZ;
                        const APtrVersionA: PETS_TILE_VERSION_A): LongInt; stdcall;
begin
  try
    if (nil=ALinkHandle) then begin
      // no pointer
      Result:=ETSR_INVALID_OBJECT_POINTER
    end else begin
      // just call object
      Result:=TDBMS_Link_Basic(ALinkHandle).Tne_Create_A(AServiceName,APtrTileID,APtrVersionA);
    end;
  except
    Result:=ETSR_STORAGE_EXCEPTION;
  end;
end;

function r_Ver_Free_A(const ALinkHandle: TETS_Link_Handle;
                      const APtrVersionA: PETS_TILE_VERSION_A): LongInt; stdcall;
begin
  try
    if (nil=ALinkHandle) or (nil=APtrVersionA) then begin
      // no pointer
      Result:=ETSR_INVALID_OBJECT_POINTER
    end else begin
      // free existing buffers
      if (nil<>APtrVersionA^.szVersion) then begin
        FreeMemory(APtrVersionA^.szVersion);
        APtrVersionA^.szVersion:=nil;
      end;
      if (nil<>APtrVersionA^.szContentType) then begin
        FreeMemory(APtrVersionA^.szContentType);
        APtrVersionA^.szContentType:=nil;
      end;
      Result:=ETSR_OK;
    end;
  except
    Result:=ETSR_STORAGE_EXCEPTION;
  end;
end;

function r_Tile_Free(const ALinkHandle: TETS_Link_Handle;
                     const APtrTileInfo: PETS_TILE_BUFFER): LongInt; stdcall;
begin
  try
    if (nil=ALinkHandle) or (nil=APtrTileInfo) then begin
      // no pointer
      Result:=ETSR_INVALID_OBJECT_POINTER
    end else begin
      // free existing buffers
      if (nil<>APtrTileInfo^.ptrTileBuffer) then begin
        FreeMemory(APtrTileInfo^.ptrTileBuffer);
        APtrTileInfo^.ptrTileBuffer:=nil;
      end;
      Result:=ETSR_OK;
    end;
  except
    Result:=ETSR_STORAGE_EXCEPTION;
  end;
end;

{ TDBMS_Provider_Basic }

constructor TDBMS_Provider_Basic.Create(const AHostProvPtr: Pointer;
                                        const AProvOptions: LongWord);
begin
  FHostProvPtr:=AHostProvPtr;
  FGlobalInitialized:=FALSE;
  FMultipleConnections:=FALSE; // TODO: get it from AProvOptions
  FGlobalStorageId:='';
  FEnvironment:=nil;
  FConnection:=nil;
  FTileIdConverter:=nil;
  FInternalProviderName:='';
  FLinks.SetZero;
  InitializeCriticalSection(FCSLink);
  FDBMS_Environment_Basic_Class:=TDBMS_Environment_Basic;
  FDBMS_Connection_Basic_Class:=TDBMS_Connection_Basic;
  FDBMS_Link_Basic_Class:=TDBMS_Link_Basic;
end;

function TDBMS_Provider_Basic.Create_Link(const AHostLinkPtr: Pointer): TDBMS_Link_Basic;
begin
  Result:=nil;
  if (not FGlobalInitialized) then
    Exit;
  
  EnterLinkCS;
  try
    if Assigned(FDBMS_Link_Basic_Class) then begin
      Result:=FDBMS_Link_Basic_Class.Create(AHostLinkPtr);
      Result.FProvider:=Self;
      FLinks.AddItem(Result);
      Result.Internal_Initialize_Link;
    end;
  finally
    LeaveLinkCS;
  end;
end;

destructor TDBMS_Provider_Basic.Destroy;
begin
  Internal_Uninitialize_Prov;
  DeleteCriticalSection(FCSLink);
  FreeAndNil(FConnection);
  FreeAndNil(FEnvironment);
  inherited;
end;

procedure TDBMS_Provider_Basic.EnterLinkCS;
begin
  EnterCriticalSection(FCSLink);
end;

procedure TDBMS_Provider_Basic.InternalDelLinkFromList(const ALinkObj: TDBMS_Link_Basic);
begin
  EnterLinkCS;
  try
    FLinks.DelItem(ALinkObj);
  finally
    LeaveLinkCS;
  end;
end;

function TDBMS_Provider_Basic.Internal_Initialize_Prov: LongInt;
begin
  // TODO: connect (if all links in single connection)

  FGlobalInitialized:=TRUE;
  Result:=ETSR_OK;
end;

function TDBMS_Provider_Basic.Internal_Uninitialize_Prov: LongInt;
begin
  FGlobalInitialized:=FALSE;
  EnterLinkCS;
  try
    // Free all links
    FLinks.FreeALL;

    // TODO: disconnect (if all links in single connection)

    Result:=ETSR_OK;
  finally
    LeaveLinkCS;
  end;
end;

procedure TDBMS_Provider_Basic.LeaveLinkCS;
begin
  LeaveCriticalSection(FCSLink);
end;

function TDBMS_Provider_Basic.Query_Info(const AProvQueryInfoClass: LongWord;
                                         const AProvQueryInfoSize: LongWord;
                                         const AProvQueryInfoData: Pointer): LongInt;
begin
  EnterLinkCS;
  try
    if (nil=Self) then begin
      Result:=ETSR_INVALID_OBJECT_POINTER;
    end else if (ETS_PQIC_NAME_W=AProvQueryInfoClass) then begin
      // get provider internal name (unicode)
      if (AProvQueryInfoSize>=sizeof(TETS_PQI_NAME_W)) then begin
        PETS_PQI_NAME_W(AProvQueryInfoData)^.szName:=PWideChar(FInternalProviderName);
        Result:=ETSR_OK;
      end else begin
        // invalid buffer
        Result:=ETSR_SIZE_MISMATCH;
      end;
    end else if (ETS_PQIC_NAME_A=AProvQueryInfoClass) then begin
      // get provider internal name (ansi)
      Result:=ETSR_NOT_SUPPORTED; // use only unicode version
    end else begin
      // unknown calls
      Result:=ETSR_NOT_SUPPORTED;
    end;
  finally
    LeaveLinkCS;
  end;
end;

function TDBMS_Provider_Basic.Set_Info(const AProvSetInfoClass: LongWord;
                                       const AProvSetInfoSize: LongWord;
                                       const AProvSetInfoData: Pointer): LongInt;
var
  Vws: WideString;
  Vas: AnsiString;
begin
  EnterLinkCS;
  try
    if (nil=Self) then begin
      Result:=ETSR_INVALID_OBJECT_POINTER;
    end else if (0<>AProvSetInfoSize) then begin
      // no structures here - use direct pointers and size=0
      Result:=ETSR_SIZE_MISMATCH;
    end else if (ETS_PSIC_GLOBALID_W=AProvSetInfoClass) then begin
      // set unicode global storage id
      Vws:=PWideChar(AProvSetInfoData);
      FGlobalStorageId:=Vws;
      Result:=Internal_Initialize_Prov;
    end else if (ETS_PSIC_GLOBALID_A=AProvSetInfoClass) then begin
      // set ansi global storage id
      Vas:=PAnsiChar(AProvSetInfoData);
      FGlobalStorageId:=Vas;
      Result:=Internal_Initialize_Prov;
    end else if (ETS_PSIC_CONVERSION=AProvSetInfoClass) then begin
      // set tile_id conversion routine
      FTileIdConverter:=AProvSetInfoData;
      Result:=ETSR_OK;
    end else if (ETS_PSIC_PROTO_VER=AProvSetInfoClass) then begin
      // set protocol version (used by host)
      Result:=ETSR_NOT_SUPPORTED;
    end else if (ETS_PSIC_EXCEPT_HANDLER=AProvSetInfoClass) then begin
      // set exception handler
      Result:=ETSR_NOT_SUPPORTED;
    end else if (ETS_PSIC_PROGRESS_FUNC=AProvSetInfoClass) then begin
      // set progress handler
      Result:=ETSR_NOT_SUPPORTED;
    end else begin
      // unknown
      Result:=ETSR_NOT_SUPPORTED;
    end;
  finally
    LeaveLinkCS;
  end;
end;

{ TDBMS_Link_Basic }

function TDBMS_Link_Basic.Begin_Tran: LongInt;
begin
  FSynchronizer.BeginWrite;
  try
    Result:=Internal_Begin_Tran;
  finally
    FSynchronizer.EndWrite;
  end;
end;

function TDBMS_Link_Basic.Commit_Tran: LongInt;
begin
  FSynchronizer.BeginWrite;
  try
    Result:=Internal_Commit_Tran;
  finally
    FSynchronizer.EndWrite;
  end;
end;

constructor TDBMS_Link_Basic.Create(const AHostLinkPtr: Pointer);
begin
  FSynchronizer := TMultiReadExclusiveWriteSynchronizer.Create;
  FCanConnectToUnderlaying:=FALSE;
  FConnectedToUnderlaying:=FALSE;
  FTerminating:=FALSE;
  FProvider:=nil;
  FConnection:=nil;
  FCallbackPointer:=nil;
  FAuth_Get:=nil;
  FAuth_Free:=nil;
  FLostConn:=nil;
  FRestConn:=nil;
  FServiceName:='';
  FConnectionInfo:='';
  FUnderlayingContentType:='';
  FUnderlayingDefaultExt:='';
  FUnderlayingOptions:=0;
  FUnderlayingTileIdFormat:=0;
  FUnderlayingStructureResult:=ETSR_NOT_CONNECTED;
  FCannotConnectResult:=ETSR_NOT_CONNECTED;
  FHostLinkPtr:=AHostLinkPtr;
end;

destructor TDBMS_Link_Basic.Destroy;
begin
  FTerminating:=TRUE;
  Internal_Uninitialize_Link;
  FreeAndNil(FSynchronizer);
  FreeAndNil(FConnection);
  inherited;
end;

function TDBMS_Link_Basic.Flush_Obj: LongInt;
begin
  FSynchronizer.BeginWrite;
  try
    Result:=ETSR_NOT_SUPPORTED;
  finally
    FSynchronizer.EndWrite;
  end;
end;

function TDBMS_Link_Basic.Internal_Begin_Tran: LongInt;
begin
  Result:=ETSR_NOT_SUPPORTED;
end;

function TDBMS_Link_Basic.Internal_Commit_Tran: LongInt;
begin
  Result:=ETSR_NOT_SUPPORTED;
end;

function TDBMS_Link_Basic.Internal_Create_Connection_Object(var AResult: Integer): TDBMS_Connection_Basic;
begin
  Result:=TDBMS_Connection_Basic.Create;
  AResult:=ETSR_OK;
end;

function TDBMS_Link_Basic.Internal_Create_Environment_Object(var AResult: Integer): TDBMS_Environment_Basic;
begin
  Result:=TDBMS_Environment_Basic.Create;
  AResult:=ETSR_OK;
end;

function TDBMS_Link_Basic.Internal_Initialize_Link: LongInt;
begin
  if (nil=FProvider) then begin
    Result:=ETSR_INVALID_OBJECT_POINTER;
    Exit;
  end;

  if (nil=FProvider.FEnvironment) then begin
    FProvider.FEnvironment:=Internal_Create_Environment_Object(Result);
    if (Result<>ETSR_OK) then
      Exit;
    if (nil=FProvider.FEnvironment) then begin
      Result:=ETSR_ERROR_CREATING_ENVIRONMENT;
      Exit;
    end;
  end;

  if (FProvider.FMultipleConnections) then begin
    // create connection object on link basis
    Self.FConnection:=Internal_Create_Connection_Object(Result);
    if (Result<>ETSR_OK) then
      Exit;
    if (nil=Self.FConnection) then begin
      Result:=ETSR_ERROR_CREATING_CONNECTION;
      Exit;
    end;
  end else if (nil=FProvider.FConnection) then begin
    // create single connection on provider basis
    // provider object is completely initialized - allow to create connection
    FProvider.FConnection:=Internal_Create_Connection_Object(Result);
    if (Result<>ETSR_OK) then
      Exit;
    if (nil=FProvider.FConnection) then begin
      Result:=ETSR_ERROR_CREATING_CONNECTION;
      Exit;
    end;
  end else begin
    // already created on provider basis
    Result:=ETSR_OK;
  end;
end;

function TDBMS_Link_Basic.Internal_Rollback_Tran: LongInt;
begin
  Result:=ETSR_NOT_SUPPORTED;
end;

function TDBMS_Link_Basic.Internal_Save_Connect_or_Exit(var AResult: Integer): Boolean;
begin
  Result:=FALSE;

  // cannot connect
  if FTerminating or (not FCanConnectToUnderlaying) then begin
    AResult:=FCannotConnectResult;
    Result:=TRUE;
    Exit;
  end;

  // can connect and not connected - try to connect
  if (FCanConnectToUnderlaying) and (not FConnectedToUnderlaying) then begin
    FSynchronizer.BeginWrite;
    try
      AResult:=Internal_Underlaying_Connect;
      if (AResult<>ETSR_OK) then begin
        Result:=TRUE;
        Exit;
      end;
    finally
      FSynchronizer.EndWrite;
    end;
  end;
end;

function TDBMS_Link_Basic.Internal_Underlaying_Connect: LongInt;
begin
  Result:=ETSR_NOT_CONNECTED;
end;

procedure TDBMS_Link_Basic.Internal_Underlaying_Disconnect;
begin
  // override
end;

function TDBMS_Link_Basic.Internal_Uninitialize_Link: LongInt;
begin
  FCanConnectToUnderlaying:=FALSE;
  FSynchronizer.BeginWrite;
  try
    // disconnect from underlaying storage
    if FConnectedToUnderlaying then begin
      FCannotConnectResult:=ETSR_NOT_CONNECTED;
      FUnderlayingStructureResult:=ETSR_NOT_CONNECTED;
      Internal_Underlaying_Disconnect;
      FConnectedToUnderlaying:=FALSE;
    end;
    // done
    Result:=ETSR_OK;
  finally
    FSynchronizer.EndWrite;
  end;
end;

function TDBMS_Link_Basic.Query_Info(const ALinkQueryInfoClass: LongWord;
                                     const ALinkQueryInfoSize: LongWord;
                                     const ALinkQueryInfoData: Pointer): LongInt;
begin
  if (nil=Self) then begin
    Result:=ETSR_INVALID_OBJECT_POINTER;
  end else if (ETS_LQIC_TILE_ROUTINES_W=ALinkQueryInfoClass) then begin
    // get tile rountines (unicode version)
    Result:=ETSR_NOT_SUPPORTED; // use only ansi version
  end else if (ETS_LQIC_TILE_ROUTINES_A=ALinkQueryInfoClass) then begin
    // get tile rountines (ansi version)
    if (ALinkQueryInfoSize>=sizeof(TETS_LQI_TILE_ROUTINES_A)) then begin
      with PETS_LQI_TILE_ROUTINES_A(ALinkQueryInfoData)^ do begin
        p_Tile_Select_A:=r_Tile_Select_A;
        p_Tile_Query_A:=r_Tile_Query_A;
        p_Tile_Delete_A:=r_Tile_Delete_A;
        p_Tile_Insert_A:=r_Tile_Insert_A;
        p_Tne_Create_A:=r_Tne_Create_A;
        p_Ver_Free_A:=r_Ver_Free_A;
        p_Tile_Free:=r_Tile_Free;
      end;
      Result:=ETSR_OK;
    end else begin
      // invalid buffer
      Result:=ETSR_SIZE_MISMATCH;
    end;
  end else if (ETS_LQIC_CONTENT_TYPE_W=ALinkQueryInfoClass) then begin
    // get content type and other params (unicode)
    if (ALinkQueryInfoSize>=sizeof(TETS_LQI_CONTENT_TYPE_W)) then begin
      // try to connect
      if Internal_Save_Connect_or_Exit(Result) then
        Exit;
      // get info
      with PETS_LQI_CONTENT_TYPE_W(ALinkQueryInfoData)^ do begin
        szContentType:=PWideChar(FUnderlayingContentType);
        szDefaultExt:=PWideChar(FUnderlayingDefaultExt);
        dwOptions:=FUnderlayingOptions;
      end;
      Result:=ETSR_OK;
    end else begin
      // invalid buffer
      Result:=ETSR_SIZE_MISMATCH;
    end;
  end else if (ETS_LQIC_CONTENT_TYPE_A=ALinkQueryInfoClass) then begin
    // get content type and other params (ansi)
    Result:=ETSR_NOT_SUPPORTED; // use only unicode version
  end else if (ETS_LQIC_TILE_ID_FORMAT=ALinkQueryInfoClass) then begin
    // get REALLY USED tile_id format from underlaying storage
    if (0=ALinkQueryInfoSize) then begin
      // try to connect
      if Internal_Save_Connect_or_Exit(Result) then
        Exit;
      PLongWord(ALinkQueryInfoData)^:=FUnderlayingTileIdFormat;
      Result:=ETSR_NOT_SUPPORTED;
    end else begin
      // invalid buffer
      Result:=ETSR_SIZE_MISMATCH;
    end;
  end else begin
    // unknown calls
    Result:=ETSR_NOT_SUPPORTED;
  end;
end;

function TDBMS_Link_Basic.Rollback_Tran: LongInt;
begin
  FSynchronizer.BeginWrite;
  try
    Result:=ETSR_NOT_SUPPORTED;
  finally
    FSynchronizer.EndWrite;
  end;
end;

function TDBMS_Link_Basic.Set_Info(const ALinkSetInfoClass: LongWord;
                                   const ALinkSetInfoSize: LongWord;
                                   const ALinkSetInfoData: Pointer): LongInt;
var
  Vws: WideString;
  Vas: AnsiString;
begin
  FSynchronizer.BeginWrite;
  try
    if (nil=Self) then begin
      Result:=ETSR_INVALID_OBJECT_POINTER;
    end else if (0<>ALinkSetInfoSize) then begin
      // no structures here - use direct pointers and size=0
      Result:=ETSR_SIZE_MISMATCH;
    end else if (ETS_LSIC_SERVICENAME_W=ALinkSetInfoClass) then begin
      // set unicode service name
      Vws:=PWideChar(ALinkSetInfoData);
      FServiceName:=Vws;
      Result:=ETSR_OK;
    end else if (ETS_LSIC_SERVICENAME_A=ALinkSetInfoClass) then begin
      // set ansi service name
      Vas:=PAnsiChar(ALinkSetInfoData);
      FServiceName:=Vas;
      Result:=ETSR_OK;
    end else if (ETS_LSIC_CONNECTIONINFO_W=ALinkSetInfoClass) then begin
      // set unicode connection info
      Vws:=PWideChar(ALinkSetInfoData);
      FConnectionInfo:=Vws;
      Result:=ETSR_OK;
      FCanConnectToUnderlaying:=TRUE;
    end else if (ETS_LSIC_CONNECTIONINFO_A=ALinkSetInfoClass) then begin
      // set ansi connection info
      Vas:=PAnsiChar(ALinkSetInfoData);
      FConnectionInfo:=Vas;
      Result:=ETSR_OK;
      FCanConnectToUnderlaying:=TRUE;
    end else if (ETS_LSIC_CALLBACK_POINTER=ALinkSetInfoClass) then begin
      // set pointer for callbacks
      FCallbackPointer:=ALinkSetInfoData;
      Result:=ETSR_OK;
    end else if (ETS_LSIC_AUTH_FUNC=ALinkSetInfoClass) then begin
      // set function to get auth info
      FAuth_Get:=ALinkSetInfoData;
      Result:=ETSR_OK;
    end else if (ETS_LSIC_AUTH_FREE=ALinkSetInfoClass) then begin
      // set function to free auth info
      FAuth_Free:=ALinkSetInfoData;
      Result:=ETSR_OK;
    end else if (ETS_LSIC_LOST_CONNECT=ALinkSetInfoClass) then begin
      // set lost connection handler
      FLostConn:=ALinkSetInfoData;
      Result:=ETSR_OK;
    end else if (ETS_LSIC_RESTORE_CONNECT=ALinkSetInfoClass) then begin
      // set restored connection handler
      FRestConn:=ALinkSetInfoData;
      Result:=ETSR_OK;
    end else begin
      // unknown
      Result:=ETSR_NOT_SUPPORTED;
    end;
  finally
    FSynchronizer.EndWrite;
  end;
end;

function TDBMS_Link_Basic.Sync_Obj(const ASyncPointer: Pointer): LongInt;
begin
  FSynchronizer.BeginWrite;
  try
    Result:=ETSR_NOT_SUPPORTED;
  finally
    FSynchronizer.EndWrite;
  end;
end;

function TDBMS_Link_Basic.Tile_Delete_A(const AServiceName: PAnsiChar;
                                        const APtrTileID: PTILE_ID_XYZ;
                                        const AVersion: PAnsiChar;
                                        const AOptions: LongWord): LongInt;
begin
  if Internal_Save_Connect_or_Exit(Result) then
    Exit;

  FSynchronizer.BeginRead;
  try
    if (nil=APtrTileID) then begin
      // no tile_id or tile body and info
      Result:=ETSR_NO_MANDATORY_PARAMETER;
    end else begin
      // delete tile or delete tne o delete both
      if (nil=AServiceName) then begin
        // use default servicename
      end else begin
        // use this servicename
      end;
      // Result:=ETSR_OK;
    end;
  finally
    FSynchronizer.EndRead;
  end;
end;

function TDBMS_Link_Basic.Tile_Insert_A(const AServiceName: PAnsiChar;
                                        const APtrTileID: PTILE_ID_XYZ;
                                        const APtrTileInfo: PETS_TILE_BUFFER;
                                        const APtrVersionA: PETS_TILE_VERSION_A): LongInt;
begin
  if Internal_Save_Connect_or_Exit(Result) then
    Exit;

  FSynchronizer.BeginRead;
  try
    if (nil=APtrTileID) or (nil=APtrTileInfo) then begin
      // no tile_id or tile body and info
      Result:=ETSR_NO_MANDATORY_PARAMETER;
    end else begin
      // insert tile if not exists, delete tne marker
      // Result:=ETSR_OK;
    end;
  finally
    FSynchronizer.EndRead;
  end;
end;

function TDBMS_Link_Basic.Tile_Query_A(const AServiceName: PAnsiChar;
                                       const APtrTileID: PTILE_ID_XYZ;
                                       const AVersion: PAnsiChar;
                                       const APtrTileInfo: PETS_TILE_BUFFER;
                                       const APtrVersionA: PETS_TILE_VERSION_A): LongInt;
begin
  if Internal_Save_Connect_or_Exit(Result) then
    Exit;

  FSynchronizer.BeginRead;
  try
    if (nil=APtrTileID) or (nil=APtrTileInfo) then begin
      // no tile_id
      Result:=ETSR_NO_MANDATORY_PARAMETER;
    end else begin
      // query tile information
      // Result:=ETSR_OK;
    end;
  finally
    FSynchronizer.EndRead;
  end;
end;

function TDBMS_Link_Basic.Tile_Select_A(const AServiceName: PAnsiChar;
                                        const APtrTileID: PTILE_ID_XYZ;
                                        const AVersion: PAnsiChar;
                                        const APtrTileInfo: PETS_TILE_BUFFER;
                                        const APtrVersionA: PETS_TILE_VERSION_A): LongInt;
begin
  if Internal_Save_Connect_or_Exit(Result) then
    Exit;

  FSynchronizer.BeginRead;
  try
    if (nil=APtrTileID) or (nil=APtrTileInfo) then begin
      // no tile_id
      Result:=ETSR_NO_MANDATORY_PARAMETER;
    end else begin
      // retrieve tile with full information
      // Result:=ETSR_OK;
    end;
  finally
    FSynchronizer.EndRead;
  end;
end;

function TDBMS_Link_Basic.Tne_Create_A(const AServiceName: PAnsiChar;
                                       const APtrTileID: PTILE_ID_XYZ;
                                       const APtrVersionA: PETS_TILE_VERSION_A): LongInt;
begin
  if Internal_Save_Connect_or_Exit(Result) then
    Exit;

  FSynchronizer.BeginRead;
  try
    if (nil=APtrTileID) then begin
      // no tile_id
      Result:=ETSR_NO_MANDATORY_PARAMETER;
    end else begin
      // make tne marker, delete tile if exists
      // Result:=ETSR_OK;
    end;
  finally
    FSynchronizer.EndRead;
  end;
end;

function TDBMS_Link_Basic.Tran_Count(const APtrValue: PLongWord): LongInt;
begin
  FSynchronizer.BeginRead;
  try
    Result:=ETSR_NOT_SUPPORTED;
  finally
    FSynchronizer.EndRead;
  end;
end;

end.
