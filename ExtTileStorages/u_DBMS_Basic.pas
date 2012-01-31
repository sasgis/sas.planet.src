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
  t_ETS_AuthKind,
  t_ETS_Tiles,
  t_ETS_List,
  t_ETS_Provider,
  u_ETS_Path;

type
  TDBMS_Initializable = class(TObject)
  private
    FInitialized: Boolean;
  protected
    function Internal_Initialize: LongInt; virtual; abstract;
    procedure Internal_Uninitialize; virtual; abstract;
    procedure Internal_Zero; virtual; abstract;
  public
    destructor Destroy; override;

    function Initialize_Obj: LongInt;
    procedure Uninitialize_Obj;

    property Initialized: Boolean read FInitialized;
  end;

  TDBMS_Environment_Basic = class(TDBMS_Initializable)
  public
    constructor Create;
  end;

  TDBMS_Environment_Basic_Class = class of TDBMS_Environment_Basic;

  TDBMS_Connection_Basic = class(TDBMS_Initializable)
  private
    FConnected: Boolean;
    FSavedAuthSuccessfullyUsed: Boolean;
    FConnectResult: LongInt;
    FDBMS_Environment: TDBMS_Environment_Basic;
  protected
    FServerNameToConnect: WideString;
    FAuthType: LongWord; // resultant options
    FAuthDomain: WideString;
    FAuthLogin: WideString;
    FAuthPassword: WideString;
  protected
    function Internal_Connect: LongInt; virtual; abstract;
    procedure Internal_Disconnect; virtual; abstract;
  public
    constructor Create(const ADBMS_Environment: TDBMS_Environment_Basic);
    destructor Destroy; override;

    function Connect_Obj: LongInt;
    procedure Disconnect_Obj;

    property Connected: Boolean read FConnected;
    property DBMS_Environment: TDBMS_Environment_Basic read FDBMS_Environment;
  end;

  TDBMS_Connection_Basic_Class = class of TDBMS_Connection_Basic;

  TDBMS_Provider_Basic = class;

  TDBMS_Link_Basic = class(TObject)
  protected
    FProvider: TDBMS_Provider_Basic;
    FConnectionIfMultiple: TDBMS_Connection_Basic;
    FSynchronizer: TMultiReadExclusiveWriteSynchronizer;
    FHostLinkPtr: Pointer;
    FCallbackPointer: Pointer;
    FServiceName: String;
    FConnectionInfo: String;
    FDBMSPathDivided: TETS_Path_Divided;
    FUnderlayingContentType: WideString;
    FUnderlayingDefaultExt: WideString;
    FUnderlayingOptions: LongWord;
    FUnderlayingTileIdFormat: LongWord;
    FAuth_Get: TETS_Link_Auth_Func;
    FAuth_Free: TETS_Link_Auth_Func;
    FLostConn: TETS_Link_Reconn_Func;
    FRestConn: TETS_Link_Reconn_Func;
    // cannot work because of destruction
    FTerminating: Boolean;
    // all params defined and can connect to underlaying storage
    FCanConnectToUnderlaying: Boolean;
    // connected to underlaying storage, checked structure and got all params
    FUnderlayingStructureOK: Boolean;
    // result of checking underlaying storage structure
    FUnderlayingStructureResult: LongInt;
  protected
    function Internal_Get_WorkingConnection: TDBMS_Connection_Basic;
    function Internal_Check_Auth: LongInt;
    function Internal_Check_Underlaying: LongInt;
    function Internal_Initialize_Link: LongInt;
    function Internal_Uninitialize_Link: LongInt;
    function Internal_Safe_Connect_or_Exit(var AResult: LongInt; const bForDDL: Boolean = FALSE): Boolean;
    function Internal_Create_Environment_Object(var AResult: LongInt): TDBMS_Environment_Basic;
    function Internal_Create_Connection_Object(var AResult: LongInt): TDBMS_Connection_Basic;
  protected
    // overridable routines
    function Internal_Execute_DDL_A(const AServiceName: PAnsiChar;
                                    const APtrTileID: PTILE_ID_XYZ;
                                    const APtrVersionA: PETS_TILE_VERSION_A): LongInt; virtual; abstract;
  public
    constructor Create(const AHostLinkPtr: Pointer); virtual;
    destructor Destroy; override;

    function Flush_Obj: LongInt;

    function Sync_Obj(const ASyncPointer: Pointer): LongInt;

    function Begin_Tran: LongInt;
    function Commit_Tran: LongInt;
    function Rollback_Tran: LongInt;
    function Tran_Count(const APtrValue: PLongWord): LongInt;
    function Tran_Options(const APtrValue: PLongWord): LongInt;

    function Query_Info(const ALinkQueryInfoClass: LongWord;
                        const ALinkQueryInfoSize: LongWord;
                        const ALinkQueryInfoData: Pointer): LongInt;

    function Set_Info(const ALinkSetInfoClass: LongWord;
                      const ALinkSetInfoSize: LongWord;
                      const ALinkSetInfoData: Pointer): LongInt;

    // DDL routines
    function Ddl_Exec_A(const AServiceName: PAnsiChar;
                        const APtrTileID: PTILE_ID_XYZ;
                        const APtrVersionA: PETS_TILE_VERSION_A): LongInt;

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
    FConnectionIfSingle: TDBMS_Connection_Basic;
    FLinks: T_ETS_ObjectList;
    FInternalProviderName: WideString;
    FGlobalStorageId: WideString;
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

function r_Ddl_Exec_A(const ALinkHandle: TETS_Link_Handle;
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
      Result:=TDBMS_Link_Basic(ALinkHandle).Ddl_Exec_A(AServiceName,APtrTileID,APtrVersionA);
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
  FConnectionIfSingle:=nil;
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
  FreeAndNil(FConnectionIfSingle);
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
      FGlobalStorageId:=PWideChar(AProvSetInfoData);
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
    Result:=ETSR_NOT_SUPPORTED;
  finally
    FSynchronizer.EndWrite;
  end;
end;

function TDBMS_Link_Basic.Commit_Tran: LongInt;
begin
  FSynchronizer.BeginWrite;
  try
    Result:=ETSR_NOT_SUPPORTED;
  finally
    FSynchronizer.EndWrite;
  end;
end;

constructor TDBMS_Link_Basic.Create(const AHostLinkPtr: Pointer);
begin
  FSynchronizer := TMultiReadExclusiveWriteSynchronizer.Create;
  FCanConnectToUnderlaying:=FALSE;
  FUnderlayingStructureOK:=FALSE;
  FTerminating:=FALSE;
  FProvider:=nil;
  FConnectionIfMultiple:=nil;
  FCallbackPointer:=nil;
  FAuth_Get:=nil;
  FAuth_Free:=nil;
  FLostConn:=nil;
  FRestConn:=nil;
  FServiceName:='';
  FConnectionInfo:='';
  Clear_TilePath_Divided(@FDBMSPathDivided);
  FUnderlayingContentType:='';
  FUnderlayingDefaultExt:='';
  FUnderlayingOptions:=0;
  FUnderlayingTileIdFormat:=0;
  FUnderlayingStructureResult:=ETSR_NOT_CONNECTED;
  FHostLinkPtr:=AHostLinkPtr;
end;

function TDBMS_Link_Basic.Ddl_Exec_A(const AServiceName: PAnsiChar;
                                     const APtrTileID: PTILE_ID_XYZ;
                                     const APtrVersionA: PETS_TILE_VERSION_A): LongInt;
begin
  if Internal_Safe_Connect_or_Exit(Result, TRUE) then
    Exit;

  FSynchronizer.BeginWrite;
  try
    // execute DDL
    Result:=Internal_Execute_DDL_A(AServiceName, APtrTileID, APtrVersionA);
  finally
    FSynchronizer.EndWrite;
  end;
end;

destructor TDBMS_Link_Basic.Destroy;
begin
  FTerminating:=TRUE;
  Internal_Uninitialize_Link;
  FreeAndNil(FSynchronizer);
  FreeAndNil(FConnectionIfMultiple);
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

function TDBMS_Link_Basic.Internal_Check_Auth: LongInt;
var
  VWorkingConnection: TDBMS_Connection_Basic;
  VAuthSize: LongWord;
  VAuthInfo: PETS_AUTH_INFO;

  function CallAuth(const AFunc: TETS_Link_Auth_Func): LongInt;
  begin
    Result:=AFunc(TETS_Link_Handle(Pointer(Self)),
                  FCallbackPointer,
                  ETS_MASK_AK_ALL_DBMS, // ask UNICODE version!
                  @(VWorkingConnection.FAuthType),
                  @VAuthSize,
                  @VAuthInfo);
  end;
begin
  VWorkingConnection:=Internal_Get_WorkingConnection;

  // if connected - nothing to do
  if VWorkingConnection.Connected then begin
    Result:=ETSR_OK;
    Exit;
  end;

  // if saved and successfuly used auth info
  if VWorkingConnection.FSavedAuthSuccessfullyUsed then begin
    Result:=ETSR_OK;
    Exit;
  end;

  // no saved auth info OR invalid auth info
  if Assigned(FAuth_Get) and Assigned(FAuth_Free) then begin
    VAuthSize:=0;
    VAuthInfo:=nil;
    try
      // get auth info from host (for DBMS, allow integrated login, allow save)
      Result:=CallAuth(FAuth_Get);
      if (ETSR_OK=Result) then begin
        // successfully get auth info
        if (ETS_AT_LOGIN_PWD=(VWorkingConnection.FAuthType and ETS_AT_LOGIN_PWD))
           OR
           (ETS_AT_DOMAIN_LOGIN_PWD=(VWorkingConnection.FAuthType and ETS_AT_DOMAIN_LOGIN_PWD)) then begin
          // use filled string values
          with PETS_AUTH_LOGIN_PWD_W(VAuthInfo)^ do begin
            VWorkingConnection.FAuthDomain:=szDomain;
            VWorkingConnection.FAuthLogin:=szLogin;
            VWorkingConnection.FAuthPassword:=szPassword;
          end;
        end else if (ETS_AT_INTEGRATED=(VWorkingConnection.FAuthType and ETS_AT_INTEGRATED)) then begin
          // use integrated login
          VWorkingConnection.FAuthDomain:='';
          VWorkingConnection.FAuthLogin:='';
          VWorkingConnection.FAuthPassword:='';
        end else begin
          // no login info
          VWorkingConnection.FAuthDomain:='';
          VWorkingConnection.FAuthLogin:='';
          VWorkingConnection.FAuthPassword:='';
        end;
      end;
    finally
      // free buffers
      CallAuth(FAuth_Free);
    end;
  end else begin
    // cannot obtain auth info - try to get integrated login
    VWorkingConnection.FAuthDomain:='';
    VWorkingConnection.FAuthLogin:='';
    VWorkingConnection.FAuthPassword:='';
    VWorkingConnection.FAuthType:=ETS_AT_INTEGRATED;
    Result:=ETSR_OK;
  end;
end;

function TDBMS_Link_Basic.Internal_Check_Underlaying: LongInt;
begin
  // check database exists (db_name = FDBMSPathDivided[1])
  // check SERVICE table exists (table_name = FDBMSPathDivided[2], if no owner prefix - use dbo or sys)
  // check OPTIONS table exists (table_name = )
  Result:=ETSR_NOT_IMPLEMENTED;
end;

function TDBMS_Link_Basic.Internal_Create_Connection_Object(var AResult: Integer): TDBMS_Connection_Basic;
begin
  Result:=FProvider.FDBMS_Connection_Basic_Class.Create(FProvider.FEnvironment);
  AResult:=ETSR_OK;
end;

function TDBMS_Link_Basic.Internal_Create_Environment_Object(var AResult: Integer): TDBMS_Environment_Basic;
begin
  Result:=FProvider.FDBMS_Environment_Basic_Class.Create;
  AResult:=ETSR_OK;
end;

function TDBMS_Link_Basic.Internal_Get_WorkingConnection: TDBMS_Connection_Basic;
begin
  if (nil=FProvider) then
    Result:=FConnectionIfMultiple // actually NIL
  else if (FProvider.FMultipleConnections) then
    Result:=FConnectionIfMultiple
  else
    Result:=FProvider.FConnectionIfSingle;
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
    Self.FConnectionIfMultiple:=Internal_Create_Connection_Object(Result);
    if (Result<>ETSR_OK) then
      Exit;
    if (nil=Self.FConnectionIfMultiple) then begin
      Result:=ETSR_ERROR_CREATING_CONNECTION;
      Exit;
    end;
  end else if (nil=FProvider.FConnectionIfSingle) then begin
    // create single connection on provider basis
    // provider object is completely initialized - allow to create connection
    FProvider.FConnectionIfSingle:=Internal_Create_Connection_Object(Result);
    if (Result<>ETSR_OK) then
      Exit;
    if (nil=FProvider.FConnectionIfSingle) then begin
      Result:=ETSR_ERROR_CREATING_CONNECTION;
      Exit;
    end;
    // set server name
    FProvider.FConnectionIfSingle.FServerNameToConnect:=FProvider.FGlobalStorageId;
  end else begin
    // already created on provider basis
    Result:=ETSR_OK;
  end;
end;

function TDBMS_Link_Basic.Internal_Safe_Connect_or_Exit(var AResult: Integer; const bForDDL: Boolean): Boolean;
var
  VWorkingConnection: TDBMS_Connection_Basic;
begin
  Result:=FALSE;

  if (FUnderlayingStructureOK) then begin
    // everything is ok
    // AResult:=ETSR_OK;
    Exit;
  end;

  // cannot connect because of destroying connection
  if FTerminating then begin
    AResult:=ETSR_NOT_CONNECTED;
    Result:=TRUE;
    Exit;
  end;

  // not enough initialized to be connected
  if (not FCanConnectToUnderlaying) then begin
    AResult:=ETSR_NOT_INITIALIZED;
    Result:=TRUE;
    Exit;
  end;

  FSynchronizer.BeginWrite;
  try
    VWorkingConnection:=Internal_Get_WorkingConnection;

    // can connect and not connected - try to connect
    if (FCanConnectToUnderlaying) and (not VWorkingConnection.Connected) then begin
      // if failed in connect - return the reason
      if (ETSR_OK<>VWorkingConnection.FConnectResult) then begin
        AResult:=VWorkingConnection.FConnectResult;
        Result:=TRUE;
        Exit;
      end;

      // check auth before connect
      AResult:=Internal_Check_Auth;
      if (AResult<>ETSR_OK) then begin
        Result:=TRUE;
        Exit;
      end;

      // if multiple connections - set server name here
      FDBMSPathDivided:=ETS_TilePath_Divided(FProvider.FGlobalStorageId, FServiceName);
      VWorkingConnection.FServerNameToConnect:=FDBMSPathDivided.Path_Items[0];

      // connect
      AResult:=VWorkingConnection.Connect_Obj;
      if (AResult<>ETSR_OK) then begin
        Result:=TRUE;
        Exit;
      end;
    end;

    // if connected and not checking underlaying storage - do it here
    if (VWorkingConnection.Connected) and (not FUnderlayingStructureOK) then begin
      // if called to exec DDL - reset flags and skip
      if bForDDL then begin
        FUnderlayingStructureResult:=ETSR_OK;
        Result:=FALSE; // allow to execute DDL
        Exit;
      end;

      // if had already failed
      if (ETSR_OK<>FUnderlayingStructureResult) then begin
        AResult:=FUnderlayingStructureResult;
        Result:=TRUE;
        Exit;
      end;

      // check underlaying storage
      AResult:=Internal_Check_Underlaying;
      FUnderlayingStructureResult:=AResult;
      if (AResult<>ETSR_OK) then begin
        Result:=TRUE;
        Exit;
      end;
    end;
  finally
    FSynchronizer.EndWrite;
  end;
end;

function TDBMS_Link_Basic.Internal_Uninitialize_Link: LongInt;
begin
  FUnderlayingStructureOK:=FALSE;
  FCanConnectToUnderlaying:=FALSE;
  FSynchronizer.BeginWrite;
  try
    // disconnect from underlaying storage - only if own connection
    if Assigned(FConnectionIfMultiple) then begin
      FConnectionIfMultiple.Disconnect_Obj;
    end;

    FUnderlayingStructureResult:=ETSR_NOT_CONNECTED;

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
        p_Ddl_Exec_A:=r_Ddl_Exec_A;
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
      if Internal_Safe_Connect_or_Exit(Result) then
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
      if Internal_Safe_Connect_or_Exit(Result) then
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
  if Internal_Safe_Connect_or_Exit(Result) then
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
  if Internal_Safe_Connect_or_Exit(Result) then
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
  if Internal_Safe_Connect_or_Exit(Result) then
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
  if Internal_Safe_Connect_or_Exit(Result) then
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
  if Internal_Safe_Connect_or_Exit(Result) then
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

function TDBMS_Link_Basic.Tran_Options(const APtrValue: PLongWord): LongInt;
begin
  FSynchronizer.BeginRead;
  try
    Result:=ETSR_NOT_SUPPORTED;
  finally
    FSynchronizer.EndRead;
  end;
end;

{ TDBMS_Initializable }

destructor TDBMS_Initializable.Destroy;
begin
  Uninitialize_Obj;
  inherited;
end;

function TDBMS_Initializable.Initialize_Obj: LongInt;
begin
  if FInitialized then
    Result:=ETSR_OK
  else begin
    Result:=Internal_Initialize;
    if (ETSR_OK=Result) then
      FInitialized:=TRUE;
  end;
end;

procedure TDBMS_Initializable.Uninitialize_Obj;
begin
  if FInitialized then begin
    Internal_Uninitialize;
    FInitialized:=FALSE;
  end;
end;

{ TDBMS_Environment_Basic }

constructor TDBMS_Environment_Basic.Create;
begin
  FInitialized:=FALSE;
  Internal_Zero;
end;

{ TDBMS_Connection_Basic }

function TDBMS_Connection_Basic.Connect_Obj: LongInt;
begin
  // initialize
  Result:=Initialize_Obj;
  if (ETSR_OK<>Result) then
    Exit;
  // connect
  if FConnected then
    Result:=ETSR_OK
  else begin
    Result:=Internal_Connect;
    if (ETSR_OK=Result) then begin
      // connected
      FSavedAuthSuccessfullyUsed:=TRUE;
      FConnected:=TRUE
    end else begin
      // failed
      FSavedAuthSuccessfullyUsed:=FALSE;
      FConnectResult:=Result;
    end;
  end;
end;

constructor TDBMS_Connection_Basic.Create(const ADBMS_Environment: TDBMS_Environment_Basic);
begin
  FInitialized:=FALSE;
  FConnected:=FALSE;
  FSavedAuthSuccessfullyUsed:=FALSE;
  FDBMS_Environment:=ADBMS_Environment;
  FConnectResult:=ETSR_OK;
  FAuthType:=0;
  FAuthDomain:='';
  FAuthLogin:='';
  FAuthPassword:='';
  FServerNameToConnect:='';
  Internal_Zero;
end;

destructor TDBMS_Connection_Basic.Destroy;
begin
  Disconnect_Obj;
  inherited Destroy;
end;

procedure TDBMS_Connection_Basic.Disconnect_Obj;
begin
  if FConnected then begin
    Internal_Disconnect;
    FConnected:=FALSE;
    FConnectResult:=ETSR_OK;
  end;
end;

end.
