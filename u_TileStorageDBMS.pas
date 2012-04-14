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

unit u_TileStorageDBMS;

interface

uses
  Types,
  Classes,
  SysUtils,
  Windows,
  i_BinaryData,
  i_SimpleTileStorageConfig,
  i_MapVersionInfo,
  i_ContentTypeInfo,
  i_TileInfoBasic,
  i_ContentTypeManager,
  i_TTLCheckNotifier,
  i_TTLCheckListener,
  u_GlobalCahceConfig,
  u_MapTypeCacheConfig,
  u_TileStorageAbstract,
  t_ETS_Tiles,
  t_ETS_Provider;

type
  TTileStorageDBMS = class(TTileStorageAbstract)
  private
    FContentTypeManager: IContentTypeManager;
    FGCList: ITTLCheckNotifier;
    FTTLListener: ITTLCheckListener;
    FTileNotExistsTileInfo: ITileInfoBasic;
    FCacheConfig: TMapTypeCacheConfigDBMS;
    FMainContentType: IContentTypeInfoBasic;
    // cached values
    FGlobalStorageIdentifier: String;
    FServiceName: String;
    FContentTypeExtensions: TStringList;
    // access
    FDLLSync: IReadWriteSync;
    FDLLHandle: THandle;
    FProviderHandle: TETS_Provider_Handle;
    FStatusBuffer: TETS_STATUS_BUFFER;
    // routines
    FETS_Insert_TNE_A: Pointer;
    FETS_Insert_TNE_W: Pointer;
    FETS_Insert_Tile_A: Pointer;
    FETS_Insert_Tile_W: Pointer;
    FETS_Delete_TNE_A: Pointer;
    FETS_Delete_TNE_W: Pointer;
    FETS_Delete_Tile_TNE_A: Pointer;
    FETS_Delete_Tile_TNE_W: Pointer;
    FETS_Query_Tile_A: Pointer;
    FETS_Query_Tile_W: Pointer;
    FETS_Sync: Pointer;
  protected
    // Lib routines
    function InternalLib_CleanupProc: Boolean; virtual;
    function InternalLib_Initialize: Boolean; virtual;
    function InternalLib_CheckInitialized: Boolean; virtual;
    function InternalLib_Connected: Boolean; virtual;
    function InternalLib_Unload: Boolean; virtual;
    function InternalLib_NotifyStateChanged(const AEnabled: Boolean): Boolean;
    function InternalLib_SetStorageIdentifier: Boolean;
    function InternalLib_SetPrimaryContentType: Boolean;
    function InternalLib_QueryAllContentTypes: Boolean;
  protected
    procedure SyncTTL(Sender: TObject);

    procedure DoOnMapSettingsEdit(Sender: TObject);

    function GetUTCNow: TDateTime;

    function GetProviderContentType(const AContentTypeId: Word): IContentTypeInfoBasic;

    // get tile or just get info about tile
    function QueryTileInternal(
      const AXY: TPoint;
      const Azoom: byte;
      const AVersionInfo: IMapVersionInfo;
      AStream: TStream;
      out ATileInfo: ITileInfoBasic
    ): Boolean;

    // insert/delete tile/tne
    function SendTileCommand(
      const AXY: TPoint;
      const Azoom: byte;
      const AVersionInfo: IMapVersionInfo;
      const AData: Pointer;
      const AFuncA, AFuncW: Pointer
    ): Boolean;
  public
    constructor Create(
      AGCList: ITTLCheckNotifier;
      AConfig: ISimpleTileStorageConfig;
      AGlobalCacheConfig: TGlobalCahceConfig;
      AContentTypeManager: IContentTypeManager
    );
    
    destructor Destroy; override;

    function GetMainContentType: IContentTypeInfoBasic; override;
    function GetAllowDifferentContentTypes: Boolean; override;

    function GetCacheConfig: TMapTypeCacheConfigAbstract; override;

    function GetTileFileName(
      const AXY: TPoint;
      const Azoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): string; override;

    function GetTileInfo(
      const AXY: TPoint;
      const Azoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): ITileInfoBasic; override;

    function LoadTile(
      const AXY: TPoint;
      const Azoom: byte;
      const AVersionInfo: IMapVersionInfo;
      out ATileInfo: ITileInfoBasic
    ): IBinaryData; override;

    function DeleteTile(
      const AXY: TPoint;
      const Azoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): Boolean; override;

    function DeleteTNE(
      const AXY: TPoint;
      const Azoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): Boolean; override;

    procedure SaveTile(
      const AXY: TPoint;
      const Azoom: byte;
      const AVersionInfo: IMapVersionInfo;
      const AData: IBinaryData
    ); override;

    procedure SaveTNE(
      const AXY: TPoint;
      const Azoom: byte;
      const AVersionInfo: IMapVersionInfo
    ); override;

  end;

implementation

uses
  t_CommonTypes,
  u_BinaryDataByMemStream,
  u_MapVersionFactorySimpleString,
  u_MapVersionInfo,
  u_Synchronizer,
  vsagps_public_sysutils,
  u_TTLCheckListener,
  u_TileStorageTypeAbilities,
  u_TileInfoBasic;

const
  C_TileStorage_DBMS_DLL = 'TileStorage_DBMS.dll';

function rETS_QueryAllContentTypes_Callback_A(
  const AHostPointer: Pointer;
  const AQueryPointer: Pointer;
  const AIndex: Integer;
  const AExtensions: PAnsiChar;
  const AContentType: PAnsiChar): Boolean; stdcall;
var
  VExtensions: AnsiString;
begin
  Result := FALSE;
  if (AQueryPointer<>nil) and (AExtensions<>nil) then
  try
    SetString(VExtensions, AExtensions, StrLen(AExtensions));
    TStringList(AQueryPointer).Insert(AIndex, VExtensions);
    Inc(Result);
  except
  end;
end;

function rETS_QueryAllContentTypes_Callback_W(
  const AHostPointer: Pointer;
  const AQueryPointer: Pointer;
  const AIndex: Integer;
  const AExtensions: PWideChar;
  const AContentType: PWideChar): Boolean; stdcall;
var
  VExtensions: WideString;
begin
  Result := FALSE;
  if (AQueryPointer<>nil) and (AExtensions<>nil) then
  try
    SetString(VExtensions, AExtensions, StrLenW(AExtensions));
    TStringList(AQueryPointer).Insert(AIndex, VExtensions);
    Inc(Result);
  except
  end;
end;

function rETS_Query_Tile_Callback_A(
  const AHostPointer: Pointer;
  const ATileBuffer: Pointer;
  const AVersionBuffer: PAnsiChar;
  const AData: PETS_QUERY_TILE_DATA): Boolean; stdcall;
var
  VVersionString: AnsiString;
begin
  Result := FALSE;
  if (AData<>nil) then
  try
    // version
    if (AVersionBuffer<>nil) and (nil=AData^.VersionHolder) then begin
      SetString(VVersionString, AVersionBuffer, StrLen(AVersionBuffer));
      AData^.VersionHolder := TMapVersionInfo.Create(VVersionString);
    end;
    // buffer
    if (ATileBuffer<>nil) and (AData^.TileHolder<>nil) and (AData^.TileSize>0) then begin
      TStream(AData^.TileHolder).WriteBuffer(ATileBuffer^, AData^.TileSize);
    end;
    // ok
    Inc(Result);
  except
  end;
end;

function rETS_Query_Tile_Callback_W(
  const AHostPointer: Pointer;
  const ATileBuffer: Pointer;
  const AVersionBuffer: PWideChar;
  const AData: PETS_QUERY_TILE_DATA): Boolean; stdcall;
var
  VVersionString: WideString;
begin
  Result := FALSE;
  if (AData<>nil) then
  try
    // version
    if (AVersionBuffer<>nil) and (nil=AData^.VersionHolder) then begin
      SetString(VVersionString, AVersionBuffer, StrLenW(AVersionBuffer));
      AData^.VersionHolder := TMapVersionInfo.Create(VVersionString);
    end;
    // buffer
    if (ATileBuffer<>nil) and (AData^.TileHolder<>nil) and (AData^.TileSize>0) then begin
      TStream(AData^.TileHolder).WriteBuffer(ATileBuffer^, AData^.TileSize);
    end;
    // ok
    Inc(Result);
  except
  end;
end;

{ TTileStorageDBMS }

constructor TTileStorageDBMS.Create(
  AGCList: ITTLCheckNotifier;
  AConfig: ISimpleTileStorageConfig;
  AGlobalCacheConfig: TGlobalCahceConfig;
  AContentTypeManager: IContentTypeManager
);
const
  CBDBSync = 300000; // 5 min
  CBDBSyncCheckInterval = 60000; // 60 sec
begin
  FDLLSync := MakeSyncRW_Big(Self, TRUE);

  FGlobalStorageIdentifier := '';
  FServiceName := '';
  FContentTypeExtensions := nil;
  FDLLHandle := 0;
  FProviderHandle := nil;

  inherited Create(
    TTileStorageTypeAbilitiesDBMS.Create,
    TMapVersionFactorySimpleString.Create,
    AConfig
  );

  InternalLib_CleanupProc;

  FGCList := AGCList;

  FTileNotExistsTileInfo := TTileInfoBasicNotExists.Create(0, nil);

  FCacheConfig := TMapTypeCacheConfigDBMS.Create(
    AConfig,
    AGlobalCacheConfig,
    DoOnMapSettingsEdit
  );

  FContentTypeManager := AContentTypeManager;
  FMainContentType := FContentTypeManager.GetInfoByExt(Config.TileFileExt);

  FTTLListener := TTTLCheckListener.Create(Self.SyncTTL, CBDBSync, CBDBSyncCheckInterval);
  FGCList.Add(FTTLListener);

  // do not check result here
  InternalLib_Initialize;

  DoOnMapSettingsEdit(nil);
end;

destructor TTileStorageDBMS.Destroy;
begin
  StorageStateInternal.ReadAccess := asDisabled;

  FDLLSync.BeginWrite;
  try
    InternalLib_Unload;
  finally
    FDLLSync.EndWrite;
  end;
  
  FGCList.Remove(FTTLListener);
  FTTLListener := nil;
  FGCList := nil;
  FMainContentType := nil;
  FContentTypeManager := nil;
  FreeAndNil(FCacheConfig);
  FreeAndNil(FContentTypeExtensions);
  FTileNotExistsTileInfo := nil;
  FDLLSync := nil;
  inherited;
end;

procedure TTileStorageDBMS.DoOnMapSettingsEdit(Sender: TObject);
var
  VGlobalStorageIdentifier, VServiceName: String;
  VAccesState: TAccesState;
begin
  if (nil=FCacheConfig) then
    Exit;

  // get to cache
  VGlobalStorageIdentifier := FCacheConfig.GlobalStorageIdentifier;
  VServiceName := FCacheConfig.ServiceName;

  // check
  if SameText(VGlobalStorageIdentifier, FGlobalStorageIdentifier) and SameText(VServiceName, FServiceName) then
    Exit;

  // change params
  FDLLSync.BeginWrite;
  try
    VAccesState := StorageStateInternal.ReadAccess;
    StorageStateInternal.ReadAccess := asUnknown;
    FGlobalStorageIdentifier := VGlobalStorageIdentifier;
    FServiceName := VServiceName;
    // call
    if not InternalLib_SetStorageIdentifier then
      StorageStateInternal.ReadAccess := VAccesState;
  finally
    FDLLSync.EndWrite;
  end;
  
end;

function TTileStorageDBMS.DeleteTile(
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo
): Boolean;
begin
  Result := SendTileCommand(AXY, AZoom, AVersionInfo, nil, FETS_Delete_Tile_TNE_A, FETS_Delete_Tile_TNE_W);
end;

function TTileStorageDBMS.DeleteTNE(
  const AXY: TPoint;
  const Azoom: byte;
  const AVersionInfo: IMapVersionInfo
): Boolean;
begin
  Result := SendTileCommand(AXY, AZoom, AVErsionInfo, nil, FETS_Delete_TNE_A, FETS_Delete_TNE_W)
end;

function TTileStorageDBMS.GetAllowDifferentContentTypes: Boolean;
begin
  Result := TRUE;
end;

function TTileStorageDBMS.GetCacheConfig: TMapTypeCacheConfigAbstract;
begin
  Result := FCacheConfig;
end;

function TTileStorageDBMS.GetMainContentType: IContentTypeInfoBasic;
begin
  Result := FMainContentType;
end;

function TTileStorageDBMS.GetProviderContentType(const AContentTypeId: Word): IContentTypeInfoBasic;
var VExt: WideString;
begin
  if (0<>AContentTypeId) and (FContentTypeExtensions<>nil) then begin
    VExt := '';

    // check content_type exists
    repeat
      FDLLSync.BeginRead;
      try
        if (FContentTypeExtensions<>nil) and (AContentTypeId<FContentTypeExtensions.Count) then
          VExt := FContentTypeExtensions[AContentTypeId];
      finally
        FDLLSync.EndRead;
      end;

      if (0<Length(VExt)) then
        break;
      if (nil=FContentTypeExtensions) then
        break;

      // changed - read again (from storage provider)
      InternalLib_QueryAllContentTypes;

      Sleep(200);
    until FALSE;

    // check if ok
    if (0<Length(VExt)) then begin
      // done
      Result := FContentTypeManager.GetInfoByExt(VExt);
    end;

    Exit;
  end;
  
  // default content type
  Result := FMainContentType;
end;

function TTileStorageDBMS.GetTileFileName(
  const AXY: TPoint;
  const Azoom: byte;
  const AVersionInfo: IMapVersionInfo
): string;
var VVer: String;
begin
  // TODO: add AVersionInfo to underlaying call GetTileFileName
  Result := FCacheConfig.GetTileFileName(AXY, Azoom);
  if Assigned(AVersionInfo) then begin
    VVer:=AVersionInfo.StoreString;
    if (0<=Length(VVer)) then
      Result:=Result +'['+VVer+']';
  end;
end;

function TTileStorageDBMS.GetTileInfo(
  const AXY: TPoint;
  const Azoom: byte;
  const AVersionInfo: IMapVersionInfo
): ITileInfoBasic;
begin
  QueryTileInternal(AXY, Azoom, AVersionInfo, nil, Result);
end;

function TTileStorageDBMS.GetUTCNow: TDateTime;
var st: TSystemTime;
begin
  GetSystemTime(st);
  Result := SystemTimeToDateTime(st);
end;

function TTileStorageDBMS.InternalLib_CheckInitialized: Boolean;
begin
  Result := (0<>FDLLHandle) and
            (nil<>FProviderHandle) and
            ((nil<>FETS_Query_Tile_A) or (nil<>FETS_Query_Tile_W)) and
            ((nil<>FETS_Insert_TNE_A) or (nil<>FETS_Insert_TNE_W)) and
            ((nil<>FETS_Insert_Tile_A) or (nil<>FETS_Insert_Tile_W)) and
            ((nil<>FETS_Delete_TNE_A) or (nil<>FETS_Delete_TNE_W)) and
            ((nil<>FETS_Delete_Tile_TNE_A) or (nil<>FETS_Delete_Tile_TNE_W));
  // FETS_Sync can be NULL
end;

function TTileStorageDBMS.InternalLib_CleanupProc: Boolean;
begin
  Result := FALSE;

  // routines
  FETS_Insert_TNE_A := nil;
  FETS_Insert_TNE_W := nil;
  FETS_Insert_Tile_A := nil;
  FETS_Insert_Tile_W := nil;
  FETS_Delete_TNE_A := nil;
  FETS_Delete_TNE_W := nil;
  FETS_Delete_Tile_TNE_A := nil;
  FETS_Delete_Tile_TNE_W := nil;
  FETS_Query_Tile_A := nil;
  FETS_Query_Tile_W := nil;
  FETS_Sync := nil;

  // status
  FillChar(FStatusBuffer, SizeOf(FStatusBuffer), 0);
  FStatusBuffer.wSize := SizeOf(FStatusBuffer);
end;

function TTileStorageDBMS.InternalLib_Connected: Boolean;
begin
  Result := InternalLib_CheckInitialized;
  if Result then
    Result := (FStatusBuffer.Status_Current<=FStatusBuffer.Status_MaxOK);
end;

function TTileStorageDBMS.InternalLib_Initialize: Boolean;
var p: Pointer;
begin
  Result := FALSE;

  if (0=FDLLHandle) then
    FDLLHandle := LoadLibraryW(C_TileStorage_DBMS_DLL);

  if (0<>FDLLHandle) then begin
    // get init proc
    p := GetProcAddress(FDLLHandle, 'ETS_Initialize');
    if (nil<>p) then begin
      Result := TETS_Initialize(p)(@FProviderHandle, @FStatusBuffer, 0, Pointer(Self));
    end;

    if Result then begin
      // set callbacks
      p := GetProcAddress(FDLLHandle, 'ETS_SetInformation');
      if (nil<>p) then begin
        // set informaion
        TETS_SetInformation(p)(@FProviderHandle, ETS_INFOCLASS_QUERY_TILE_CALLBACK_A, 0, @rETS_Query_Tile_Callback_A, nil);
        TETS_SetInformation(p)(@FProviderHandle, ETS_INFOCLASS_QUERY_TILE_CALLBACK_W, 0, @rETS_Query_Tile_Callback_W, nil);
      end;

      // initialized - get other functions
      FETS_Insert_TNE_A := GetProcAddress(FDLLHandle, 'ETS_Insert_TNE_A');
      FETS_Insert_TNE_W := GetProcAddress(FDLLHandle, 'ETS_Insert_TNE_W');
      FETS_Insert_Tile_A := GetProcAddress(FDLLHandle, 'ETS_Insert_Tile_A');
      FETS_Insert_Tile_W := GetProcAddress(FDLLHandle, 'ETS_Insert_Tile_W');
      FETS_Delete_TNE_A := GetProcAddress(FDLLHandle, 'ETS_Delete_TNE_A');
      FETS_Delete_TNE_W := GetProcAddress(FDLLHandle, 'ETS_Delete_TNE_W');
      FETS_Delete_Tile_TNE_A := GetProcAddress(FDLLHandle, 'ETS_Delete_Tile_TNE_A');
      FETS_Delete_Tile_TNE_W := GetProcAddress(FDLLHandle, 'ETS_Delete_Tile_TNE_W');
      FETS_Query_Tile_A := GetProcAddress(FDLLHandle, 'ETS_Query_Tile_A');
      FETS_Query_Tile_W := GetProcAddress(FDLLHandle, 'ETS_Query_Tile_W');
      FETS_Sync := GetProcAddress(FDLLHandle, 'ETS_Sync');

      if InternalLib_CheckInitialized then begin
        // final operations
        InternalLib_SetPrimaryContentType;
        InternalLib_QueryAllContentTypes;

        // completely
        p := GetProcAddress(FDLLHandle, 'ETS_Complete');
        if (nil<>p) then begin
          // notify provider
          TETS_Complete(p)(@FProviderHandle, 0);
        end;
      end else begin
        // failed to initialize
        Result := FALSE;
        InternalLib_Unload;
      end;
    end;
  end;
end;

function TTileStorageDBMS.InternalLib_NotifyStateChanged(const AEnabled: Boolean): Boolean;
var VReadAccess: TAccesState;
begin
  Result := FALSE;
  
  if AEnabled then
    VReadAccess := asEnabled
  else
    VReadAccess := asDisabled;

  StorageStateInternal.ReadAccess := VReadAccess;
end;

function TTileStorageDBMS.InternalLib_QueryAllContentTypes: Boolean;
var
  p: Pointer;
begin
  Result := FALSE;
  if (0<>FDLLHandle) then
  if (nil<>FProviderHandle) then begin
    FDLLSync.BeginWrite;
    try
      if (nil=FContentTypeExtensions) then
        FContentTypeExtensions := TStringList.Create
      else
        FContentTypeExtensions.Clear;

      // try ANSI version
      p := GetProcAddress(FDLLHandle, 'ETS_QueryAllContentTypes_A');
      if (nil<>p) then begin
        TETS_QueryAllContentTypes_A(p)(@FProviderHandle, FContentTypeExtensions, rETS_QueryAllContentTypes_Callback_A);
      end;

      // try UNICODE version
      p := GetProcAddress(FDLLHandle, 'ETS_QueryAllContentTypes_W');
      if (nil<>p) then begin
        TETS_QueryAllContentTypes_W(p)(@FProviderHandle, FContentTypeExtensions, rETS_QueryAllContentTypes_Callback_W);
      end;
    finally
      if (FContentTypeExtensions<>nil) and (0=FContentTypeExtensions.Count) then
        FreeAndNil(FContentTypeExtensions);

      FDLLSync.EndWrite;
    end;
  end;
end;

function TTileStorageDBMS.InternalLib_SetPrimaryContentType: Boolean;
var
  p: Pointer;
  VDefaultExtA, VContentTypeA: AnsiString;
  VDefaultExtW, VContentTypeW: WideString;
begin
  Result := FALSE;
  if (0<>FDLLHandle) then
  if (nil<>FProviderHandle) then begin
    // try ANSI version
    p := GetProcAddress(FDLLHandle, 'ETS_SetPrimaryContentType_A');
    if (nil<>p) then begin
      VDefaultExtA := FMainContentType.GetDefaultExt;
      VContentTypeA := FMainContentType.GetContentType;
      Result := TETS_SetPrimaryContentType_A(p)(@FProviderHandle, PAnsiChar(VDefaultExtA), PAnsiChar(VContentTypeA));
      if Result then
        Exit;
    end;

    // try UNICODE version
    p := GetProcAddress(FDLLHandle, 'ETS_SetPrimaryContentType_W');
    if (nil<>p) then begin
      VDefaultExtW := FMainContentType.GetDefaultExt;
      VContentTypeW := FMainContentType.GetContentType;
      Result := TETS_SetPrimaryContentType_W(p)(@FProviderHandle, PWideChar(VDefaultExtW), PWideChar(VContentTypeW));
    end;
  end;
end;

function TTileStorageDBMS.InternalLib_SetStorageIdentifier: Boolean;
var
  p: Pointer;
  dwFlagsOut: Cardinal;
  VGlobalStorageIdentifierA, VServiceNameA: AnsiString;
  VGlobalStorageIdentifierW, VServiceNameW: WideString;
begin
  Result := FALSE;
  try
    if (0=FDLLHandle) then
      InternalLib_Initialize;
    if InternalLib_CheckInitialized then begin
      // try ANSI version
      p := GetProcAddress(FDLLHandle, 'ETS_SetStorageIdentifier_A');
      if (nil<>p) then begin
        VGlobalStorageIdentifierA := FGlobalStorageIdentifier;
        VServiceNameA := FServiceName;
        dwFlagsOut := 0;
        Result := TETS_SetStorageIdentifier_A(p)(@FProviderHandle, PAnsiChar(VGlobalStorageIdentifierA), PAnsiChar(VServiceNameA), @dwFlagsOut);
        Exit;
      end;
      
      // try UNICODE version
      p := GetProcAddress(FDLLHandle, 'ETS_SetStorageIdentifier_W');
      if (nil<>p) then begin
        VGlobalStorageIdentifierW := FGlobalStorageIdentifier;
        VServiceNameW := FServiceName;
        dwFlagsOut := 0;
        Result := TETS_SetStorageIdentifier_W(p)(@FProviderHandle, PWideChar(VGlobalStorageIdentifierW), PWideChar(VServiceNameW), @dwFlagsOut);
        Exit;
      end;
    end;
  finally
    InternalLib_NotifyStateChanged(Result);
  end;
end;

function TTileStorageDBMS.InternalLib_Unload: Boolean;
var p: Pointer;
begin
  Result := FALSE;
  if (0<>FDLLHandle) then begin
    // uninit
    p := GetProcAddress(FDLLHandle, 'ETS_Uninitialize');
    if (nil<>p) then begin
      TETS_Uninitialize(p)(@FProviderHandle, 0);
    end;
    
    // finishing
    Inc(Result);
    try
      FreeLibrary(FDLLHandle);
    except
      // catch any exception from dll finalization
    end;

    FDLLHandle := 0;
    InternalLib_CleanupProc;
    InternalLib_NotifyStateChanged(FALSE);
  end;
end;

function TTileStorageDBMS.LoadTile(
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  out ATileInfo: ITileInfoBasic
): IBinaryData;
var
  VMemStream: TMemoryStream;
begin
  Result := nil;
  VMemStream:=TMemoryStream.Create;
  try
    if QueryTileInternal(AXY, Azoom, AVersionInfo, VMemStream, ATileInfo) then begin
      Result := TBinaryDataByMemStream.CreateWithOwn(VMemStream);
      VMemStream := nil;
    end;
  finally
    VMemStream.Free;
  end;
end;

function TTileStorageDBMS.QueryTileInternal(
  const AXY: TPoint;
  const Azoom: byte;
  const AVersionInfo: IMapVersionInfo;
  AStream: TStream;
  out ATileInfo: ITileInfoBasic): Boolean;
var
  VData: TETS_QUERY_TILE_DATA;

  function _GetResultVersion: IMapVersionInfo;
  begin
    if ((VData.dwFlagsOut and ETS_QTO_SOURCE_VERSION) <> 0) then begin
      // same version
      Result := AVersionInfo;
    end else begin
      // get from data
      Result := TMapVersionInfo(VData.VersionHolder);
      VData.VersionHolder := nil;
    end;
  end;

begin
  FillChar(VData, SizeOf(VData), 0);
  VData.wSize := SizeOf(VData);
  VData.TileHolder := AStream;
  try
    Result := SendTileCommand(AXY, Azoom, AVersionInfo, @VData, FETS_Query_Tile_A, FETS_Query_Tile_W);

    if (not Result) then begin
      // failed
      ATileInfo := nil;
    end else if ((VData.dwFlagsOut and ETS_QTO_TILE_NOT_FOUND) <> 0) then begin
      // no info
      ATileInfo := FTileNotExistsTileInfo;
    end else if (0=VData.TileSize) then begin
      // TNE
      ATileInfo := TTileInfoBasicTNE.Create(VData.TileDate, _GetResultVersion);
    end else begin
      // tile found
      ATileInfo := TTileInfoBasicExists.Create(VData.TileDate, VData.TileSize, _GetResultVersion, GetProviderContentType(VData.wContentType));
    end;
  finally
    // cleanup version
    if (VData.VersionHolder<>nil) then
      FreeAndNil(TMapVersionInfo(VData.VersionHolder));
  end;
end;

procedure TTileStorageDBMS.SaveTile(
  const AXY: TPoint;
  const Azoom: byte;
  const AVersionInfo: IMapVersionInfo;
  const AData: IBinaryData
);
var
  VData: TETS_INSERT_TILE_DATA;
begin
  FillChar(VData, SizeOf(VData), 0);
  VData.wSize := SizeOf(VData);
  if Assigned(AData) then begin
    VData.TileSize := AData.Size;
    VData.TileBuffer := AData.Buffer;
    VData.TileDate := GetUTCNow;
  end;
  SendTileCommand(AXY, Azoom, AVersionInfo, @VData, FETS_Insert_Tile_A, FETS_Insert_Tile_W);
end;

procedure TTileStorageDBMS.SaveTNE(
  const AXY: TPoint;
  const Azoom: byte;
  const AVersionInfo: IMapVersionInfo
);
begin
  SendTileCommand(AXY, Azoom, AVersionInfo, nil, FETS_Insert_TNE_A, FETS_Insert_TNE_W);
end;

function TTileStorageDBMS.SendTileCommand(
  const AXY: TPoint;
  const Azoom: byte;
  const AVersionInfo: IMapVersionInfo;
  const AData: Pointer;
  const AFuncA, AFuncW: Pointer): Boolean;
var
  Vtid: TTILE_ID_XYZ;
  VVersionStringA: AnsiString;
  VVersionStringW: WideString;
begin
  Result := FALSE;

  if StorageStateStatic.WriteAccess <> asDisabled then begin
    // ensure connected
    if not InternalLib_Connected then
      Exit;

    // make tile_id struct
    Vtid.x:=AXY.X;
    Vtid.y:=AXY.Y;
    Vtid.z:=AZoom;

    FDLLSync.BeginRead;
    try
      if (nil<>FProviderHandle) then
      if ((nil<>AFuncA) or (nil<>AFuncW)) then
      try
        // execute
        if (nil<>AFuncA) then begin
          // ansi
          VVersionStringA := AVersionInfo.StoreString;
          Result := TETS_Single_Tile_Command_A(AFuncA)(@FProviderHandle, @Vtid, PAnsiChar(VVersionStringA), AData);
        end else begin
          // unicode
          VVersionStringW := AVersionInfo.StoreString;
          Result := TETS_Single_Tile_Command_W(AFuncW)(@FProviderHandle, @Vtid, PWideChar(VVersionStringW), AData);
        end;
      except
      end;
    finally
      FDLLSync.EndRead;
      // notify
      if Result then
        NotifyTileUpdate(AXY, Azoom, AVersionInfo);
    end;
  end;
end;

procedure TTileStorageDBMS.SyncTTL(Sender: TObject);
begin
  FDLLSync.BeginWrite;
  try
    if (nil<>FProviderHandle) and (nil<>FETS_Sync) then
      TETS_Sync(FETS_Sync)(@FProviderHandle, 0);
  finally
    FDLLSync.EndWrite;
  end;
end;

end.
