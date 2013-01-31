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
  Classes,
  SysUtils,
  Windows,
  i_BinaryData,
  i_MapVersionInfo,
  i_MapVersionConfig,
  i_BasicMemCache,
  i_ContentTypeInfo,
  i_TileInfoBasic,
  i_TileStorage,
  i_InternalDomainOptions,
  i_CoordConverter,
  i_ContentTypeManager,
  i_NotifierTime,
  i_ListenerTime,
  i_TileInfoBasicMemCache,
  u_TileStorageAbstract,
  t_ETS_Tiles,
  t_ETS_Provider;

type
  TTileStorageETS = class(TTileStorageAbstract
                        , IInternalDomainOptions
                        , IMapVersionChanger
                        , IBasicMemCache)
  // base interface
  private
    FMainContentType: IContentTypeInfoBasic;
    FContentTypeManager: IContentTypeManager;
    FMapVersionConfig: IMapVersionConfig;
    FGCNotifier: INotifierTime;
    FETSTTLListener: IListenerTimeWithUsedFlag;
    FTileInfoMemCache: ITileInfoBasicMemCache;

    // some special values
    FTileNotExistsTileInfo: ITileInfoBasic;
    FEmptyVersion: IMapVersionInfo;

    // access
    FDLLSync: IReadWriteSync;
    FDLLHandle: THandle;
    FDLLProvHandle: TETS_Provider_Handle;

    // routines
    FETS_Sync: Pointer;       // TETS_Sync (OPTIONAL)
    FETS_SelectTile: Pointer; // TETS_SelectTile
    FETS_InsertTile: Pointer; // TETS_InsertTile
    FETS_InsertTNE: Pointer;  // TETS_InsertTile (OPTIONAL)
    FETS_DeleteTile: Pointer; // TETS_DeleteTile
    FETS_SetInformation: Pointer;   // TETS_SetInformation
    FETS_EnumTileVersions: Pointer; // TETS_EnumTileVersions (OPTIONAL)
    FETS_GetTileRectInfo: Pointer;  // TETS_GetTileRectInfo (OPTIONAL)
    FETS_MakeTileEnum: Pointer;     // TETS_MakeTileEnum (OPTIONAL)
    FETS_NextTileEnum: Pointer;     // TETS_NextTileEnum (OPTIONAL)
    FETS_KillTileEnum: Pointer;     // TETS_KillTileEnum (OPTIONAL)
    FETS_ExecOption: Pointer;       // TETS_ExecOption (OPTIONAL)
    FETS_FreeMem: Pointer;          // TETS_FreeMem (OPTIONAL)

    // shared buffer
    FETS_SERVICE_STORAGE_OPTIONS: TETS_SERVICE_STORAGE_OPTIONS;

    // sync provider routine
    procedure DoProviderSync;
    // internal sync prov caller
    function InternalProviderSync(const AExclusiveFlag: LongWord): Byte;
  private
    function InternalLib_CleanupProc: Boolean; virtual;
    function InternalLib_Initialize: Boolean; virtual;
    function InternalLib_CheckInitialized: Boolean; virtual;
    function InternalLib_Unload: Boolean; virtual;
    function InternalLib_NotifyStateChanged(const AEnabled: Boolean): Boolean;
    function InternalLib_SetPath(const AGlobalStorageIdentifier, AServiceName: String): Boolean;
    function InternalLib_Complete: Boolean;

    // sync routines
    procedure DoBeginWork(const AExclusiveFlag: LongWord; out AExclusively: Boolean);
    procedure DoEndWork(const AExclusively: Boolean);

    // helpers
    function InternalCreate_Version(
      const AVersionStr: Pointer;
      const AOptionsInp: LongWord;
      const AOptionsOut: LongWord
    ): IMapVersionInfo;

    function InternalCreate_ContentType(
      const AContentTypeStr: Pointer;
      const AOptionsInp: LongWord;
      const AOptionsOut: LongWord
    ): IContentTypeInfoBasic;

    function InternalCreate_BinaryData(
      const AOptionsInp: LongWord;
      const ASelectBufferOut: PETS_SELECT_TILE_OUT
    ): IBinaryData;

    function InternalCreate_TileInfoType(
      const AOptionsOut: LongWord
    ): TTileInfoType;

  private
    // service helpers routines
    procedure CheckMalfunction;
    function MalfunctionDetected: Boolean; inline;
    function GetInitialExclusiveFlag(const AForQuery: Boolean): LongWord;

    procedure InternalSaveTileOrTNE(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo;
      const ALoadDate: TDateTime;
      const AData: IBinaryData;
      const ARoutinePtr: Pointer;
      const ACallForTNE: Boolean
    );
    
  private
    function CallbackLib_SelectTile(
      const ACallbackPointer: Pointer;
      const ASelectBufferInp: PETS_SELECT_TILE_IN;
      const ASelectBufferOut: PETS_SELECT_TILE_OUT
    ): Byte;

    function CallbackLib_EnumTileVersions(
      const ACallbackPointer: Pointer;
      const AEnumTileVerBufferInp: PETS_SELECT_TILE_IN;
      const AEnumTileVerBufferOut: PETS_ENUM_TILE_VERSION_OUT
    ): Byte;

    function CallbackLib_GetTileRectInfo(
      const ACallbackPointer: Pointer;
      const ATileRectInfoInp: PETS_GET_TILE_RECT_IN;
      const ATileRectInfoOut: PETS_GET_TILE_RECT_OUT
    ): Byte;

    function CallbackLib_SetVersionNotifier(
      const ACallbackPointer: Pointer;
      const ASetVersionOption: PETS_SET_VERSION_OPTION
    ): Byte;

  private
    { IMapVersionChanger }
    procedure SetMapVersionConfig(const AMapVersionConfig: IMapVersionConfig);
  private
    { IInternalDomainOptions }
    function DomainHtmlOptions(
      const AFullPrefix, ARequest: String;
      out AResponse: String;
      out AFlags: TDomainOptionsResponseFlags;
      const ARequestType: LongWord = c_IDO_RT_None
    ): Boolean;
  private
    { IBasicMemCache }
    procedure ClearMemCache;
    procedure IBasicMemCache.Clear = ClearMemCache;
  protected
    // base storage interface
    function GetIsFileCache: Boolean; override;
    function GetTileFileName(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): string; override;

    function GetTileInfo(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo;
      const AMode: TGetTileInfoMode
    ): ITileInfoBasic; override;

    function GetTileRectInfo(
      const ARect: TRect;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): ITileRectInfo; override;

    function DeleteTile(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): Boolean; override;

    procedure SaveTile(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo;
      const ALoadDate: TDateTime;
      const AContentType: IContentTypeInfoBasic;
      const AData: IBinaryData
    ); override;

    procedure SaveTNE(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo;
      const ALoadDate: TDateTime
    ); override;

    function GetListOfTileVersions(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): IMapVersionListStatic; override;

    function ScanTiles(
      const AIgnoreTNE: Boolean
    ): IEnumTileInfo; override;
    
  public
    constructor Create(
      const AGeoConverter: ICoordConverter;
      const AGlobalStorageIdentifier, AStoragePath: String;
      const AGCNotifier: INotifierTime;
      const ACacheTileInfo: ITileInfoBasicMemCache;
      const AContentTypeManager: IContentTypeManager;
      const AMapVersionFactory: IMapVersionFactory;
      const AMainContentType: IContentTypeInfoBasic
    );
    destructor Destroy; override;
  end;

  TTileStorageDBMS = class(TTileStorageETS)
    // for TileStorage_DBMS.dll
  protected
    function InternalLib_Initialize: Boolean; override;
  end;

type
  EETSBaseError           = class(Exception);
  EETSNoRoutine           = class(EETSBaseError);
  EETSFailed              = class(EETSBaseError);
  EETSCriticalError       = class(EETSBaseError);
  EETSCannotConnect       = class(EETSBaseError);
  EETSDeadConnection      = class(EETSBaseError);
  EETSNoSpaceAvailable    = class(EETSBaseError);
  EETSDataTruncation      = class(EETSBaseError);
  EETSCantCreateTable     = class(EETSBaseError);
  EETSReadOnlyConnection  = class(EETSBaseError);
  EETSUnknownError        = class(EETSBaseError);
  EETSCannotParseTile     = class(EETSBaseError);

implementation

uses
  t_CommonTypes,
  u_BinaryDataByMemStream,
  u_MapVersionListStatic,
  u_Synchronizer,
  u_ListenerTime,
  u_TileStorageTypeAbilities,
  u_TileRectInfoShort,
  u_TileInfoBasic,
  u_ResStrings,
  u_BaseInterfacedObject;

procedure SetUpExclusiveFlag(var AExclusiveFlag: LongWord); inline;
begin
  AExclusiveFlag := (AExclusiveFlag or ETS_ROI_EXCLUSIVELY)
end;

function ExclusiveFlagWasSetUp(const AExclusiveFlag: LongWord): Boolean; inline;
begin
  Result := ((AExclusiveFlag and ETS_ROI_EXCLUSIVELY) <> 0)
end;

type
  PTileInfo = ^TTileInfo;

type
  TEnumTileInfoByETS = class(TBaseInterfacedObject, IEnumTileInfo)
  private
    // from origin
    FIgnoreTNE: Boolean;
    FStorage: TTileStorageETS;

    // link to provider
    FETSEnumTilesHandle: TETS_EnumTiles_Handle;
    FETSAllEnumInfo: TETS_GET_TILE_RECT_IN;
    FETSResult: Byte;
   
  private
    function InternalMake: Byte;
    function InternalKill: Byte;
  private
    function CallbackLib_NextTileEnum(
      const ACallbackPointer: Pointer;
      const ANextBufferInp: PETS_GET_TILE_RECT_IN;
      const ANextBufferOut: PETS_NEXT_TILE_ENUM_OUT
    ): Byte;
  private
    function Next(var ATileInfo: TTileInfo): Boolean;
  public
    constructor Create(
      const AIgnoreTNE: Boolean;
      const AStorage: TTileStorageETS
    );
    destructor Destroy; override;
  end;

  TSelectTileCallbackInfo = packed record
    TileResult: ITileInfoBasic;
    AllowSaveToMemCache: Boolean;
    //UseGetTileInfoMode: TGetTileInfoMode;
  end;
  PSelectTileCallbackInfo = ^TSelectTileCallbackInfo;

  TEnumTileVersionsCallbackInfo = packed record
    TileVersionsList: IInterfaceList;
  end;
  PEnumTileVersionsCallbackInfo = ^TEnumTileVersionsCallbackInfo;

  TGetTileRectCallbackInfo = packed record
    TileCount: TPoint;
    // InfoCount: Integer;
    InfoArray: TArrayOfTileInfoShortInternal;
  end;
  PGetTileRectCallbackInfo = ^TGetTileRectCallbackInfo;

function Host_SelectTile_Callback(
  const AHostPointer: Pointer;
  const ACallbackPointer: Pointer;
  const ASelectBufferInp: PETS_SELECT_TILE_IN;
  const ASelectBufferOut: PETS_SELECT_TILE_OUT
): Byte; stdcall;
begin
  try
    if (nil=AHostPointer) then
      Result := ETS_RESULT_INVALID_HOST_PTR
    else if (nil=ACallbackPointer) then
      Result := ETS_RESULT_INVALID_CALLBACK_PTR
    else
      Result := TTileStorageETS(AHostPointer).CallbackLib_SelectTile(ACallbackPointer, ASelectBufferInp, ASelectBufferOut);
  except
    Result := ETS_RESULT_CALLBACK_EXCEPTION;
  end;
end;

function Host_EnumTileVersions_Callback(
  const AHostPointer: Pointer;
  const ACallbackPointer: Pointer;
  const AEnumTileVerBufferInp: PETS_SELECT_TILE_IN;
  const AEnumTileVerBufferOut: PETS_ENUM_TILE_VERSION_OUT
): Byte; stdcall;
begin
  try
    if (nil=AHostPointer) then
      Result := ETS_RESULT_INVALID_HOST_PTR
    else if (nil=ACallbackPointer) then
      Result := ETS_RESULT_INVALID_CALLBACK_PTR
    else
      Result := TTileStorageETS(AHostPointer).CallbackLib_EnumTileVersions(ACallbackPointer, AEnumTileVerBufferInp, AEnumTileVerBufferOut);
  except
    Result := ETS_RESULT_CALLBACK_EXCEPTION;
  end;
end;

function Host_GetTileRectInfo_Callback(
  const AHostPointer: Pointer;
  const ACallbackPointer: Pointer;
  const ATileRectInfoInp: PETS_GET_TILE_RECT_IN;
  const ATileRectInfoOut: PETS_GET_TILE_RECT_OUT
): Byte; stdcall;
begin
  try
    if (nil=AHostPointer) then
      Result := ETS_RESULT_INVALID_HOST_PTR
    else if (nil=ACallbackPointer) then
      Result := ETS_RESULT_INVALID_CALLBACK_PTR
    else
      Result := TTileStorageETS(AHostPointer).CallbackLib_GetTileRectInfo(ACallbackPointer, ATileRectInfoInp, ATileRectInfoOut);
  except
    Result := ETS_RESULT_CALLBACK_EXCEPTION;
  end;
end;

function Host_NextTileEnum_Callback(
  const AHostPointer: Pointer;
  const ACallbackPointer: Pointer;
  const ANextBufferInp: PETS_GET_TILE_RECT_IN;
  const ANextBufferOut: PETS_NEXT_TILE_ENUM_OUT
): Byte; stdcall;
begin
  try
    if (nil=AHostPointer) then
      Result := ETS_RESULT_INVALID_HOST_PTR
    else if (nil=ACallbackPointer) then
      Result := ETS_RESULT_INVALID_CALLBACK_PTR
    else
      Result := TEnumTileInfoByETS(AHostPointer).CallbackLib_NextTileEnum(ACallbackPointer, ANextBufferInp, ANextBufferOut);
  except
    Result := ETS_RESULT_CALLBACK_EXCEPTION;
  end;
end;

function Host_SetVersion_Notifier(
  const AHostPointer: Pointer;
  const ACallbackPointer: Pointer;
  const ASetVersionOption: PETS_SET_VERSION_OPTION
): Byte; stdcall;
begin
  try
    if (nil=AHostPointer) then
      Result := ETS_RESULT_INVALID_HOST_PTR
    else if (nil=ACallbackPointer) then
      Result := ETS_RESULT_INVALID_CALLBACK_PTR
    else
      Result := TTileStorageETS(AHostPointer).CallbackLib_SetVersionNotifier(ACallbackPointer, ASetVersionOption);
  except
    Result := ETS_RESULT_CALLBACK_EXCEPTION;
  end;
end;


{ TTileStorageETS }

function TTileStorageETS.CallbackLib_EnumTileVersions(
  const ACallbackPointer: Pointer;
  const AEnumTileVerBufferInp: PETS_SELECT_TILE_IN;
  const AEnumTileVerBufferOut: PETS_ENUM_TILE_VERSION_OUT
): Byte;
var
  VVersion: IMapVersionInfo;
begin
  Result := ETS_RESULT_OK;

  // ACallbackPointer is PEnumTileVersionsCallbackInfo
  with PEnumTileVersionsCallbackInfo(ACallbackPointer)^ do
  if (nil=TileVersionsList) then begin
    // make list
    TileVersionsList := TInterfaceList.Create;
  end;

  // make version
  VVersion := InternalCreate_Version(
    Pointer(PETS_VERSION_A(AEnumTileVerBufferOut^.ResponseValue)^.ver_value), // PETS_VERSION_A or PETS_VERSION_W
    AEnumTileVerBufferInp^.dwOptionsIn,
    ETS_ROO_TILE_EXISTS // always create!
  );

  // add this version to list
  PEnumTileVersionsCallbackInfo(ACallbackPointer)^.TileVersionsList.Add(VVersion);
end;

function TTileStorageETS.CallbackLib_GetTileRectInfo(
  const ACallbackPointer: Pointer;
  const ATileRectInfoInp: PETS_GET_TILE_RECT_IN;
  const ATileRectInfoOut: PETS_GET_TILE_RECT_OUT
): Byte;
var
  VIndex: Integer;
begin
  // ACallbackPointer is PGetTileRectCallbackInfo
  // add info from tile
  VIndex := TTileRectInfoShort.TileInRectToIndex(ATileRectInfoOut^.TilePos, ATileRectInfoInp^.ptTileRect^);
  // write info to TTileInfoShortInternal
  if (VIndex>=0) then begin
    with PGetTileRectCallbackInfo(ACallbackPointer)^.InfoArray[VIndex] do begin
      // base fields
      FLoadDate := ATileRectInfoOut^.TileInfo.dtLoadedUTC;
      FSize     := ATileRectInfoOut^.TileInfo.dwTileSize;
      // derived field
      FInfoType := InternalCreate_TileInfoType(ATileRectInfoOut^.TileInfo.dwOptionsOut);
    end;
    Result := ETS_RESULT_OK;
  end else begin
    // invalid tile position
    Result := ETS_RESULT_INVALID_TILE_POS;
  end;
end;

function TTileStorageETS.CallbackLib_SelectTile(
  const ACallbackPointer: Pointer;
  const ASelectBufferInp: PETS_SELECT_TILE_IN;
  const ASelectBufferOut: PETS_SELECT_TILE_OUT
): Byte;
var
  VTileVersion: IMapVersionInfo;
  VTileContentType: IContentTypeInfoBasic;
  VTileBody: IBinaryData;
begin
  Result := ETS_RESULT_OK;

  // make Version
  VTileVersion := InternalCreate_Version(
    ASelectBufferOut^.szVersionOut,
    ASelectBufferInp^.dwOptionsIn,
    ASelectBufferOut^.dwOptionsOut
  );

  // make ContentType
  VTileContentType := InternalCreate_ContentType(
    ASelectBufferOut^.szContentTypeOut,
    ASelectBufferInp^.dwOptionsIn,
    ASelectBufferOut^.dwOptionsOut
  );

  // make BinaryData
  VTileBody := InternalCreate_BinaryData(
    ASelectBufferInp^.dwOptionsIn,
    ASelectBufferOut
  );

  // ACallbackPointer is PSelectTileCallbackInfo
  if ((ASelectBufferOut^.dwOptionsOut and ETS_ROO_TILE_EXISTS) <> 0) then begin
    // tile exists
    if Assigned(VTileBody) then begin
      // with body
      PSelectTileCallbackInfo(ACallbackPointer)^.TileResult := TTileInfoBasicExistsWithTile.Create(
        ASelectBufferOut^.dtLoadedUTC,
        VTileBody,
        VTileVersion,
        VTileContentType
      );
    end else begin
      // without body
      PSelectTileCallbackInfo(ACallbackPointer)^.TileResult := TTileInfoBasicExists.Create(
        ASelectBufferOut^.dtLoadedUTC,
        ASelectBufferOut^.dwTileSize,
        VTileVersion,
        VTileContentType
      );
      // не сохраняем в MemCache
      PSelectTileCallbackInfo(ACallbackPointer)^.AllowSaveToMemCache := FALSE;
    end;
  end else if ((ASelectBufferOut^.dwOptionsOut and ETS_ROO_TNE_EXISTS) <> 0) then begin
    // TNE exists
    PSelectTileCallbackInfo(ACallbackPointer)^.TileResult := TTileInfoBasicTNE.Create(
      ASelectBufferOut^.dtLoadedUTC,
      VTileVersion
    );
  end else begin
    // neither tile nor TNE
    PSelectTileCallbackInfo(ACallbackPointer)^.TileResult := nil;
  end;
end;

function TTileStorageETS.CallbackLib_SetVersionNotifier(
  const ACallbackPointer: Pointer;
  const ASetVersionOption: PETS_SET_VERSION_OPTION
): Byte;
var
  VStringVersion: String;
  VMapVersionInfo: IMapVersionInfo;
begin
  Result := ETS_RESULT_OK;
  if (ASetVersionOption<>nil) and (FMapVersionConfig<>nil) then begin
    // make version by pointer and flag
    if (nil=ASetVersionOption^.szVersion) then begin
      // no version
      VMapVersionInfo := FEmptyVersion;
    end else begin
      // has version
      if ((ASetVersionOption^.dwOptions and ETS_SVO_ANSI_VALUES)<>0) then begin
        // version is AnsiString
        VStringVersion := (AnsiString(PAnsiChar(ASetVersionOption^.szVersion)));
      end else begin
        // version is WideString
        VStringVersion := (WideString(PWideChar(ASetVersionOption^.szVersion)));
      end;
      VMapVersionInfo := MapVersionFactory.CreateByStoreString(VStringVersion);
    end;

    // switch to version
    if (nil<>VMapVersionInfo) then begin
      FMapVersionConfig.Version := VMapVersionInfo;
    end;
  end;
end;

procedure TTileStorageETS.CheckMalfunction;
begin
  // проверяем только перечисленное тут
  case FETS_SERVICE_STORAGE_OPTIONS.malfunction_mode of
    ETS_PMM_INITIAL_MODE, ETS_PMM_NOT_COMPLETED: begin
      // ошибка протокола связи с хранилищем
      raise EETSNoRoutine.Create(SAS_ERR_ETS_NotImplemented);
    end;
    ETS_PMM_CONNECT_DEAD: begin
      // соединение было, но оно разорвано и не восстанавливается
      raise EETSDeadConnection.Create(SAS_ERR_ETS_ConnectionIsDead);
    end;
    ETS_PMM_FAILED_CONNECT: begin
      // никак не получается подключиться
      raise EETSCannotConnect.Create(SAS_ERR_ETS_CannotConnect);
    end;
  end;
end;

procedure TTileStorageETS.ClearMemCache;
begin
  if Assigned(FTileInfoMemCache) then
    FTileInfoMemCache.Clear;
end;

constructor TTileStorageETS.Create(
  const AGeoConverter: ICoordConverter;
  const AGlobalStorageIdentifier, AStoragePath: String;
  const AGCNotifier: INotifierTime;
  const ACacheTileInfo: ITileInfoBasicMemCache;
  const AContentTypeManager: IContentTypeManager;
  const AMapVersionFactory: IMapVersionFactory;
  const AMainContentType: IContentTypeInfoBasic);
const
  CETSSync = 300000; // 5 min
var
  VCorrectPath: String;
begin
  VCorrectPath := AStoragePath;
  while (0<Length(VCorrectPath)) and (VCorrectPath[Length(VCorrectPath)]=PathDelim) do begin
    SetLength(VCorrectPath, Length(VCorrectPath)-1);
  end;

  inherited Create(
    TTileStorageTypeAbilitiesDBMS.Create,
    AMapVersionFactory,
    AGeoConverter,
    VCorrectPath
  );

  FETS_SERVICE_STORAGE_OPTIONS.Clear;

  FDLLSync := MakeSyncRW_Big(Self);

  FContentTypeManager := AContentTypeManager;
  FMainContentType := AMainContentType;
  FMapVersionConfig := nil;
  FTileInfoMemCache := ACacheTileInfo;

  FTileNotExistsTileInfo := TTileInfoBasicNotExists.Create(0, nil);
  FEmptyVersion := MapVersionFactory.CreateByStoreString('');

  FETSTTLListener := TListenerTTLCheck.Create(DoProviderSync, CETSSync);

  FGCNotifier := AGCNotifier;
  if Assigned(FGCNotifier) then begin
    FGCNotifier.Add(FETSTTLListener);
  end;

  FDLLHandle := 0;
  FDLLProvHandle := nil;
  InternalLib_CleanupProc;

  if not InternalLib_SetPath(AGlobalStorageIdentifier, StoragePath) then begin
    StorageStateInternal.ReadAccess := asEnabled;
  end;

  InternalLib_Complete;
end;

function TTileStorageETS.DeleteTile(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo
): Boolean;
var
  VLockedExclusively: Boolean;
  VResult: Byte;
  VTileID: TTILE_ID_XYZ;
  VBufferIn: TETS_DELETE_TILE_IN;
  VVersionString: String;
begin
  Result := FALSE;
  // check if no routine
  if (nil=FETS_DeleteTile) then
    Exit;
  if MalfunctionDetected then
    Exit;

  VTileID.z := 0; // check for first time
  VResult := ETS_RESULT_NEED_EXCLUSIVE; // any value <> ETS_RESULT_OK
  VBufferIn.dwOptionsIn := GetInitialExclusiveFlag(FALSE);
  repeat
    // let us go
    DoBeginWork(VBufferIn.dwOptionsIn, VLockedExclusively);
    try
      if StorageStateInternal.DeleteAccess <> asDisabled then begin
        // allow delete - initialize buffers
        if (0=VTileID.z) then begin
          VTileID.xy := AXY;
          VTileID.z := AZoom+1; // zoom from 1
          VBufferIn.XYZ := @VTileID;
          // make version
          VVersionString := AVersionInfo.StoreString;
          VBufferIn.szVersionIn := PChar(VVersionString); // Pointer to VersionString with the same type of char
          if SizeOf(Char)=SizeOf(AnsiChar) then begin
            // AnsiString
            VBufferIn.dwOptionsIn := (VBufferIn.dwOptionsIn or ETS_ROI_ANSI_VERSION_IN);
          end;
        end;

        // request to storage
        VResult := TETS_DeleteTile(FETS_DeleteTile)(
          @FDLLProvHandle,
          @VBufferIn);
      end else begin
        // no access
        Exit;
      end;
    finally
      DoEndWork(VLockedExclusively);
    end;

    // check response
    case VResult of
      ETS_RESULT_DISCONNECTED: begin
        // repeat exclusively
        SetUpExclusiveFlag(VBufferIn.dwOptionsIn);
      end;
      ETS_RESULT_NEED_EXCLUSIVE: begin
        // repeat exclusively
        if ExclusiveFlagWasSetUp(VBufferIn.dwOptionsIn) then
          Exit;
        SetUpExclusiveFlag(VBufferIn.dwOptionsIn);
      end;
      ETS_RESULT_OK: begin
        // success
        Result := TRUE;
        // break to exit loop and write to cache
        break;
      end;
      else begin
        // failed
        Exit;
      end;
    end;
  until FALSE;

  if Result then begin
    if Assigned(FTileInfoMemCache) then begin
      // delete both tile and TNE
      FTileInfoMemCache.Remove(AXY, AZoom);
    end;
    FETSTTLListener.CheckUseTimeUpdated;
    NotifyTileUpdate(AXY, AZoom, AVersionInfo);
  end;
end;

destructor TTileStorageETS.Destroy;
var VDummyLocked: Boolean;
begin
  StorageStateInternal.ReadAccess := asDisabled;

  DoBeginWork(ETS_ROI_EXCLUSIVELY, VDummyLocked);
  try
    InternalLib_Unload;

    if Assigned(FGCNotifier) then begin
      FGCNotifier.Remove(FETSTTLListener);
      FGCNotifier := nil;
    end;

    FETSTTLListener := nil;
    FTileInfoMemCache := nil;   
    FMapVersionConfig := nil;
    FMainContentType := nil;
    FContentTypeManager := nil;
    FTileNotExistsTileInfo := nil;
    FEmptyVersion := nil;
  finally
    DoEndWork(VDummyLocked);
  end;

  inherited;

  FDLLSync := nil;
end;

procedure TTileStorageETS.DoBeginWork(const AExclusiveFlag: LongWord; out AExclusively: Boolean);
begin
  AExclusively := ExclusiveFlagWasSetUp(AExclusiveFlag);
  if AExclusively then
    FDLLSync.BeginWrite
  else
    FDLLSync.BeginRead;
end;

procedure TTileStorageETS.DoEndWork(const AExclusively: Boolean);
begin
  if AExclusively then
    FDLLSync.EndWrite
  else
    FDLLSync.EndRead;
end;

procedure TTileStorageETS.DoProviderSync;
begin
  if (nil=FETS_Sync) then
    Exit;

  if ETS_RESULT_NEED_EXCLUSIVE = InternalProviderSync(0) then
    InternalProviderSync(ETS_ROI_EXCLUSIVELY);
end;

function TTileStorageETS.DomainHtmlOptions(
  const AFullPrefix, ARequest: String;
  out AResponse: String;
  out AFlags: TDomainOptionsResponseFlags;
  const ARequestType: LongWord
): Boolean;
var
  VResponseW: WideString;
  VOptionIn: TETS_EXEC_OPTION_IN;
begin
  AResponse := '';
  AFlags := [];
  if (0<Length(AFullPrefix)) and Assigned(FETS_ExecOption) and Assigned(FETS_FreeMem) then begin
    // supported
    FillChar(VOptionIn, sizeof(VOptionIn), 0);

    if SizeOf(Char)=SizeOf(AnsiChar) then begin
      VOptionIn.dwOptionsIn := VOptionIn.dwOptionsIn or ETS_EOI_ANSI_VALUES;
    end;
    VOptionIn.szFullPrefix := PChar(AFullPrefix);
    VOptionIn.szRequest    := PChar(ARequest);

    if (0<>ARequestType) then begin
      VOptionIn.dwRequestType := ARequestType;
      VOptionIn.dwOptionsIn := VOptionIn.dwOptionsIn or ETS_EOI_REQUEST_TYPE;
    end;
    
    // call
    TETS_ExecOption(FETS_ExecOption)(
      @FDLLProvHandle,
      nil,
      @VOptionIn
    );

    // do not check result from DLL because it can return errorcode from DBMS
    // check response only
    Result := (nil<>VOptionIn.szResponse);

    try
      if Result then begin
        // set response to output
        if ((VOptionIn.dwOptionsOut and ETS_EOO_ANSI_VALUES) <> 0) then begin
          // as AnsiString
          AResponse := PAnsiChar(VOptionIn.szResponse);
        end else begin
          // as WideString
          VResponseW := PWideChar(VOptionIn.szResponse);
          AResponse := VResponseW;
        end;

        if ((VOptionIn.dwOptionsOut and ETS_EOO_HTML_DECORATED) <> 0) then begin
          Include(AFlags, dorf_HtmlDecorated);
        end;

        if ((VOptionIn.dwOptionsOut and ETS_EOO_CLEAR_MEMCACHE) <> 0) then begin
          Include(AFlags, dorf_ClearMemCache);
          if Assigned(FTileInfoMemCache) then
            FTileInfoMemCache.Clear;
        end;

        if ((VOptionIn.dwOptionsOut and ETS_EOO_NEED_REFRESH) <> 0) then begin
          Include(AFlags, dorf_Refresh);
        end;

        if ((VOptionIn.dwOptionsOut and ETS_EOO_NEED_RESTART) <> 0) then begin
          Include(AFlags, dorf_Restart);
        end;

      end;
    finally
      if (VOptionIn.szResponse<>nil) then begin
        // second call
        VOptionIn.szFullPrefix:=nil;
        VOptionIn.szRequest:=nil;
        TETS_FreeMem(FETS_FreeMem)(
          VOptionIn.szResponse
        );
      end;
    end;
  end else begin
    // not supported
    Result := FALSE;
  end;
end;

function TTileStorageETS.GetInitialExclusiveFlag(const AForQuery: Boolean): LongWord;
begin
  Result := 0;

  if (ETS_PMM_ESTABLISHED<>FETS_SERVICE_STORAGE_OPTIONS.malfunction_mode) then begin
    // если нет подключения - запросы пускаем по очереди
    SetUpExclusiveFlag(Result);
    Exit;
  end;

  case FETS_SERVICE_STORAGE_OPTIONS.exclusive_mode of
    ETS_HEM_EXCLISUVE: begin
      // all exclusive - start from 1
      SetUpExclusiveFlag(Result);
    end;
    ETS_HEM_QUERY_ONLY: begin
      // synchronize query only
      if AForQuery then
        SetUpExclusiveFlag(Result);
    end;
  end;
end;

function TTileStorageETS.GetIsFileCache: Boolean;
begin
  Result := False;
end;

function TTileStorageETS.GetListOfTileVersions(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo
): IMapVersionListStatic;
var
  VLockedExclusively: Boolean;
  VResult: Byte;
  VVersionString: String;
  VObj: TEnumTileVersionsCallbackInfo;
  VTileID: TTILE_ID_XYZ;
  VBufferIn: TETS_SELECT_TILE_IN;
begin
  VResult := ETS_RESULT_OK;
  FillChar(VObj, SizeOf(VObj), 0);
  VTileID.z := 0; // initial flag
  VBufferIn.dwOptionsIn := GetInitialExclusiveFlag(TRUE);
  if (nil<>FETS_EnumTileVersions) then
  if (not MalfunctionDetected) then
  repeat
    // let us go
    DoBeginWork(VBufferIn.dwOptionsIn, VLockedExclusively);
    try
      if StorageStateInternal.ReadAccess <> asDisabled then begin
        // has access

        // initialize buffers
        if (0=VTileID.z) then begin
          VTileID.xy := AXY;
          VTileID.z := AZoom+1; // zoom from 1
          VBufferIn.XYZ := @VTileID;
          // make flags
          VBufferIn.dwOptionsIn := VBufferIn.dwOptionsIn or (ETS_ROI_ANSI_CONTENTTYPE_IN or ETS_ROI_ANSI_CONTENTTYPE_OUT);
          // make version
          VVersionString := AVersionInfo.StoreString;
          VBufferIn.szVersionIn := PChar(VVersionString); // Pointer to VersionString with the same type of char
          if SizeOf(Char)=SizeOf(AnsiChar) then begin
            // AnsiString
            VBufferIn.dwOptionsIn := (VBufferIn.dwOptionsIn or ETS_ROI_ANSI_VERSION_IN or ETS_ROI_ANSI_VERSION_OUT);
          end;
        end;

        // request to storage
        VResult := TETS_EnumTileVersions(FETS_EnumTileVersions)(
          @FDLLProvHandle,
          @VObj,
          @VBufferIn);
      end else begin
        // no access
        break;
      end;
    finally
      DoEndWork(VLockedExclusively);
    end;

    // check response
    case VResult of
      ETS_RESULT_DISCONNECTED: begin
        // repeat exclusively
        SetUpExclusiveFlag(VBufferIn.dwOptionsIn);
      end;
      ETS_RESULT_NEED_EXCLUSIVE: begin
        // repeat exclusively
        if ExclusiveFlagWasSetUp(VBufferIn.dwOptionsIn) then
          break;
        SetUpExclusiveFlag(VBufferIn.dwOptionsIn);
      end;
      ETS_RESULT_OK: begin
        // success - output result object after break
        break;
      end;
      else begin
        // failed
        break;
      end;
    end;
  until FALSE;
  
  // make result
  Result := TMapVersionListStatic.Create(VObj.TileVersionsList);
end;

function TTileStorageETS.GetTileFileName(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo
): string;
begin
  Result := StoragePath;
end;

function TTileStorageETS.GetTileInfo(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo;
  const AMode: TGetTileInfoMode
): ITileInfoBasic;
var
  VLockedExclusively: Boolean;
  VResult: Byte;
  VVersionString: String;
  VObj: TSelectTileCallbackInfo;
  VTileID: TTILE_ID_XYZ;
  VBufferIn: TETS_SELECT_TILE_IN;
begin
  // try to read from cache
  if Assigned(FTileInfoMemCache) then begin
    Result := FTileInfoMemCache.Get(AXY, AZoom, True);
    if Result <> nil then begin
      Exit;
    end;
  end;

  Result := FTileNotExistsTileInfo;

  if MalfunctionDetected then
    Exit;

  VResult := ETS_RESULT_OK;

  FillChar(VObj, SizeOf(VObj), 0);
  VObj.AllowSaveToMemCache := Assigned(FTileInfoMemCache);
  //VObj.UseGetTileInfoMode := AMode;

  VTileID.z := 0;
  VBufferIn.dwOptionsIn := GetInitialExclusiveFlag(TRUE);
  repeat
    // let us go
    DoBeginWork(VBufferIn.dwOptionsIn, VLockedExclusively);
    try
      if StorageStateInternal.ReadAccess <> asDisabled then begin
        // has access

        // initialize buffers
        if (0=VTileID.z) then begin
          VTileID.xy := AXY;
          VTileID.z := AZoom+1; // zoom from 1
          VBufferIn.XYZ := @VTileID;
          // make flags (all except ETS_STI_CHECK_EXISTS, ContentType is AnsiString)
          VBufferIn.dwOptionsIn := VBufferIn.dwOptionsIn or (ETS_ROI_ANSI_CONTENTTYPE_IN or ETS_ROI_ANSI_CONTENTTYPE_OUT);
          if (AMode=gtimWithData) then begin
            VBufferIn.dwOptionsIn := (VBufferIn.dwOptionsIn or ETS_ROI_SELECT_TILE_BODY);
          end;
          // make version
          VVersionString := AVersionInfo.StoreString;
          VBufferIn.szVersionIn := PChar(VVersionString); // Pointer to VersionString with the same type of char
          if SizeOf(Char)=SizeOf(AnsiChar) then begin
            // AnsiString
            VBufferIn.dwOptionsIn := (VBufferIn.dwOptionsIn or ETS_ROI_ANSI_VERSION_IN or ETS_ROI_ANSI_VERSION_OUT);
          end;
        end;

        // request to storage
        VResult := TETS_SelectTile(FETS_SelectTile)(
          @FDLLProvHandle,
          @VObj,
          @VBufferIn);
      end else begin
        // no access
        Exit;
      end;
    finally
      DoEndWork(VLockedExclusively);
    end;

    // check response
    case VResult of
      ETS_RESULT_DISCONNECTED: begin
        // repeat exclusively
        SetUpExclusiveFlag(VBufferIn.dwOptionsIn);
      end;
      ETS_RESULT_NEED_EXCLUSIVE: begin
        // repeat exclusively
        if ExclusiveFlagWasSetUp(VBufferIn.dwOptionsIn) then begin
          raise EETSCriticalError.Create(SAS_ERR_ETS_CriticalError);
          //Exit;
        end;
        SetUpExclusiveFlag(VBufferIn.dwOptionsIn);
      end;
      ETS_RESULT_OK: begin
        // success - output result object
        if (VObj.TileResult<>nil) then begin
          Result := VObj.TileResult;
        end;
        // break to exit loop and write to cache
        break;
      end;
      ETS_RESULT_UNKNOWN_VERSION: begin
        // version not found - i.e. no tile
        break;
      end
      else begin
        // failed
        raise EETSCriticalError.Create(SAS_ERR_ETS_CriticalError);
        Exit;
      end;
    end;
  until FALSE;

  // write to cache
  if VObj.AllowSaveToMemCache and Assigned(FTileInfoMemCache) then begin
    FTileInfoMemCache.Add(AXY, AZoom, AVersionInfo, Result);
  end;
end;

function TTileStorageETS.GetTileRectInfo(
  const ARect: TRect;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo
): ITileRectInfo;
var
  VLockedExclusively: Boolean;
  VResult: Byte;
  VVersionString: String;
  VObj: TGetTileRectCallbackInfo;
  VBufferIn: TETS_GET_TILE_RECT_IN;
begin
  VResult := ETS_RESULT_OK;
  FillChar(VObj, SizeOf(VObj), 0);
  VBufferIn.btTileZoom := 0;
  VBufferIn.dwOptionsIn := GetInitialExclusiveFlag(TRUE);

  // allocate
  with ARect do begin
    SetLength(
      VObj.InfoArray,
      (Right - Left) * (Bottom - Top)
    );
  end;

  if (nil<>FETS_GetTileRectInfo) then
  if (not MalfunctionDetected) then
  repeat
    // let us go
    DoBeginWork(VBufferIn.dwOptionsIn, VLockedExclusively);
    try
      if StorageStateInternal.ReadAccess <> asDisabled then begin
        // has access

        // initialize buffers
        if (0=VBufferIn.btTileZoom) then begin
          VBufferIn.ptTileRect := @ARect;
          VBufferIn.btTileZoom := AZoom+1; // zoom from 1
          VBufferIn.dwInfoMode := 0;
          // make flags
          VBufferIn.dwOptionsIn := VBufferIn.dwOptionsIn or (ETS_ROI_ANSI_CONTENTTYPE_IN or ETS_ROI_ANSI_CONTENTTYPE_OUT);
          // make version
          VVersionString := AVersionInfo.StoreString;
          VBufferIn.szVersionIn := PChar(VVersionString); // Pointer to VersionString with the same type of char
          if SizeOf(Char)=SizeOf(AnsiChar) then begin
            // AnsiString
            VBufferIn.dwOptionsIn := (VBufferIn.dwOptionsIn or ETS_ROI_ANSI_VERSION_IN or ETS_ROI_ANSI_VERSION_OUT);
          end;
        end;

        // request to storage
        VResult := TETS_GetTileRectInfo(FETS_GetTileRectInfo)(
          @FDLLProvHandle,
          @VObj,
          @VBufferIn);
      end else begin
        // no access
        break;
      end;
    finally
      DoEndWork(VLockedExclusively);
    end;

    // check response
    case VResult of
      ETS_RESULT_DISCONNECTED: begin
        // repeat exclusively
        SetUpExclusiveFlag(VBufferIn.dwOptionsIn);
      end;
      ETS_RESULT_NEED_EXCLUSIVE: begin
        // repeat exclusively
        if ExclusiveFlagWasSetUp(VBufferIn.dwOptionsIn) then
          break;
        SetUpExclusiveFlag(VBufferIn.dwOptionsIn);
      end;
      ETS_RESULT_OK: begin
        // success - output result object after break
        break;
      end;
      else begin
        // failed
        break;
      end;
    end;
  until FALSE;

  // make result
  Result := TTileRectInfoShort.CreateWithOwn(
    ARect,
    AZoom,
    AVersionInfo,
    FMainContentType,
    VObj.InfoArray
  );
  VObj.InfoArray := nil;
end;

function TTileStorageETS.InternalCreate_BinaryData(
  const AOptionsInp: LongWord;
  const ASelectBufferOut: PETS_SELECT_TILE_OUT
): IBinaryData;
begin
  // make binarydata for tile
  if ((AOptionsInp and ETS_ROI_SELECT_TILE_BODY)<>0) then
  if ((ASelectBufferOut^.dwOptionsOut and ETS_ROO_TILE_EXISTS)<>0) then
  if (ASelectBufferOut^.ptTileBuffer <> nil) then
  if (ASelectBufferOut^.dwTileSize > 0) then begin
    // make TileBody object
    Result := TBinaryDataByMemStream.CreateFromMem(ASelectBufferOut^.dwTileSize, ASelectBufferOut^.ptTileBuffer);
    Exit;
  end;

  Result := nil;
end;

function TTileStorageETS.InternalCreate_ContentType(
  const AContentTypeStr: Pointer;
  const AOptionsInp: LongWord;
  const AOptionsOut: LongWord
): IContentTypeInfoBasic;
var
  VResponseContentType: AnsiString;
begin
  // make contenttype for tile
  if ((AOptionsOut and ETS_ROO_TILE_EXISTS)<>0) then begin
    // tile
    if (nil=AContentTypeStr) then begin
      // no contenttype for tile
      Result := FMainContentType;
    end else begin
      // has contenttype
      if ((AOptionsInp and ETS_ROI_ANSI_CONTENTTYPE_OUT)<>0) then begin
        // contenttype is AnsiString
        VResponseContentType := AnsiString(PAnsiChar(AContentTypeStr));
      end else begin
        // contenttype is WideString
        VResponseContentType := WideString(PWideChar(AContentTypeStr));
      end;
      // make contenttype
      Result := FContentTypeManager.GetInfo(VResponseContentType);
    end;
  end else begin
    // tne or (neither tile nor tne)
    Result := nil;
  end;
end;

function TTileStorageETS.InternalCreate_TileInfoType(
  const AOptionsOut: LongWord
): TTileInfoType;
begin
  if ((AOptionsOut and ETS_ROO_TILE_EXISTS) <> 0) then begin
    Result := titExists;
  end else if ((AOptionsOut and ETS_ROO_TNE_EXISTS) <> 0) then begin
    Result := titTneExists;
  end else begin
    Result := titNotExists;
  end;
end;

function TTileStorageETS.InternalCreate_Version(
  const AVersionStr: Pointer;
  const AOptionsInp: LongWord;
  const AOptionsOut: LongWord
): IMapVersionInfo;
var
  VResponseVersion: String;
begin
  // make versions only for tile or tne
  if ((AOptionsOut and (ETS_ROO_TILE_EXISTS or ETS_ROO_TNE_EXISTS))<>0) then begin
    // tile or tne
    if (nil=AVersionStr) then begin
      // no version for tile or tne
      Result := FEmptyVersion;
    end else begin
      // has version
      if ((AOptionsInp and ETS_ROI_ANSI_VERSION_OUT)<>0) then begin
        // version is AnsiString
        VResponseVersion := AnsiString(PAnsiChar(AVersionStr));
      end else begin
        // version is WideString
        VResponseVersion := WideString(PWideChar(AVersionStr));
      end;
      // make version
      Result := MapVersionFactory.CreateByStoreString(VResponseVersion);
    end;
  end else begin
    // neither tile nor tne
    Result := nil;
  end;
end;

function TTileStorageETS.InternalLib_CheckInitialized: Boolean;
begin
  Result := (0 <> FDLLHandle) and (nil <> FDLLProvHandle) and
    // mandatory routines
    (nil <> FETS_SelectTile) and
    (nil <> FETS_InsertTile) and
    (nil <> FETS_DeleteTile) and
    (nil <> FETS_SetInformation);
  // FETS_Sync is OPTIONAL
  // FETS_InsertTNE and OPTIONAL (if storage supports TNE only with tiles)
  // FETS_EnumTileVersions is OPTIONAL (used for enum versions for single tile)
  // FETS_GetTileRectInfo is OPTIONAL (used by filling map)
  // FETS_MakeTileEnum and FETS_NextTileEnum and FETS_KillTileEnum is OPTIONAL (used by cache manager)
end;

function TTileStorageETS.InternalLib_CleanupProc: Boolean;
begin
  Result := FALSE;
  
  FETS_Sync       := nil;

  FETS_SelectTile := nil;
  FETS_InsertTile := nil;
  FETS_InsertTNE  := nil;
  FETS_DeleteTile := nil;

  FETS_SetInformation   := nil;
  FETS_EnumTileVersions := nil;
  FETS_GetTileRectInfo  := nil;

  FETS_MakeTileEnum := nil;
  FETS_NextTileEnum := nil;
  FETS_KillTileEnum := nil;

  FETS_ExecOption := nil;
  FETS_FreeMem    := nil;
end;

function TTileStorageETS.InternalLib_Complete: Boolean;
var
  p: Pointer;
  VPrimContentType: AnsiString;
begin
  Result := FALSE;
  if (0 <> FDLLHandle) then begin
    // notify provider about primary contenttype
    VPrimContentType := FMainContentType.GetContentType;
    TETS_SetInformation(FETS_SetInformation)(
      @FDLLProvHandle,
      Ord(ETS_INFOCLASS_SetPrimaryContentType),
      SizeOf(AnsiChar),
      PAnsiChar(VPrimContentType),
      nil
    );
    
    // complete initialization
    p := GetProcAddress(FDLLHandle, 'ETS_Complete');
    if (nil <> p) then begin
      TETS_Complete(p)(@FDLLProvHandle, 0);
      Inc(Result);
    end;
  end;
end;

function TTileStorageETS.InternalLib_Initialize: Boolean;
var
  VFunc: Pointer;
  VResult: Byte;
begin
  Result := FALSE;
  if (0 <> FDLLHandle) then begin
    // get init proc
    VFunc := GetProcAddress(FDLLHandle, 'ETS_Initialize');
    if (nil <> VFunc) then begin
      FETS_SERVICE_STORAGE_OPTIONS.Clear;
      VResult := TETS_Initialize(VFunc)(@FDLLProvHandle, @FETS_SERVICE_STORAGE_OPTIONS, 0, Pointer(Self));
      Result := (ETS_RESULT_OK=VResult);
    end;

    if Result then begin
      // set exif reader
      FETS_SetInformation := GetProcAddress(FDLLHandle, 'ETS_SetInformation');
      if (nil <> FETS_SetInformation) then begin
        // set callbacks
        TETS_SetInformation(FETS_SetInformation)(@FDLLProvHandle, Ord(ETS_INFOCLASS_SelectTile_Callback), 0, @Host_SelectTile_Callback, nil);
        TETS_SetInformation(FETS_SetInformation)(@FDLLProvHandle, Ord(ETS_INFOCLASS_EnumTileVersions_Callback), 0, @Host_EnumTileVersions_Callback, nil);
        TETS_SetInformation(FETS_SetInformation)(@FDLLProvHandle, Ord(ETS_INFOCLASS_GetTileRectInfo_Callback), 0, @Host_GetTileRectInfo_Callback, nil);
        TETS_SetInformation(FETS_SetInformation)(@FDLLProvHandle, Ord(ETS_INFOCLASS_NextTileEnum_Callback), 0, @Host_NextTileEnum_Callback, nil);
        TETS_SetInformation(FETS_SetInformation)(@FDLLProvHandle, Ord(ETS_INFOCLASS_SetVersion_Notifier), 0, @Host_SetVersion_Notifier, nil);
        // TODO: ETS_INFOCLASS_Reconnect_Notifier
        // TODO: ETS_INFOCLASS_Messages_Notifier
      end else begin
        // failed
        InternalLib_CleanupProc;
        Result := FALSE;
        Exit;
      end;

      // initialized - get other functions
      FETS_Sync := GetProcAddress(FDLLHandle, 'ETS_Sync');
      FETS_SelectTile := GetProcAddress(FDLLHandle, 'ETS_SelectTile');
      FETS_InsertTile := GetProcAddress(FDLLHandle, 'ETS_InsertTile');
      FETS_InsertTNE := GetProcAddress(FDLLHandle, 'ETS_InsertTNE');
      FETS_DeleteTile := GetProcAddress(FDLLHandle, 'ETS_DeleteTile');
      FETS_EnumTileVersions := GetProcAddress(FDLLHandle, 'ETS_EnumTileVersions');
      FETS_GetTileRectInfo := GetProcAddress(FDLLHandle, 'ETS_GetTileRectInfo');
      // enumtiles
      FETS_MakeTileEnum := GetProcAddress(FDLLHandle, 'ETS_MakeTileEnum');
      FETS_NextTileEnum := GetProcAddress(FDLLHandle, 'ETS_NextTileEnum');
      FETS_KillTileEnum := GetProcAddress(FDLLHandle, 'ETS_KillTileEnum');
      // execoption(s)
      FETS_ExecOption   := GetProcAddress(FDLLHandle, 'ETS_ExecOption');
      FETS_FreeMem      := GetProcAddress(FDLLHandle, 'ETS_FreeMem');
    end;
  end;
end;

function TTileStorageETS.InternalLib_NotifyStateChanged(const AEnabled: Boolean): Boolean;
var
  VReadAccess: TAccesState;
begin
  Result := FALSE;

  if AEnabled then begin
    VReadAccess := asEnabled;
  end else begin
    VReadAccess := asDisabled;
  end;

  StorageStateInternal.ReadAccess := VReadAccess;
end;

function TTileStorageETS.InternalLib_SetPath(const AGlobalStorageIdentifier, AServiceName: String): Boolean;
var
  VResult: Byte;
  VBufferIn: TETS_SET_IDENTIFIER_INFO;
  VOutput: LongWord;
begin
  Result := FALSE;
  try
    if (0 = FDLLHandle) then begin
      InternalLib_Initialize;
    end;
    if InternalLib_CheckInitialized then begin
      // set strings pointers
      VBufferIn.szGlobalStorageIdentifier := PChar(AGlobalStorageIdentifier); // PChar is correct!
      VBufferIn.szServiceName := PChar(AServiceName); // PChar is correct!
      VBufferIn.dwOptionsIn := 0;

      // set ansi flag
      if SizeOf(Char)=SizeOf(AnsiChar) then
        VBufferIn.dwOptionsIn := (VBufferIn.dwOptionsIn or ETS_ROI_ANSI_SET_INFORMATION);

      // call provider
      VResult := TETS_SetInformation(FETS_SetInformation)(
        @FDLLProvHandle,
        ETS_INFOCLASS_SetStorageIdentifier,
        SizeOf(VBufferIn),
        @VBufferIn,
        @VOutput
      );

      Result := (ETS_RESULT_OK=VResult);
    end;
  finally
    InternalLib_NotifyStateChanged(Result);
  end;
end;

function TTileStorageETS.InternalLib_Unload: Boolean;
var
  p: Pointer;
begin
  Result := FALSE;
  if (0 <> FDLLHandle) then begin
    // uninit
    p := GetProcAddress(FDLLHandle, 'ETS_Uninitialize');
    if (nil <> p) then begin
      TETS_Uninitialize(p)(@FDLLProvHandle, 0);
    end;

    // finishing
    Inc(Result);
    FreeLibrary(FDLLHandle);
    FDLLHandle := 0;
    InternalLib_CleanupProc;
    InternalLib_NotifyStateChanged(FALSE);
  end;
end;

function TTileStorageETS.InternalProviderSync(const AExclusiveFlag: LongWord): Byte;
var VLockedExclusively: Boolean;
begin
  DoBeginWork(AExclusiveFlag, VLockedExclusively);
  try
    Result := TETS_Sync(FETS_Sync)(@FDLLProvHandle, 0);
  finally
    DoEndWork(VLockedExclusively);
  end;
end;

procedure TTileStorageETS.InternalSaveTileOrTNE(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo;
  const ALoadDate: TDateTime;
  const AData: IBinaryData;
  const ARoutinePtr: Pointer;
  const ACallForTNE: Boolean
);
var
  VLockedExclusively: Boolean;
  VResult: Byte;
  VTileID: TTILE_ID_XYZ;
  VBufferIn: TETS_INSERT_TILE_IN;
  VVersionString: String;
  VTileInfo: ITileInfoBasic;
  VDeadConnectionFound: Boolean;
begin
  // check if no routine
  if (nil=ARoutinePtr) then
    raise EETSNoRoutine.Create(SAS_ERR_ETS_NotImplemented);
  // raise if error detected
  CheckMalfunction;

  VDeadConnectionFound := FALSE;
  VResult := ETS_RESULT_NEED_EXCLUSIVE; // any value <> ETS_RESULT_OK
  VTileID.z := 0; // initiali flag
  VBufferIn.dwOptionsIn := GetInitialExclusiveFlag(FALSE);
  repeat
    // let us go
    DoBeginWork(VBufferIn.dwOptionsIn, VLockedExclusively);
    try
      if StorageStateInternal.WriteAccess <> asDisabled then begin
        // allow insert\update - initialize buffers
        if (0=VTileID.z) then begin
          VTileID.xy := AXY;
          VTileID.z := AZoom+1; // zoom from 1
          VBufferIn.XYZ := @VTileID;
          // make flags
          VBufferIn.dwOptionsIn := VBufferIn.dwOptionsIn or ETS_ROI_ANSI_CONTENTTYPE_IN;
          // make version
          if (AVersionInfo<>nil) then begin
            VVersionString := AVersionInfo.StoreString;
            VBufferIn.szVersionIn := PChar(VVersionString); // Pointer to VersionString with the same type of char
          end else begin
            VBufferIn.szVersionIn := nil;
          end;
          if SizeOf(Char)=SizeOf(AnsiChar) then begin
            // AnsiString
            VBufferIn.dwOptionsIn := (VBufferIn.dwOptionsIn or ETS_ROI_ANSI_VERSION_IN);
          end;
          // no contenttype
          VBufferIn.szContentType := nil;
          // other fields
          VBufferIn.dtLoadedUTC := ALoadDate;
          if Assigned(AData) then begin
            VBufferIn.dwTileSize := AData.Size;
            VBufferIn.ptTileBuffer := AData.Buffer;
          end else begin
            VBufferIn.dwTileSize := 0;
            VBufferIn.ptTileBuffer := nil;
          end;
        end;

        // request to storage
        VResult := TETS_InsertTile(ARoutinePtr)(
          @FDLLProvHandle,
          @VBufferIn);
      end else begin
        // no access
        Exit;
      end;
    finally
      DoEndWork(VLockedExclusively);
    end;

    // check response
    case VResult of
      ETS_RESULT_DISCONNECTED: begin
        // соединение разорвано - попробуем переподключиться
        if (VDeadConnectionFound) then begin
          // уже пробовали ((
          raise EETSDeadConnection.Create(SAS_ERR_ETS_ConnectionIsDead);
        end;
        VDeadConnectionFound := TRUE;
        SetUpExclusiveFlag(VBufferIn.dwOptionsIn);
      end;
      ETS_RESULT_NEED_EXCLUSIVE: begin
        // repeat exclusively
        if ExclusiveFlagWasSetUp(VBufferIn.dwOptionsIn) then
          raise EETSCriticalError.Create(SAS_ERR_ETS_CriticalError);
        SetUpExclusiveFlag(VBufferIn.dwOptionsIn);
      end;
      ETS_RESULT_OK: begin
        // success
        // break to exit loop and write to cache
        break;
      end;
      // FAILED:
      ETS_RESULT_INI_SECTION_NOT_FOUND,
      ETS_RESULT_INI_FILE_NOT_FOUND,
      ETS_RESULT_NOT_CONNECTED: begin
        // cannot connect to server
        raise EETSCannotConnect.Create(SAS_ERR_ETS_CannotConnect);
      end;
      ETS_RESULT_INVALID_EXIF: begin
        // cannot parse exif or another source for tile version
        raise EETSCannotParseTile.Create(SAS_ERR_ETS_CannotParseTile);
      end;
      ETS_RESULT_NO_SPACE_AVAILABLE: begin
        // no space available
        raise EETSNoSpaceAvailable.Create(SAS_ERR_ETS_NoSpaceAvailable);
      end;
      ETS_RESULT_DATA_TRUNCATION: begin
        // data truncation on insert/update
        raise EETSDataTruncation.Create(SAS_ERR_ETS_DataTruncation);
      end;
      ETS_RESULT_TILE_TABLE_NOT_FOUND: begin
        raise EETSCantCreateTable.Create(SAS_ERR_ETS_CannotCreateTable);
      end;
      ETS_RESULT_READ_ONLY: begin
        raise EETSReadOnlyConnection.Create(SAS_ERR_ETS_ReadOnlyConnect);
      end;
      ETS_RESULT_UNKNOWN_EXEPTION: begin
        // very unknown exception
        raise EETSUnknownError.Create(SAS_ERR_ETS_UnknownError);
      end;
      else begin
        // some unusual
        raise EETSFailed.Create(SAS_ERR_ETS_FailToSaveTile + IntToStr(VResult));
      end;
    end;
  until FALSE;

  if (ETS_RESULT_OK=VResult) then begin
    if Assigned(FTileInfoMemCache) then begin
      if ACallForTNE then begin
        // write TNE to cache
        VTileInfo := TTileInfoBasicTNE.Create(
          VBufferIn.dtLoadedUTC,
          AVersionInfo
        );
      end else begin
        // write tile to cache
        VTileInfo := TTileInfoBasicExistsWithTile.Create(
          VBufferIn.dtLoadedUTC,
          AData,
          AVersionInfo,
          FMainContentType
        );
      end;
      FTileInfoMemCache.Add(
        AXY,
        AZoom,
        AVersionInfo,
        VTileInfo
      );
    end;
    FETSTTLListener.CheckUseTimeUpdated;
    NotifyTileUpdate(AXY, AZoom, AVersionInfo);
  end;
end;

function TTileStorageETS.MalfunctionDetected: Boolean;
begin
  Result := not (FETS_SERVICE_STORAGE_OPTIONS.malfunction_mode in [ETS_PMM_HAS_COMPLETED,ETS_PMM_ESTABLISHED]);
end;

procedure TTileStorageETS.SaveTile(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo;
  const ALoadDate: TDateTime;
  const AContentType: IContentTypeInfoBasic;
  const AData: IBinaryData
);
begin
  InternalSaveTileOrTNE(AXY, AZoom, AVersionInfo, ALoadDate, AData, FETS_InsertTile, FALSE);
end;

procedure TTileStorageETS.SaveTNE(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo;
  const ALoadDate: TDateTime
);
begin
  InternalSaveTileOrTNE(AXY, AZoom, AVersionInfo, ALoadDate, nil, FETS_InsertTNE, TRUE);
end;

function TTileStorageETS.ScanTiles(const AIgnoreTNE: Boolean): IEnumTileInfo;
begin
  if (FETS_MakeTileEnum<>nil) and
     (FETS_NextTileEnum<>nil) and
     (FETS_KillTileEnum<>nil) and
     (not MalfunctionDetected) and
     (FETS_SERVICE_STORAGE_OPTIONS.scan_tiles_mode<>ETS_STM_NOT) then begin
    // allow to create enumerator in storage provider
    Result := TEnumTileInfoByETS.Create(
      AIgnoreTNE,
      Self
    );
  end else begin
    // not available
    Result := nil;
  end;
end;

procedure TTileStorageETS.SetMapVersionConfig(const AMapVersionConfig: IMapVersionConfig);
begin
  FMapVersionConfig := AMapVersionConfig;
end;

{ TTileStorageDBMS }

function TTileStorageDBMS.InternalLib_Initialize: Boolean;
begin
  if (0 = FDLLHandle) then begin
    FDLLHandle := LoadLibrary('TileStorage_DBMS.dll');
  end;

  // common routines
  Result := inherited InternalLib_Initialize;
  (*
  if Result then begin
    // special routines
  end;
  *)
end;

{ TEnumTileInfoByETS }

function TEnumTileInfoByETS.CallbackLib_NextTileEnum(
  const ACallbackPointer: Pointer;
  const ANextBufferInp: PETS_GET_TILE_RECT_IN;
  const ANextBufferOut: PETS_NEXT_TILE_ENUM_OUT
): Byte;
begin
  Result := ETS_RESULT_OK;
  
  // ACallbackPointer is PTileInfo
  with PTileInfo(ACallbackPointer)^ do begin
    // base values
    FTile := ANextBufferOut^.TileFull^.xy;
    FZoom := ANextBufferOut^.TileFull^.z;
    FLoadDate := ANextBufferOut^.TileInfo.dtLoadedUTC;
    FSize := ANextBufferOut^.TileInfo.dwTileSize;

    // check flags
    FInfoType := FStorage.InternalCreate_TileInfoType(ANextBufferOut^.TileInfo.dwOptionsOut);

    // FVersionInfo
    FVersionInfo := FStorage.InternalCreate_Version(
      ANextBufferOut^.TileInfo.szVersionOut,
      ANextBufferInp^.dwOptionsIn,
      ANextBufferOut^.TileInfo.dwOptionsOut
    );

    // FContentType
    FContentType := FStorage.InternalCreate_ContentType(
      ANextBufferOut^.TileInfo.szContentTypeOut,
      ANextBufferInp^.dwOptionsIn,
      ANextBufferOut^.TileInfo.dwOptionsOut
    );

    // FData
    FData := FStorage.InternalCreate_BinaryData(
      ETS_ROI_SELECT_TILE_BODY, // always create tile body!
      @(ANextBufferOut^.TileInfo)
    );
  end;
end;

constructor TEnumTileInfoByETS.Create(
  const AIgnoreTNE: Boolean;
  const AStorage: TTileStorageETS
);
var VLockedExclusively: Boolean;
begin
  inherited Create;
  FIgnoreTNE := AIgnoreTNE;
  FStorage := AStorage;

  // init
  FETSEnumTilesHandle := nil;
  FillChar(FETSAllEnumInfo, sizeof(FETSAllEnumInfo), 0);

  // init options
  FETSAllEnumInfo.dwOptionsIn := FStorage.GetInitialExclusiveFlag(TRUE);

  // set options
  FETSAllEnumInfo.dwOptionsIn := FETSAllEnumInfo.dwOptionsIn or (ETS_ROI_ANSI_CONTENTTYPE_IN or ETS_ROI_ANSI_CONTENTTYPE_OUT);

  if (sizeof(Char)=sizeof(AnsiChar)) then begin
    FETSAllEnumInfo.dwOptionsIn := (FETSAllEnumInfo.dwOptionsIn or ETS_ROI_ANSI_VERSION_IN or ETS_ROI_ANSI_VERSION_OUT);
  end;

  if AIgnoreTNE then begin
    FETSAllEnumInfo.dwOptionsIn := (FETSAllEnumInfo.dwOptionsIn or ETS_ROI_SELECT_TILE_BODY);
  end;

  // make
  FStorage.DoBeginWork(FETSAllEnumInfo.dwOptionsIn, VLockedExclusively);
  try
    FETSResult := InternalMake;
  finally
    FStorage.DoEndWork(VLockedExclusively);
  end;
end;

destructor TEnumTileInfoByETS.Destroy;
var VDummyLocked: Boolean;
begin
  FStorage.DoBeginWork(ETS_ROI_EXCLUSIVELY, VDummyLocked);
  try
    InternalKill;
  finally
    FStorage.DoEndWork(VDummyLocked);
  end;
  FStorage := nil;
  inherited;
end;

function TEnumTileInfoByETS.InternalKill: Byte;
begin
  Result := TETS_KillTileEnum(FStorage.FETS_KillTileEnum)(
    @FETSEnumTilesHandle,
    0
  );
end;

function TEnumTileInfoByETS.InternalMake: Byte;
begin
  Result := TETS_MakeTileEnum(FStorage.FETS_MakeTileEnum)(
    @(FStorage.FDLLProvHandle),
    @(FETSEnumTilesHandle),
    0,
    Self
  );
end;

function TEnumTileInfoByETS.Next(var ATileInfo: TTileInfo): Boolean;
var
  VLockedExclusively: Boolean;
begin
  Result := FALSE;

  // check avail
  if (FETSResult<>ETS_RESULT_OK) or (nil=FETSEnumTilesHandle) then
    Exit;

  // call provider
  FStorage.DoBeginWork(FETSAllEnumInfo.dwOptionsIn, VLockedExclusively);
  try
    FETSResult := TETS_NextTileEnum(FStorage.FETS_NextTileEnum)(
      @FETSEnumTilesHandle,
      @ATileInfo,
      @FETSAllEnumInfo
    );

    // check result
    Result := (ETS_RESULT_OK = FETSResult);

    if Result then begin
      // в сасе зум на 1 меньше
      Dec(ATileInfo.FZoom);
    end;
  finally
    FStorage.DoEndWork(VLockedExclusively);
  end;
end;

end.
