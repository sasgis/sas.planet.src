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
  i_SimpleTileStorageConfig,
  i_MapVersionInfo,
  i_MapVersionConfig,
  i_ContentTypeInfo,
  i_TileInfoBasic,
  i_TileStorage,
  i_CoordConverter,
  i_ContentTypeManager,
  i_NotifierTTLCheck,
  i_ListenerTTLCheck,
  u_GlobalCahceConfig,
  u_TileStorageAbstract,
  u_TileInfoBasicMemCache,
  t_ETS_Tiles,
  t_ETS_Provider;

type
  TTileStorageETS = class(TTileStorageAbstract)
  // base interface
  private
    FMainContentType: IContentTypeInfoBasic;
    FContentTypeManager: IContentTypeManager;
    FGCList: INotifierTTLCheck;
    FETSTTLListener: IListenerTTLCheck;
    FMemCacheTTLListener: IListenerTTLCheck;
    FTileInfoMemCache: TTileInfoBasicMemCache;
    FUseMemCache: Boolean;

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
    FETS_DeleteTNE: Pointer;  // TETS_DeleteTile (OPTIONAL)
    FETS_SetInformation: Pointer;   // TETS_SetInformation
    FETS_EnumTileVersions: Pointer; // TETS_EnumTileVersions (OPTIONAL)
    FETS_GetTileRectInfo: Pointer;  // TETS_GetTileRectInfo (OPTIONAL)
    FETS_MakeTileEnum: Pointer;     // TETS_MakeTileEnum (OPTIONAL)
    FETS_NextTileEnum: Pointer;     // TETS_NextTileEnum (OPTIONAL)
    FETS_KillTileEnum: Pointer;     // TETS_KillTileEnum (OPTIONAL)

    // shared buffer
    FETS_STATIC_BUFFER: TETS_STATIC_BUFFER;

    // sync mem cache routine
    procedure DoTTLSync(Sender: TObject);
    // sync provider routine
    procedure DoProviderSync(Sender: TObject);
  private
    function InternalLib_CleanupProc: Boolean; virtual;
    function InternalLib_Initialize: Boolean; virtual;
    function InternalLib_CheckInitialized: Boolean; virtual;
    function InternalLib_Unload: Boolean; virtual;
    function InternalLib_NotifyStateChanged(const AEnabled: Boolean): Boolean;
    function InternalLib_SetPath(const AGlobalStorageIdentifier, AServiceName: String): Boolean;
    function InternalLib_Complete: Boolean;

    // sync routines
    procedure DoBeginWork(const AExclusively: Boolean);
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

    function NowUTC: TDateTime;

    // delete tile or TNE
    function InternalDeleteTileOrTNE(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo;
      const ARoutinePtr: Pointer;
      const ACallForTNE: Boolean
    ): Boolean;

    procedure InternalSaveTileOrTNE(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo;
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


  protected
    // base storage interface
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

    function DeleteTNE(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): Boolean; override;

    procedure SaveTile(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo;
      const AData: IBinaryData
    ); override;

    procedure SaveTNE(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo
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
      const AGCList: INotifierTTLCheck;
      const AUseMemCache: Boolean;
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

implementation

uses
  t_CommonTypes,
  vsagps_public_sysutils,
  u_BinaryDataByMemStream,
  u_MapVersionFactorySimpleString,
  u_MapVersionInfo,
  u_MapVersionListStatic,
  u_Synchronizer,
  u_ListenerTTLCheck,
  u_TileStorageTypeAbilities,
  u_TileRectInfoShort,
  u_TileInfoBasic;

type
  PTileInfo = ^TTileInfo;

function TileInRectToIndex(const ATileRect: PRect; const ATile: TPoint): Integer;
begin
  if (ATile.X < ATileRect^.Left) or (ATile.X >= ATileRect^.Right) then begin
    Result := -1;
  end else if (ATile.Y < ATileRect^.Top) or (ATile.Y >= ATileRect^.Bottom) then begin
    Result := -1;
  end else begin
    Result :=
      (ATile.X - ATileRect^.Left) +
      (ATile.Y - ATileRect^.Top) * (ATileRect^.Right - ATileRect^.Left);
  end;
end;

type
  TEnumTileInfoByETS = class(TInterfacedObject, IEnumTileInfo)
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
  end;
  PSelectTileCallbackInfo = ^TSelectTileCallbackInfo;

  TEnumTileVersionsCallbackInfo = packed record
    TileVersionsList: IInterfaceList;
  end;
  PEnumTileVersionsCallbackInfo = ^TEnumTileVersionsCallbackInfo;

  TGetTileRectCallbackInfo = packed record
    TileCount: TPoint;
    InfoCount: Integer;
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
  if (nil<>TileVersionsList) then begin
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
  with PGetTileRectCallbackInfo(ACallbackPointer)^ do begin
    if (0=InfoCount) then begin
      with ATileRectInfoInp^.ptTileRect^ do begin
        TileCount.X := Right - Left;
        TileCount.Y := Bottom - Top;
      end;
      InfoCount := TileCount.X * TileCount.Y;
      SetLength(InfoArray, InfoCount);
    end;
  end;

  // add info from tile
  VIndex := TileInRectToIndex(ATileRectInfoInp^.ptTileRect, ATileRectInfoOut^.TilePos);
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

constructor TTileStorageETS.Create(
  const AGeoConverter: ICoordConverter;
  const AGlobalStorageIdentifier, AStoragePath: String;
  const AGCList: INotifierTTLCheck;
  const AUseMemCache: Boolean;
  const AContentTypeManager: IContentTypeManager;
  const AMapVersionFactory: IMapVersionFactory;
  const AMainContentType: IContentTypeInfoBasic);
const
  CETSSync = 300000; // 5 min
  CETSSyncCheckInterval = 60000; // 60 sec
begin
  inherited Create(
    TTileStorageTypeAbilitiesDBMS.Create,
    AMapVersionFactory,
    AGeoConverter,
    AStoragePath
  );

  FETS_STATIC_BUFFER.Clear;

  FDLLSync := MakeSyncRW_Big(Self);

  FContentTypeManager := AContentTypeManager;
  FMainContentType := AMainContentType;

  FUseMemCache := AUseMemCache;

  FTileNotExistsTileInfo := TTileInfoBasicNotExists.Create(0, nil);
  FEmptyVersion := MapVersionFactory.CreateByStoreString('');

  FETSTTLListener := TListenerTTLCheck.Create(
    DoProviderSync,
    CETSSync,
    CETSSyncCheckInterval
  );

  FMemCacheTTLListener := TListenerTTLCheck.Create(
    Self.DoTTLSync,
    CETSSync,
    CETSSyncCheckInterval
  );

  FGCList := AGCList;
  FGCList.Add(FETSTTLListener);
  FGCList.Add(FMemCacheTTLListener);

  FTileInfoMemCache := TTileInfoBasicMemCache.Create(100, 30000);

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
begin
  Result := InternalDeleteTileOrTNE(AXY, AZoom, AVersionInfo, FETS_DeleteTile, FALSE);
end;

function TTileStorageETS.DeleteTNE(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo
): Boolean;
begin
  Result := InternalDeleteTileOrTNE(AXY, AZoom, AVersionInfo, FETS_DeleteTNE, TRUE);
end;

destructor TTileStorageETS.Destroy;
begin
  StorageStateInternal.ReadAccess := asDisabled;

  DoBeginWork(TRUE);
  try
    InternalLib_Unload;

    if Assigned(FGCList) then begin
      FGCList.Remove(FMemCacheTTLListener);
      FGCList.Remove(FETSTTLListener);
      FGCList := nil;
    end;

    FETSTTLListener := nil;
    FMemCacheTTLListener := nil;

    FreeAndNil(FTileInfoMemCache);

    FMainContentType := nil;
    FContentTypeManager := nil;
    FTileNotExistsTileInfo := nil;
    FEmptyVersion := nil;
  finally
    DoEndWork(TRUE);
  end;

  inherited;

  FDLLSync := nil;
end;

procedure TTileStorageETS.DoBeginWork(const AExclusively: Boolean);
begin
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

procedure TTileStorageETS.DoProviderSync(Sender: TObject);
var
  VResult: Byte;
begin
  if (nil=FETS_Sync) then
    Exit;

  DoBeginWork(FALSE);
  try
    // call
    VResult := TETS_Sync(FETS_Sync)(@FDLLProvHandle, 0);
  finally
    DoEndWork(FALSE);
  end;

  if (ETS_RESULT_NEED_EXCLUSIVE=VResult) then begin
    DoBeginWork(TRUE);
    try
      // call
      TETS_Sync(FETS_Sync)(@FDLLProvHandle, ETS_ROI_EXCLUSIVELY);
    finally
      DoEndWork(TRUE);
    end;
  end;
end;

procedure TTileStorageETS.DoTTLSync(Sender: TObject);
begin
  FTileInfoMemCache.ClearByTTL;
end;

function TTileStorageETS.GetListOfTileVersions(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo
): IMapVersionListStatic;
var
  VStep: Byte;
  VResult: Byte;
  VVersionString: String;
  VObj: TEnumTileVersionsCallbackInfo;
  VTileID: TTILE_ID_XYZ;
  VBufferIn: TETS_SELECT_TILE_IN;
begin
  VResult := ETS_RESULT_OK;
  FillChar(VObj, SizeOf(VObj), 0);
  VStep := 0;
  if (nil<>FETS_EnumTileVersions) then
  repeat
    // let us go
    DoBeginWork((VStep>0));
    try
      if StorageStateInternal.ReadAccess <> asDisabled then begin
        // has access

        // initialize buffers
        if (0=VStep) then begin
          VTileID.xy := AXY;
          VTileID.z := AZoom;
          VBufferIn.XYZ := @VTileID;
          // make flags
          VBufferIn.dwOptionsIn := (ETS_ROI_ANSI_CONTENTTYPE_IN or ETS_ROI_ANSI_CONTENTTYPE_OUT);
          // make version
          VVersionString := AVersionInfo.StoreString;
          VBufferIn.szVersionIn := PChar(VVersionString); // Pointer to VersionString with the same type of char
          if SizeOf(Char)=SizeOf(AnsiChar) then begin
            // AnsiString
            VBufferIn.dwOptionsIn := (VBufferIn.dwOptionsIn or ETS_ROI_ANSI_VERSION_IN or ETS_ROI_ANSI_VERSION_OUT);
          end;
        end else begin
          // exclusively
          VBufferIn.dwOptionsIn := (VBufferIn.dwOptionsIn or ETS_ROI_EXCLUSIVELY);
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
      DoEndWork((VStep>0));
    end;

    // check response
    case VResult of
      ETS_RESULT_NEED_EXCLUSIVE: begin
        // repeat exclusively
        Inc(VStep);
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
  Result := 'x' + IntToStr(AXY.X) + PathDelim +
            'y' + IntToStr(AXY.Y) + FMainContentType.GetDefaultExt;
end;

function TTileStorageETS.GetTileInfo(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo;
  const AMode: TGetTileInfoMode
): ITileInfoBasic;
var
  VStep: Byte;
  VResult: Byte;
  VVersionString: String;
  VObj: TSelectTileCallbackInfo;
  VTileID: TTILE_ID_XYZ;
  VBufferIn: TETS_SELECT_TILE_IN;
begin
  // try to read from cache
  if FUseMemCache then begin
    Result := FTileInfoMemCache.Get(AXY, AZoom);
    if Result <> nil then begin
      Exit;
    end;
  end;

  Result := FTileNotExistsTileInfo;

  VResult := ETS_RESULT_OK;
  FillChar(VObj, SizeOf(VObj), 0);
  VStep := 0;
  repeat
    // let us go
    DoBeginWork((VStep>0));
    try
      if StorageStateInternal.ReadAccess <> asDisabled then begin
        // has access

        // initialize buffers
        if (0=VStep) then begin
          VTileID.xy := AXY;
          VTileID.z := AZoom;
          VBufferIn.XYZ := @VTileID;
          // make flags (all except ETS_STI_CHECK_EXISTS, ContentType is AnsiString)
          VBufferIn.dwOptionsIn := (ETS_ROI_ANSI_CONTENTTYPE_IN or ETS_ROI_ANSI_CONTENTTYPE_OUT);
          if (AMode<>gtimWithoutData) then begin
            VBufferIn.dwOptionsIn := (VBufferIn.dwOptionsIn or ETS_ROI_SELECT_TILE_BODY);
          end;
          // make version
          VVersionString := AVersionInfo.StoreString;
          VBufferIn.szVersionIn := PChar(VVersionString); // Pointer to VersionString with the same type of char
          if SizeOf(Char)=SizeOf(AnsiChar) then begin
            // AnsiString
            VBufferIn.dwOptionsIn := (VBufferIn.dwOptionsIn or ETS_ROI_ANSI_VERSION_IN or ETS_ROI_ANSI_VERSION_OUT);
          end;
        end else begin
          // exclusively
          VBufferIn.dwOptionsIn := (VBufferIn.dwOptionsIn or ETS_ROI_EXCLUSIVELY);
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
      DoEndWork((VStep>0));
    end;

    // check response
    case VResult of
      ETS_RESULT_NEED_EXCLUSIVE: begin
        // repeat exclusively
        Inc(VStep);
      end;
      ETS_RESULT_OK: begin
        // success - output result object
        Result := VObj.TileResult;
        // break to exit loop and write to cache
        break;
      end;
      else begin
        // failed
        Exit;
      end;
    end;
  until FALSE;

  // write to cache
  if FUseMemCache then begin
    FTileInfoMemCache.Add(AXY, AZoom, AVersionInfo, Result);
  end;
end;

function TTileStorageETS.GetTileRectInfo(
  const ARect: TRect;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo
): ITileRectInfo;
var
  VStep: Byte;
  VResult: Byte;
  VVersionString: String;
  VObj: TGetTileRectCallbackInfo;
  VBufferIn: TETS_GET_TILE_RECT_IN;
begin
  VResult := ETS_RESULT_OK;
  FillChar(VObj, SizeOf(VObj), 0);
  VStep := 0;
  if (nil<>FETS_GetTileRectInfo) then
  repeat
    // let us go
    DoBeginWork((VStep>0));
    try
      if StorageStateInternal.ReadAccess <> asDisabled then begin
        // has access

        // initialize buffers
        if (0=VStep) then begin
          VBufferIn.ptTileRect := @ARect;
          VBufferIn.btTileZoom := AZoom;
          VBufferIn.dwInfoMode := 0;
          // make flags
          VBufferIn.dwOptionsIn := (ETS_ROI_ANSI_CONTENTTYPE_IN or ETS_ROI_ANSI_CONTENTTYPE_OUT);
          // make version
          VVersionString := AVersionInfo.StoreString;
          VBufferIn.szVersionIn := PChar(VVersionString); // Pointer to VersionString with the same type of char
          if SizeOf(Char)=SizeOf(AnsiChar) then begin
            // AnsiString
            VBufferIn.dwOptionsIn := (VBufferIn.dwOptionsIn or ETS_ROI_ANSI_VERSION_IN or ETS_ROI_ANSI_VERSION_OUT);
          end;
        end else begin
          // exclusively
          VBufferIn.dwOptionsIn := (VBufferIn.dwOptionsIn or ETS_ROI_EXCLUSIVELY);
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
      DoEndWork((VStep>0));
    end;

    // check response
    case VResult of
      ETS_RESULT_NEED_EXCLUSIVE: begin
        // repeat exclusively
        Inc(VStep);
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

function TTileStorageETS.InternalDeleteTileOrTNE(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo;
  const ARoutinePtr: Pointer;
  const ACallForTNE: Boolean
): Boolean;
var
  VStep: Byte;
  VResult: Byte;
  VTileID: TTILE_ID_XYZ;
  VBufferIn: TETS_DELETE_TILE_IN;
  VVersionString: String;
begin
  Result := FALSE;
  // check if no routine
  if (nil=ARoutinePtr) then
    Exit;

  VResult := ETS_RESULT_NEED_EXCLUSIVE; // any value <> ETS_RESULT_OK
  VStep := 0;
  repeat
    // let us go
    DoBeginWork((VStep>0));
    try
      if StorageStateInternal.DeleteAccess <> asDisabled then begin
        // allow delete - initialize buffers
        if (0=VStep) then begin
          VTileID.xy := AXY;
          VTileID.z := AZoom;
          VBufferIn.XYZ := @VTileID;
          // make flags
          VBufferIn.dwOptionsIn := 0;
          // make version
          VVersionString := AVersionInfo.StoreString;
          VBufferIn.szVersionIn := PChar(VVersionString); // Pointer to VersionString with the same type of char
          if SizeOf(Char)=SizeOf(AnsiChar) then begin
            // AnsiString
            VBufferIn.dwOptionsIn := (VBufferIn.dwOptionsIn or ETS_ROI_ANSI_VERSION_IN);
          end;
        end else begin
          // exclusively
          VBufferIn.dwOptionsIn := (VBufferIn.dwOptionsIn or ETS_ROI_EXCLUSIVELY);
        end;

        // request to storage
        VResult := TETS_DeleteTile(ARoutinePtr)(
          @FDLLProvHandle,
          @VBufferIn);
      end else begin
        // no access
        Exit;
      end;
    finally
      DoEndWork((VStep>0));
    end;

    // check response
    case VResult of
      ETS_RESULT_NEED_EXCLUSIVE: begin
        // repeat exclusively
        Inc(VStep);
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
    if FUseMemCache then begin
      // delete both tile and TNE
      FTileInfoMemCache.Remove(AXY, AZoom);
    end;
    NotifyTileUpdate(AXY, AZoom, AVersionInfo);
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
  // FETS_InsertTNE and FETS_DeleteTNE are OPTIONAL (if storage supports TNE only with tiles)
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
  FETS_DeleteTNE  := nil;

  FETS_SetInformation   := nil;
  FETS_EnumTileVersions := nil;
  FETS_GetTileRectInfo  := nil;

  FETS_MakeTileEnum := nil;
  FETS_NextTileEnum := nil;
  FETS_KillTileEnum := nil;
end;

function TTileStorageETS.InternalLib_Complete: Boolean;
var
  p: Pointer;
begin
  Result := FALSE;
  if (0 <> FDLLHandle) then begin
    // uninit
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
      FETS_STATIC_BUFFER.Clear;
      VResult := TETS_Initialize(VFunc)(@FDLLProvHandle, @FETS_STATIC_BUFFER, 0, Pointer(Self));
      Result := (ETS_RESULT_OK=VResult);
    end;

    if Result then begin
      // set exif reader
      FETS_SetInformation := GetProcAddress(FDLLHandle, 'ETS_SetInformation');
      if (nil <> FETS_SetInformation) then begin
        // set callbacks
        TETS_SetInformation(FETS_SetInformation)(@FDLLProvHandle, ETS_INFOCLASS_SelectTile_Callback, 0, @Host_SelectTile_Callback, nil);
        TETS_SetInformation(FETS_SetInformation)(@FDLLProvHandle, ETS_INFOCLASS_EnumTileVersions_Callback, 0, @Host_EnumTileVersions_Callback, nil);
        TETS_SetInformation(FETS_SetInformation)(@FDLLProvHandle, ETS_INFOCLASS_GetTileRectInfo_Callback, 0, @Host_GetTileRectInfo_Callback, nil);
        TETS_SetInformation(FETS_SetInformation)(@FDLLProvHandle, ETS_INFOCLASS_NextTileEnum_Callback, 0, @Host_NextTileEnum_Callback, nil);
        // TODO: ETS_INFOCLASS_Disconnect_Notifier
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
      FETS_DeleteTNE := GetProcAddress(FDLLHandle, 'ETS_DeleteTNE');
      FETS_EnumTileVersions := GetProcAddress(FDLLHandle, 'ETS_EnumTileVersions');
      FETS_GetTileRectInfo := GetProcAddress(FDLLHandle, 'ETS_GetTileRectInfo');
      // enumtiles
      FETS_MakeTileEnum := GetProcAddress(FDLLHandle, 'ETS_MakeTileEnum');
      FETS_NextTileEnum := GetProcAddress(FDLLHandle, 'ETS_NextTileEnum');
      FETS_KillTileEnum := GetProcAddress(FDLLHandle, 'ETS_KillTileEnum');
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

procedure TTileStorageETS.InternalSaveTileOrTNE(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo;
  const AData: IBinaryData;
  const ARoutinePtr: Pointer;
  const ACallForTNE: Boolean
);
var
  VStep: Byte;
  VResult: Byte;
  VTileID: TTILE_ID_XYZ;
  VBufferIn: TETS_INSERT_TILE_IN;
  VVersionString: String;
  VTileInfo: ITileInfoBasic;
begin
  // check if no routine
  if (nil=ARoutinePtr) then
    Exit;

  VResult := ETS_RESULT_NEED_EXCLUSIVE; // any value <> ETS_RESULT_OK

  VStep := 0;
  repeat
    // let us go
    DoBeginWork((VStep>0));
    try
      if StorageStateInternal.WriteAccess <> asDisabled then begin
        // allow insert\update - initialize buffers
        if (0=VStep) then begin
          VTileID.xy := AXY;
          VTileID.z := AZoom;
          VBufferIn.XYZ := @VTileID;
          // make flags
          VBufferIn.dwOptionsIn := ETS_ROI_ANSI_CONTENTTYPE_IN;
          // make version
          VVersionString := AVersionInfo.StoreString;
          VBufferIn.szVersionIn := PChar(VVersionString); // Pointer to VersionString with the same type of char
          if SizeOf(Char)=SizeOf(AnsiChar) then begin
            // AnsiString
            VBufferIn.dwOptionsIn := (VBufferIn.dwOptionsIn or ETS_ROI_ANSI_VERSION_IN);
          end;
          // no contenttype
          VBufferIn.szContentType := nil;
          // other fields
          VBufferIn.dtLoadedUTC := NowUTC;
          if Assigned(AData) then begin
            VBufferIn.dwTileSize := AData.Size;
            VBufferIn.ptTileBuffer := AData.Buffer;
          end else begin
            VBufferIn.dwTileSize := 0;
            VBufferIn.ptTileBuffer := nil;
          end;
        end else begin
          // exclusively
          VBufferIn.dwOptionsIn := (VBufferIn.dwOptionsIn or ETS_ROI_EXCLUSIVELY);
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
      DoEndWork((VStep>0));
    end;

    // check response
    case VResult of
      ETS_RESULT_NEED_EXCLUSIVE: begin
        // repeat exclusively
        Inc(VStep);
      end;
      ETS_RESULT_OK: begin
        // success
        // break to exit loop and write to cache
        break;
      end;
      else begin
        // failed
        Exit;
      end;
    end;
  until FALSE;

  if (ETS_RESULT_OK=VResult) then begin
    if FUseMemCache then begin
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
    NotifyTileUpdate(AXY, AZoom, AVersionInfo);
  end;
end;

function TTileStorageETS.NowUTC: TDateTime;
var st: TSystemTime;
begin
  GetSystemTime(st);
  Result := SystemTimeToDateTime(st);
end;

procedure TTileStorageETS.SaveTile(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo;
  const AData: IBinaryData
);
begin
  InternalSaveTileOrTNE(AXY, AZoom, AVersionInfo, AData, FETS_InsertTile, FALSE);
end;

procedure TTileStorageETS.SaveTNE(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo
);
begin
  InternalSaveTileOrTNE(AXY, AZoom, AVersionInfo, nil, FETS_InsertTNE, TRUE);
end;

function TTileStorageETS.ScanTiles(const AIgnoreTNE: Boolean): IEnumTileInfo;
begin
  if (FETS_MakeTileEnum<>nil) and (FETS_NextTileEnum<>nil) and (FETS_KillTileEnum<>nil) then begin
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
begin
  FIgnoreTNE := AIgnoreTNE;
  FStorage := AStorage;

  // init
  FETSEnumTilesHandle := nil;
  FillChar(FETSAllEnumInfo, sizeof(FETSAllEnumInfo), 0);

  // set options
  FETSAllEnumInfo.dwOptionsIn := (ETS_ROI_ANSI_CONTENTTYPE_IN or ETS_ROI_ANSI_CONTENTTYPE_OUT);

  if (sizeof(Char)=sizeof(AnsiChar)) then begin
    FETSAllEnumInfo.dwOptionsIn := (FETSAllEnumInfo.dwOptionsIn or ETS_ROI_ANSI_VERSION_IN or ETS_ROI_ANSI_VERSION_OUT);
  end;

  if AIgnoreTNE then begin
    FETSAllEnumInfo.dwOptionsIn := (FETSAllEnumInfo.dwOptionsIn or ETS_ROI_SELECT_TILE_BODY);
  end;

  // make
  FStorage.DoBeginWork(TRUE);
  try
    FETSResult := InternalMake;
  finally
    FStorage.DoEndWork(TRUE);
  end;
end;

destructor TEnumTileInfoByETS.Destroy;
begin
  FStorage.DoBeginWork(TRUE);
  try
    InternalKill;
  finally
    FStorage.DoEndWork(TRUE);
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
begin
  Result := FALSE;

  // check avail
  if (FETSResult<>ETS_RESULT_OK) or (nil=FETSEnumTilesHandle) then
    Exit;

  // call provider
  FStorage.DoBeginWork(FALSE);
  try
    FETSResult := TETS_NextTileEnum(FStorage.FETS_NextTileEnum)(
      @FETSEnumTilesHandle,
      @ATileInfo,
      @FETSAllEnumInfo
    );
  finally
    FStorage.DoEndWork(FALSE);
  end;

  // check result
  Result := (ETS_RESULT_OK = FETSResult);
end;

end.
