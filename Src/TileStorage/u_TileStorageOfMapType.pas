{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_TileStorageOfMapType;

interface

uses
  Types,
  SysUtils,
  i_SimpleTileStorageConfig,
  i_TileStorage,
  i_ContentTypeManager,
  i_InternalPerformanceCounter,
  i_NotifierTilePyramidUpdate,
  i_StorageState,
  i_CoordConverter,
  i_CoordConverterFactory,
  i_ContentTypeInfo,
  i_Listener,
  i_MapVersionInfo,
  i_MapVersionRequest,
  i_MapVersionListStatic,
  i_TileInfoBasic,
  i_SimpleFlag,
  i_PathConfig,
  i_StorageStateProxy,
  i_BinaryData,
  i_TileInfoBasicMemCache,
  i_TileStorageAbilities,
  i_TileStorageTypeList,
  i_GlobalCacheConfig,
  u_BaseInterfacedObject;

type
  TTileStorageOfMapType = class(TBaseInterfacedObject, ITileStorage)
  private
    FGlobalCacheConfig: IGlobalCacheConfig;
    FCoordConverter: ICoordConverter;
    FTileStorageTypeList: ITileStorageTypeListStatic;
    FConfig: ISimpleTileStorageConfig;
    FContentTypeManager: IContentTypeManager;
    FCacheTileInfo: ITileInfoBasicMemCache;
    FActualPath: IPathConfig;
    FStorageState: IStorageStateChangeble;
    FStorageStateProxy: IStorageStateProxy;
    FTileNotifier: INotifierTilePyramidUpdateInternal;
    FAbilitiesNoStorage: ITileStorageTypeAbilities;

    FStorageLock: ICounter;
    FStorageNeedUpdate: ISimpleFlag;

    FCurrentTypeCode: Byte;
    FCurrentPath: string;
    FStorage: ITileStorage;
    FStorageCS: IReadWriteSync;
    FConfigChangeListener: IListener;
    FGlobalConfigChangeListener: IListener;
    FPathChangeListener: IListener;

    FPerfCounterList: IInternalPerformanceCounterList;
    FGetTileInfoCounter: IInternalPerformanceCounter;
    FGetTileRectInfoCounter: IInternalPerformanceCounter;
    FDeleteTileCounter: IInternalPerformanceCounter;
    FDeleteTNECounter: IInternalPerformanceCounter;
    FSaveTileCounter: IInternalPerformanceCounter;
    FSaveTNECounter: IInternalPerformanceCounter;
    procedure OnConfigChange;
    procedure OnGlobalConfigChange;
    procedure OnPathConfigChange;
    procedure StorageLock;
    procedure StorageUnlock;
    procedure DoUpdateStorage;
    procedure UpdateActualPath;
  private
    function GetStorage: ITileStorage;
    procedure BuildStorage(
      const AConfig: ISimpleTileStorageConfigStatic;
      ATypeCode: Byte;
      const APath: string
    );
  private
    function GetTileNotifier: INotifierTilePyramidUpdate;
    function GetStorageTypeAbilities: ITileStorageTypeAbilities;
    function GetState: IStorageStateChangeble;
    function GetCoordConverter: ICoordConverter;

    function GetTileFileName(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersion: IMapVersionInfo
    ): string;
    function GetTileInfo(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersion: IMapVersionInfo;
      const AMode: TGetTileInfoMode
    ): ITileInfoBasic;
    function GetTileInfoEx(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersion: IMapVersionRequest;
      const AMode: TGetTileInfoMode
    ): ITileInfoBasic;
    function GetTileRectInfo(
      const ARect: TRect;
      const AZoom: byte;
      const AVersionInfo: IMapVersionRequest
    ): ITileRectInfo;

    function DeleteTile(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersion: IMapVersionInfo
    ): Boolean;
    function SaveTile(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersion: IMapVersionInfo;
      const ALoadDate: TDateTime;
      const AContentType: IContentTypeInfoBasic;
      const AData: IBinaryData;
      const AIsOverwrite: Boolean
    ): Boolean;
    function GetListOfTileVersions(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionRequest
    ): IMapVersionListStatic;

    function ScanTiles(
      const AIgnoreTNE: Boolean;
      const AIgnoreMultiVersionTiles: Boolean
    ): IEnumTileInfo;
  public
    constructor Create(
      const AGlobalCacheConfig: IGlobalCacheConfig;
      const ACoordConverter: ICoordConverter;
      const AProjectionInfoFactory: IProjectionInfoFactory;
      const ATileStorageTypeList: ITileStorageTypeListStatic;
      const AConfig: ISimpleTileStorageConfig;
      const ACacheTileInfo: ITileInfoBasicMemCache;
      const AContentTypeManager: IContentTypeManager;
      const APerfCounterList: IInternalPerformanceCounterList
    );
    destructor Destroy; override;
  end;

implementation

uses
  c_CacheTypeCodes,
  i_TileStorageTypeListItem,
  u_ListenerByEvent,
  u_PathConfig,
  u_StorageStateProxy,
  u_NotifierTilePyramidUpdate,
  u_TileStorageAbilities,
  u_SimpleFlagWithInterlock,
  u_Synchronizer;

{ TTileStorageOfMapType }

constructor TTileStorageOfMapType.Create(
  const AGlobalCacheConfig: IGlobalCacheConfig;
  const ACoordConverter: ICoordConverter;
  const AProjectionInfoFactory: IProjectionInfoFactory;
  const ATileStorageTypeList: ITileStorageTypeListStatic;
  const AConfig: ISimpleTileStorageConfig;
  const ACacheTileInfo: ITileInfoBasicMemCache;
  const AContentTypeManager: IContentTypeManager;
  const APerfCounterList: IInternalPerformanceCounterList
);
var
  VState: TStorageStateProxy;
begin
  inherited Create;
  FGlobalCacheConfig := AGlobalCacheConfig;
  FCoordConverter := ACoordConverter;
  FTileStorageTypeList := ATileStorageTypeList;
  FConfig := AConfig;
  FCacheTileInfo := ACacheTileInfo;
  FContentTypeManager := AContentTypeManager;

  FAbilitiesNoStorage := TTileStorageTypeAbilitiesNoAccess.Create;
  FStorageCS := GSync.SyncVariable.Make(Self.ClassName);
  VState := TStorageStateProxy.Create;
  FStorageState := VState;
  FStorageStateProxy := VState;
  FStorageLock := TCounterInterlock.Create;
  FStorageNeedUpdate := TSimpleFlagWithInterlock.Create;
  FTileNotifier := TNotifierTilePyramidUpdate.Create(AProjectionInfoFactory, FCoordConverter);

  FPerfCounterList := APerfCounterList.CreateAndAddNewSubList('TileStorage');
  FGetTileInfoCounter := FPerfCounterList.CreateAndAddNewCounter('GetTileInfo');
  FGetTileRectInfoCounter := FPerfCounterList.CreateAndAddNewCounter('GetTileRectInfo');
  FDeleteTileCounter := FPerfCounterList.CreateAndAddNewCounter('DeleteTile');
  FDeleteTNECounter := FPerfCounterList.CreateAndAddNewCounter('DeleteTNE');
  FSaveTileCounter := FPerfCounterList.CreateAndAddNewCounter('SaveTile');
  FSaveTNECounter := FPerfCounterList.CreateAndAddNewCounter('SaveTNE');

  FActualPath := TPathConfig.Create('', '', nil);

  FGlobalConfigChangeListener := TNotifyNoMmgEventListener.Create(Self.OnGlobalConfigChange);
  FPathChangeListener := TNotifyNoMmgEventListener.Create(Self.OnPathConfigChange);
  FConfigChangeListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);

  UpdateActualPath;
  DoUpdateStorage;

  FConfig.ChangeNotifier.Add(FConfigChangeListener);
  FGlobalCacheConfig.ChangeNotifier.Add(FGlobalConfigChangeListener);
  FActualPath.ChangeNotifier.Add(FPathChangeListener);
end;

destructor TTileStorageOfMapType.Destroy;
begin
  if Assigned(FConfig) and Assigned(FConfigChangeListener) then begin
    FConfig.ChangeNotifier.Remove(FConfigChangeListener);
    FConfig := nil;
    FConfigChangeListener := nil;
  end;
  if Assigned(FGlobalCacheConfig) and Assigned(FGlobalConfigChangeListener) then begin
    FGlobalCacheConfig.ChangeNotifier.Remove(FGlobalConfigChangeListener);
    FGlobalCacheConfig := nil;
    FGlobalConfigChangeListener := nil;
  end;
  if Assigned(FActualPath) and Assigned(FPathChangeListener) then begin
    FActualPath.ChangeNotifier.Remove(FPathChangeListener);
    FActualPath := nil;
    FPathChangeListener := nil;
  end;

  inherited;
end;

procedure TTileStorageOfMapType.BuildStorage(
  const AConfig: ISimpleTileStorageConfigStatic;
  ATypeCode: Byte;
  const APath: string
);
var
  VMainContentType: IContentTypeInfoBasic;
  VCoordConverter: ICoordConverter;
  VStroageType: ITileStorageTypeListItem;
begin
  FStorage := nil;
  try
    FCurrentTypeCode := ATypeCode;
    FCurrentPath := APath;
    VCoordConverter := FCoordConverter;
    VMainContentType := FContentTypeManager.GetInfoByExt(AConfig.TileFileExt);
    if VMainContentType <> nil then begin
      VStroageType := FTileStorageTypeList.GetItemByCode(ATypeCode);
      if VStroageType <> nil then begin
        FStorage :=
          VStroageType.StorageType.BuildStorage(
            AConfig.Abilities,
            VCoordConverter,
            VMainContentType,
            FTileNotifier,
            FCurrentPath,
            FCacheTileInfo
          );
      end;
    end;
  except
    FStorage := nil;
  end;
  if FStorage <> nil then begin
    FStorageStateProxy.Target := FStorage.State;
  end else begin
    FStorageStateProxy.Target := nil;
  end;
end;

procedure TTileStorageOfMapType.DoUpdateStorage;
var
  VConfig: ISimpleTileStorageConfigStatic;
  VPath: string;
  VTypeCode: Byte;
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  FStorageCS.BeginWrite;
  try
    VConfig := FConfig.GetStatic;
    VTypeCode := VConfig.CacheTypeCode;
    if VTypeCode = c_File_Cache_Id_DEFAULT then begin
      VTypeCode := FGlobalCacheConfig.DefCache;
    end;
    VPath := IncludeTrailingPathDelimiter(FActualPath.FullPath);
    // build
    if (FCurrentTypeCode <> VTypeCode) or (FCurrentPath <> VPath) then begin
      BuildStorage(VConfig, VTypeCode, VPath);
      VNeedNotify := True;
    end;
  finally
    FStorageCS.EndWrite;
  end;
  if VNeedNotify then begin
    FTileNotifier.TileFullUpdateNotify;
  end;
end;

function TTileStorageOfMapType.GetStorage: ITileStorage;
begin
  FStorageCS.BeginRead;
  try
    Result := FStorage;
  finally
    FStorageCS.EndRead;
  end;
end;

function TTileStorageOfMapType.GetStorageTypeAbilities: ITileStorageTypeAbilities;
var
  VStorage: ITileStorage;
begin
  VStorage := GetStorage;
  if VStorage <> nil then begin
    Result := VStorage.StorageTypeAbilities;
  end else begin
    Result := FAbilitiesNoStorage;
  end;
end;

procedure TTileStorageOfMapType.OnConfigChange;
begin
  StorageLock;
  try
    UpdateActualPath;
    FStorageNeedUpdate.SetFlag;
  finally
    StorageUnlock;
  end;
end;

procedure TTileStorageOfMapType.OnGlobalConfigChange;
begin
  StorageLock;
  try
    UpdateActualPath;
    FStorageNeedUpdate.SetFlag;
  finally
    StorageUnlock;
  end;
end;

procedure TTileStorageOfMapType.OnPathConfigChange;
begin
  StorageLock;
  try
    FStorageNeedUpdate.SetFlag;
  finally
    StorageUnlock;
  end;
end;

procedure TTileStorageOfMapType.UpdateActualPath;
var
  VConfig: ISimpleTileStorageConfigStatic;
  VTypeCode: Byte;
  VBasePath: IPathConfig;
  VStorageType: ITileStorageTypeListItem;
begin
  VConfig := FConfig.GetStatic;
  VTypeCode := VConfig.CacheTypeCode;
  if VTypeCode = c_File_Cache_Id_DEFAULT then begin
    VTypeCode := FGlobalCacheConfig.DefCache;
  end;
  VBasePath := nil;
  VStorageType := FTileStorageTypeList.GetItemByCode(VTypeCode);
  if Assigned(VStorageType) then begin
    VBasePath := VStorageType.StorageType.Config.BasePath;
  end;
  FActualPath.LockWrite;
  try
    FActualPath.BasePathConfig := VBasePath;
    FActualPath.Path := VConfig.NameInCache;
  finally
    FActualPath.UnlockWrite;
  end;
end;

procedure TTileStorageOfMapType.StorageLock;
begin
  FStorageLock.Inc;
end;

procedure TTileStorageOfMapType.StorageUnlock;
var
  VLockCount: Integer;
begin
  VLockCount := FStorageLock.Dec;
  Assert(VLockCount >= 0);
  if VLockCount = 0 then begin
    if FStorageNeedUpdate.CheckFlagAndReset then begin
      DoUpdateStorage;
    end;
  end;
end;

function TTileStorageOfMapType.DeleteTile(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersion: IMapVersionInfo
): Boolean;
var
  VCounter: IInternalPerformanceCounter;
  VCounterContext: TInternalPerformanceCounterContext;
  VStorage: ITileStorage;
begin
  Result := False;
  VCounter := FDeleteTileCounter;
  VCounterContext := VCounter.StartOperation;
  try
    VStorage := GetStorage;
    if VStorage <> nil then begin
      Result := VStorage.DeleteTile(AXY, AZoom, AVersion);
    end;
  finally
    VCounter.FinishOperation(VCounterContext);
  end;
end;

function TTileStorageOfMapType.GetCoordConverter: ICoordConverter;
begin
  Result := FCoordConverter;
end;

function TTileStorageOfMapType.GetListOfTileVersions(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionRequest
): IMapVersionListStatic;
var
  VStorage: ITileStorage;
begin
  Result := nil;
  VStorage := GetStorage;
  if VStorage <> nil then begin
    Result := VStorage.GetListOfTileVersions(AXY, AZoom, AVersionInfo);
  end;
end;

function TTileStorageOfMapType.GetTileNotifier: INotifierTilePyramidUpdate;
begin
  Result := FTileNotifier;
end;

function TTileStorageOfMapType.GetState: IStorageStateChangeble;
begin
  Result := FStorageState;
end;

function TTileStorageOfMapType.GetTileFileName(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersion: IMapVersionInfo
): string;
var
  VStorage: ITileStorage;
begin
  Result := '';
  VStorage := GetStorage;
  if VStorage <> nil then begin
    Result := VStorage.GetTileFileName(AXY, AZoom, AVersion);
  end;
end;

function TTileStorageOfMapType.GetTileInfo(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersion: IMapVersionInfo;
  const AMode: TGetTileInfoMode
): ITileInfoBasic;
var
  VCounter: IInternalPerformanceCounter;
  VCounterContext: TInternalPerformanceCounterContext;
  VStorage: ITileStorage;
begin
  Result := nil;
  VCounter := FGetTileInfoCounter;
  VCounterContext := VCounter.StartOperation;
  try
    VStorage := GetStorage;
    if VStorage <> nil then begin
      Result := VStorage.GetTileInfo(AXY, AZoom, AVersion, AMode);
    end;
  finally
    VCounter.FinishOperation(VCounterContext);
  end;
end;

function TTileStorageOfMapType.GetTileInfoEx(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersion: IMapVersionRequest;
  const AMode: TGetTileInfoMode
): ITileInfoBasic;
var
  VCounter: IInternalPerformanceCounter;
  VCounterContext: TInternalPerformanceCounterContext;
  VStorage: ITileStorage;
begin
  Result := nil;
  VCounter := FGetTileInfoCounter;
  VCounterContext := VCounter.StartOperation;
  try
    VStorage := GetStorage;
    if VStorage <> nil then begin
      Result := VStorage.GetTileInfoEx(AXY, AZoom, AVersion, AMode);
    end;
  finally
    VCounter.FinishOperation(VCounterContext);
  end;
end;

function TTileStorageOfMapType.GetTileRectInfo(
  const ARect: TRect;
  const AZoom: byte;
  const AVersionInfo: IMapVersionRequest
): ITileRectInfo;
var
  VCounter: IInternalPerformanceCounter;
  VCounterContext: TInternalPerformanceCounterContext;
  VStorage: ITileStorage;
begin
  Result := nil;
  VCounter := FGetTileRectInfoCounter;
  VCounterContext := VCounter.StartOperation;
  try
    VStorage := GetStorage;
    if VStorage <> nil then begin
      Result := VStorage.GetTileRectInfo(ARect, AZoom, AVersionInfo);
    end;
  finally
    VCounter.FinishOperation(VCounterContext);
  end;
end;

function TTileStorageOfMapType.SaveTile(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersion: IMapVersionInfo;
  const ALoadDate: TDateTime;
  const AContentType: IContentTypeInfoBasic;
  const AData: IBinaryData;
  const AIsOverwrite: Boolean
): Boolean;
var
  VCounter: IInternalPerformanceCounter;
  VCounterContext: TInternalPerformanceCounterContext;
  VStorage: ITileStorage;
begin
  Result := False;
  if Assigned(AContentType) and Assigned(AData) then begin
    VCounter := FSaveTileCounter;
  end else begin
    VCounter := FSaveTNECounter;
  end;
  VCounterContext := VCounter.StartOperation;
  try
    VStorage := GetStorage;
    if VStorage <> nil then begin
      Result := VStorage.SaveTile(AXY, AZoom, AVersion, ALoadDate, AContentType, AData, AIsOverwrite);
    end;
  finally
    VCounter.FinishOperation(VCounterContext);
  end;
end;

function TTileStorageOfMapType.ScanTiles(
  const AIgnoreTNE: Boolean;
  const AIgnoreMultiVersionTiles: Boolean
): IEnumTileInfo;
var
  VStorage: ITileStorage;
begin
  Result := nil;
  VStorage := GetStorage;
  if VStorage <> nil then begin
    Result := VStorage.ScanTiles(AIgnoreTNE, AIgnoreMultiVersionTiles);
  end;
end;

end.
