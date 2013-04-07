unit u_TileStorageOfMapType;

interface

uses
  Types,
  SysUtils,
  i_SimpleTileStorageConfig,
  i_TileStorage,
  i_NotifierTime,
  i_ContentTypeManager,
  i_InternalPerformanceCounter,
  i_NotifierTilePyramidUpdate,
  i_StorageState,
  i_CoordConverter,
  i_ContentTypeInfo,
  i_Listener,
  i_MapVersionConfig,
  i_MapVersionInfo,
  i_TileInfoBasic,
  i_SimpleFlag,
  i_PathConfig,
  i_StorageStateProxy,
  i_BinaryData,
  i_TileFileNameGeneratorsList,
  i_TileFileNameParsersList,
  i_GlobalBerkeleyDBHelper,
  i_TileInfoBasicMemCache,
  i_GlobalCacheConfig,
  u_BaseInterfacedObject;

type
  TTileStorageOfMapType = class(TBaseInterfacedObject, ITileStorage)
  private
    FGlobalCacheConfig: IGlobalCacheConfig;
    FGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
    FConfig: ISimpleTileStorageConfig;
    FVersionConfig: IMapVersionConfig;
    FGCNotifier: INotifierTime;
    FContentTypeManager: IContentTypeManager;
    FFileNameGeneratorsList: ITileFileNameGeneratorsList;
    FFileNameParsersList: ITileFileNameParsersList;
    FCacheTileInfo: ITileInfoBasicMemCache;
    FActualPath: IPathConfig;
    FStorageState: IStorageStateChangeble;
    FStorageStateProxy: IStorageStateProxy;
    FMapVersionFactoryDefault: IMapVersionFactory;

    FStorageChangeCS: IReadWriteSync;

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
    function GetState: IStorageStateChangeble;
    function GetCoordConverter: ICoordConverter;
    function GetIsFileCache: Boolean;
    function GetIsCanSaveMultiVersionTiles: Boolean;

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
    function GetTileRectInfo(
      const ARect: TRect;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): ITileRectInfo;

    function DeleteTile(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersion: IMapVersionInfo
    ): Boolean;
    procedure SaveTile(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersion: IMapVersionInfo;
      const ALoadDate: TDateTime;
      const AContentType: IContentTypeInfoBasic;
      const AData: IBinaryData
    );
    procedure SaveTNE(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersion: IMapVersionInfo;
      const ALoadDate: TDateTime
    );
    function GetListOfTileVersions(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): IMapVersionListStatic;

    function ScanTiles(
      const AIgnoreTNE: Boolean;
      const AIgnoreMultiVersionTiles: Boolean
    ): IEnumTileInfo;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
  public
    constructor Create(
      const AGlobalCacheConfig: IGlobalCacheConfig;
      const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
      const AConfig: ISimpleTileStorageConfig;
      const ACacheTileInfo: ITileInfoBasicMemCache;
      const AVersionConfig: IMapVersionConfig;
      const AGCNotifier: INotifierTime;
      const AContentTypeManager: IContentTypeManager;
      const AFileNameGeneratorsList: ITileFileNameGeneratorsList;
      const AFileNameParsersList: ITileFileNameParsersList;
      const APerfCounterList: IInternalPerformanceCounterList
    );
    destructor Destroy; override;
  end;

implementation

uses
  c_CacheTypeCodes,
  c_CoordConverter,
  i_TileFileNameGenerator,
  i_TileFileNameParser,
  u_ListenerByEvent,
  u_PathConfig,
  u_StorageStateProxy,
  u_SimpleFlagWithInterlock,
  u_TileStorageBerkeleyDB,
  u_TileStorageFileSystem,
  u_TileStorageGE,
  u_TileStorageDBMS,
  u_TileStorageInRAM,
  u_Synchronizer;

{ TTileStorageOfMapType }

constructor TTileStorageOfMapType.Create(
  const AGlobalCacheConfig: IGlobalCacheConfig;
  const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
  const AConfig: ISimpleTileStorageConfig;
  const ACacheTileInfo: ITileInfoBasicMemCache;
  const AVersionConfig: IMapVersionConfig;
  const AGCNotifier: INotifierTime;
  const AContentTypeManager: IContentTypeManager;
  const AFileNameGeneratorsList: ITileFileNameGeneratorsList;
  const AFileNameParsersList: ITileFileNameParsersList;
  const APerfCounterList: IInternalPerformanceCounterList
);
var
  VState: TStorageStateProxy;
begin
  inherited Create;
  FGlobalCacheConfig := AGlobalCacheConfig;
  FGlobalBerkeleyDBHelper := AGlobalBerkeleyDBHelper;
  FConfig := AConfig;
  FCacheTileInfo := ACacheTileInfo;
  FVersionConfig := AVersionConfig;
  FGCNotifier := AGCNotifier;
  FContentTypeManager := AContentTypeManager;
  FFileNameGeneratorsList := AFileNameGeneratorsList;
  FFileNameParsersList := AFileNameParsersList;

  FStorageCS := MakeSyncRW_Var(Self, False);
  FStorageChangeCS := MakeSyncRW_Sym(Self, False);
  VState := TStorageStateProxy.Create;
  FStorageState := VState;
  FStorageStateProxy := VState;
  FMapVersionFactoryDefault := FVersionConfig.VersionFactory;
  FStorageLock := TCounterInterlock.Create;
  FStorageNeedUpdate := TSimpleFlagWithInterlock.Create;

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
  if FConfig <> nil then begin
    FConfig.ChangeNotifier.Remove(FConfigChangeListener);
    FConfig := nil;
    FConfigChangeListener := nil;
  end;
  if FGlobalCacheConfig <> nil then begin
    FGlobalCacheConfig.ChangeNotifier.Remove(FGlobalConfigChangeListener);
    FGlobalCacheConfig := nil;
    FGlobalConfigChangeListener := nil;
  end;
  if FActualPath <> nil then begin
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
  VFileNameGenerator: ITileFileNameGenerator;
  VFileNameParser: ITileFileNameParser;
  VCoordConverter: ICoordConverter;
begin
  FStorage := nil;
  try
    FCurrentTypeCode := ATypeCode;
    FCurrentPath := APath;
    VCoordConverter := AConfig.CoordConverter;
    if ATypeCode = c_File_Cache_Id_BDB then begin
      VMainContentType := FContentTypeManager.GetInfoByExt(AConfig.TileFileExt);
      if VMainContentType <> nil then begin
        FStorage :=
          TTileStorageBerkeleyDB.Create(
            FGlobalBerkeleyDBHelper,
            VCoordConverter,
            FCurrentPath,
            FGCNotifier,
            FCacheTileInfo,
            FContentTypeManager,
            FVersionConfig.VersionFactory,
            VMainContentType
          );
      end;
    end else if ATypeCode = c_File_Cache_Id_DBMS then begin
      VMainContentType := FContentTypeManager.GetInfoByExt(AConfig.TileFileExt);
      if VMainContentType <> nil then begin
        FStorage :=
          TTileStorageDBMS.Create(
            VCoordConverter,
            FGlobalCacheConfig.DBMSCachePath.Path,
            FCurrentPath,
            FGCNotifier,
            FCacheTileInfo,
            FContentTypeManager,
            FVersionConfig.VersionFactory,
            VMainContentType
          );
      end;
    end else if ATypeCode = c_File_Cache_Id_GE then begin
      if
        (VCoordConverter.GetProjectionEPSG = CGELonLatProjectionEPSG) and
        (VCoordConverter.GetTileSplitCode = CTileSplitQuadrate256x256)
      then begin
        FStorage :=
          TTileStorageGE.Create(
            VCoordConverter,
            FCurrentPath,
            FVersionConfig.VersionFactory,
            FContentTypeManager
          );
      end;
    end else if ATypeCode = c_File_Cache_Id_GC then begin
      if
        (VCoordConverter.GetProjectionEPSG = CGELonLatProjectionEPSG) and
        (VCoordConverter.GetTileSplitCode = CTileSplitQuadrate256x256)
      then begin
        FStorage :=
          TTileStorageGC.Create(
            VCoordConverter,
            FCurrentPath,
            FVersionConfig.VersionFactory,
            FContentTypeManager
          );
      end;
    end else if ATypeCode in [c_File_Cache_Id_GMV, c_File_Cache_Id_SAS, c_File_Cache_Id_ES, c_File_Cache_Id_GM, c_File_Cache_Id_GM_Aux, c_File_Cache_Id_GM_Bing] then begin
      VMainContentType := FContentTypeManager.GetInfoByExt(AConfig.TileFileExt);
      VFileNameGenerator := FFileNameGeneratorsList.GetGenerator(ATypeCode);
      VFileNameParser := FFileNameParsersList.GetParser(ATypeCode);
      if (VMainContentType <> nil) and (VFileNameGenerator <> nil) and (VFileNameParser <> nil) then begin
        FStorage :=
          TTileStorageFileSystem.Create(
            VCoordConverter,
            FCurrentPath,
            VMainContentType,
            FVersionConfig.VersionFactory,
            VFileNameGenerator,
            VFileNameParser
          );
      end;
    end else if ATypeCode = c_File_Cache_Id_RAM then begin
      VMainContentType := FContentTypeManager.GetInfoByExt(AConfig.TileFileExt);
      if VMainContentType <> nil then begin
        FStorage :=
          TTileStorageInRAM.Create(
            FCacheTileInfo,
            VCoordConverter,
            FVersionConfig.VersionFactory,
            VMainContentType
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
begin
  FStorageCS.BeginWrite;
  try
    VConfig := FConfig.GetStatic;
    VTypeCode := VConfig.CacheTypeCode;
    if VTypeCode = c_File_Cache_Id_DEFAULT then begin
      VTypeCode := FGlobalCacheConfig.DefCache;
    end;
    // for DBMS use only defined Path
    if (c_File_Cache_Id_DBMS=VTypeCode) then
      VPath := FActualPath.Path
    else
      VPath := IncludeTrailingPathDelimiter(FActualPath.FullPath);
    // build
    if (FCurrentTypeCode <> VTypeCode) or (FCurrentPath <> VPath) then begin
      BuildStorage(VConfig, VTypeCode, VPath);
    end;
  finally
    FStorageCS.EndWrite;
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

function TTileStorageOfMapType.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
  if (E_NOINTERFACE=Result) and Assigned(FStorage) then
    Result := FStorage.QueryInterface(IID, Obj);
end;

procedure TTileStorageOfMapType.UpdateActualPath;
var
  VConfig: ISimpleTileStorageConfigStatic;
  VTypeCode: Byte;
  VBasePath: IPathConfig;
begin
  VConfig := FConfig.GetStatic;
  VTypeCode := VConfig.CacheTypeCode;
  if VTypeCode = c_File_Cache_Id_DEFAULT then begin
    VTypeCode := FGlobalCacheConfig.DefCache;
  end;
  VBasePath := nil;
  case VTypeCode of
    c_File_Cache_Id_GMV: begin
      VBasePath := FGlobalCacheConfig.OldCPath;
    end;
    c_File_Cache_Id_SAS: begin
      VBasePath := FGlobalCacheConfig.NewCPath;
    end;
    c_File_Cache_Id_ES: begin
      VBasePath := FGlobalCacheConfig.ESCPath;
    end;
    c_File_Cache_Id_GM: begin
      VBasePath := FGlobalCacheConfig.GMTilesPath;
    end;
    c_File_Cache_Id_GM_Aux: begin
      VBasePath := FGlobalCacheConfig.GMTilesPath;
    end;
    c_File_Cache_Id_GM_Bing: begin
      VBasePath := FGlobalCacheConfig.GMTilesPath;
    end;
    c_File_Cache_Id_GE: begin
      VBasePath := FGlobalCacheConfig.GECachePath;
    end;
    c_File_Cache_Id_BDB: begin
      VBasePath := FGlobalCacheConfig.BDBCachePath;
    end;
    c_File_Cache_Id_DBMS: begin
      VBasePath := FGlobalCacheConfig.DBMSCachePath;
    end;
    c_File_Cache_Id_GC: begin
      VBasePath := FGlobalCacheConfig.GCCachePath;
    end;
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

function TTileStorageOfMapType.DeleteTile(const AXY: TPoint; const AZoom: byte;
  const AVersion: IMapVersionInfo): Boolean;
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
  Result := FConfig.CoordConverter;
end;

function TTileStorageOfMapType.GetIsFileCache: Boolean;
var
  VStorage: ITileStorage;
begin
  Result := False;
  VStorage := GetStorage;
  if VStorage <> nil then begin
    Result := VStorage.IsFileCache;
  end;
end;

function TTileStorageOfMapType.GetIsCanSaveMultiVersionTiles: Boolean;
var
  VStorage: ITileStorage;
begin
  Result := False;
  VStorage := GetStorage;
  if VStorage <> nil then begin
    Result := VStorage.GetIsCanSaveMultiVersionTiles;
  end;
end;

function TTileStorageOfMapType.GetListOfTileVersions(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo
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
var
  VStorage: ITileStorage;
begin
  Result := nil;
  VStorage := GetStorage;
  if VStorage <> nil then begin
    Result := VStorage.TileNotifier;
  end;
end;

function TTileStorageOfMapType.GetState: IStorageStateChangeble;
begin
  Result := FStorageState;
end;

function TTileStorageOfMapType.GetTileFileName(const AXY: TPoint;
  const AZoom: byte; const AVersion: IMapVersionInfo): string;
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

function TTileStorageOfMapType.GetTileRectInfo(const ARect: TRect;
  const AZoom: byte; const AVersionInfo: IMapVersionInfo): ITileRectInfo;
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

procedure TTileStorageOfMapType.SaveTile(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersion: IMapVersionInfo;
  const ALoadDate: TDateTime;
  const AContentType: IContentTypeInfoBasic;
  const AData: IBinaryData
);
var
  VCounter: IInternalPerformanceCounter;
  VCounterContext: TInternalPerformanceCounterContext;
  VStorage: ITileStorage;
begin
  VCounter := FSaveTileCounter;
  VCounterContext := VCounter.StartOperation;
  try
    VStorage := GetStorage;
    if VStorage <> nil then begin
      VStorage.SaveTile(AXY, AZoom, AVersion, ALoadDate, AContentType, AData);
    end;
  finally
    VCounter.FinishOperation(VCounterContext);
  end;
end;

procedure TTileStorageOfMapType.SaveTNE(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersion: IMapVersionInfo;
  const ALoadDate: TDateTime
);
var
  VCounter: IInternalPerformanceCounter;
  VCounterContext: TInternalPerformanceCounterContext;
  VStorage: ITileStorage;
begin
  VCounter := FSaveTNECounter;
  VCounterContext := VCounter.StartOperation;
  try
    VStorage := GetStorage;
    if VStorage <> nil then begin
      VStorage.SaveTNE(AXY, AZoom, AVersion, ALoadDate);
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
