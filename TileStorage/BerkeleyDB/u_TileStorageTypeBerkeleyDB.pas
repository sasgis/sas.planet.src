unit u_TileStorageTypeBerkeleyDB;

interface

uses
  i_CoordConverter,
  i_ContentTypeInfo,
  i_ContentTypeManager,
  i_NotifierTime,
  i_MapVersionFactory,
  i_TileStorage,
  i_TileStorageAbilities,
  i_TileStorageTypeConfig,
  i_ConfigDataProvider,
  i_TileInfoBasicMemCache,
  i_GlobalBerkeleyDBHelper,
  i_TileStorageBerkeleyDBConfigStatic,
  u_TileStorageTypeBase;

type
  TTileStorageTypeBerkeleyDB = class(TTileStorageTypeBase)
  private
    FGCNotifier: INotifierTime;
    FIsVersioned: Boolean;
    FContentTypeManager: IContentTypeManager;
    FGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
    function ReadConfig(
      const AStorageConfigData: IConfigDataProvider
    ): ITileStorageBerkeleyDBConfigStatic;
  protected
    function BuildStorageInternal(
      const AStorageConfigData: IConfigDataProvider;
      const AForceAbilities: ITileStorageAbilities;
      const AGeoConverter: ICoordConverter;
      const AMainContentType: IContentTypeInfoBasic;
      const APath: string;
      const ACacheTileInfo: ITileInfoBasicMemCache
    ): ITileStorage; override;
  public
    constructor Create(
      const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
      const AGCNotifier: INotifierTime;
      const AIsVersioned: Boolean;
      const AContentTypeManager: IContentTypeManager;
      const AMapVersionFactory: IMapVersionFactory;
      const AConfig: ITileStorageTypeConfig
    );
  end;

implementation

uses
  u_TileStorageAbilities,
  u_TileStorageBerkeleyDBConfigStatic,
  u_TileStorageBerkeleyDB;

{ TTileStorageTypeBerkeleyDB }

constructor TTileStorageTypeBerkeleyDB.Create(
  const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
  const AGCNotifier: INotifierTime;
  const AIsVersioned: Boolean;
  const AContentTypeManager: IContentTypeManager;
  const AMapVersionFactory: IMapVersionFactory;
  const AConfig: ITileStorageTypeConfig
);
var
  VAbilities: ITileStorageTypeAbilities;
begin
  VAbilities :=
    TTileStorageTypeAbilities.Create(
      TTileStorageAbilities.Create(False, True, True, True),
      AIsVersioned,
      False
    );
  inherited Create(
    VAbilities,
    AMapVersionFactory,
    AConfig
  );
  FGCNotifier := AGCNotifier;
  FIsVersioned := AIsVersioned;
  FContentTypeManager := AContentTypeManager;
  FGlobalBerkeleyDBHelper := AGlobalBerkeleyDBHelper;
end;

function TTileStorageTypeBerkeleyDB.ReadConfig(
  const AStorageConfigData: IConfigDataProvider
): ITileStorageBerkeleyDBConfigStatic;
var
  VSyncInterval: Cardinal;
  VCommitsCountToSync: Cardinal;
  VPoolSize: Cardinal;
  VPoolObjectTTL: Cardinal;
  VDatabasePageSize: Cardinal;
  VOnDeadLockRetryCount: Integer;
  VIsFullVerboseMode: Boolean;
  VBerkeleyDBConfigData: IConfigDataProvider;
begin
  VSyncInterval := 300000;
  VCommitsCountToSync := 1000;
  VPoolSize := 32;
  VPoolObjectTTL := 60000;
  VDatabasePageSize := 1024;
  VOnDeadLockRetryCount := 3;
  VIsFullVerboseMode := False;
  if Assigned(AStorageConfigData) then begin
    VBerkeleyDBConfigData := AStorageConfigData.GetSubItem('BerkeleyDB');
    if Assigned(VBerkeleyDBConfigData) then begin
      VSyncInterval := VBerkeleyDBConfigData.ReadInteger('SyncInterval', VSyncInterval);
      VCommitsCountToSync := VBerkeleyDBConfigData.ReadInteger('CommitsCountToSync', VCommitsCountToSync);
      VPoolSize := VBerkeleyDBConfigData.ReadInteger('PoolSize', VPoolSize);
      VPoolObjectTTL := VBerkeleyDBConfigData.ReadInteger('PoolObjectTTL', VPoolObjectTTL);
      VDatabasePageSize := VBerkeleyDBConfigData.ReadInteger('DatabasePageSize', VDatabasePageSize);
      VOnDeadLockRetryCount := VBerkeleyDBConfigData.ReadInteger('OnDeadLockRetryCount', VOnDeadLockRetryCount);
      VIsFullVerboseMode := VBerkeleyDBConfigData.ReadBool('IsFullVerboseMode', VIsFullVerboseMode);
    end;
  end;
  Result :=
    TTileStorageBerkeleyDBConfigStatic.Create(
      VSyncInterval,
      VCommitsCountToSync,
      VPoolSize,
      VPoolObjectTTL,
      VDatabasePageSize,
      VOnDeadLockRetryCount,
      VIsFullVerboseMode
    );
end;

function TTileStorageTypeBerkeleyDB.BuildStorageInternal(
  const AStorageConfigData: IConfigDataProvider;
  const AForceAbilities: ITileStorageAbilities;
  const AGeoConverter: ICoordConverter;
  const AMainContentType: IContentTypeInfoBasic;
  const APath: string;
  const ACacheTileInfo: ITileInfoBasicMemCache
): ITileStorage;
var
  VConfig: ITileStorageBerkeleyDBConfigStatic;
begin
  VConfig := ReadConfig(AStorageConfigData);
  Result :=
    TTileStorageBerkeleyDB.Create(
      GetAbilities,
      AForceAbilities,
      FGlobalBerkeleyDBHelper,
      AGeoConverter,
      APath,
      VConfig,
      FIsVersioned,
      FGCNotifier,
      ACacheTileInfo,
      FContentTypeManager,
      GetMapVersionFactory,
      AMainContentType
    );
end;

end.
