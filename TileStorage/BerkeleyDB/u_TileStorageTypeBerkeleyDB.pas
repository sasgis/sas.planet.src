unit u_TileStorageTypeBerkeleyDB;

interface

uses
  i_CoordConverter,
  i_ContentTypeInfo,
  i_ContentTypeManager,
  i_NotifierTime,
  i_MapVersionConfig,
  i_TileStorage,
  i_TileStorageTypeConfig,
  i_TileInfoBasicMemCache,
  i_GlobalBerkeleyDBHelper,
  u_TileStorageTypeBase;

type
  TTileStorageTypeBerkeleyDB = class(TTileStorageTypeBase)
  private
    FGCNotifier: INotifierTime;
    FIsVersioned: Boolean;
    FContentTypeManager: IContentTypeManager;
    FGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
  protected
    function BuildStorage(
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
  u_TileStorageTypeAbilities,
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
begin
  inherited Create(
    TTileStorageTypeAbilitiesBerkeleyDB.Create(False), // ToDo
    AMapVersionFactory,
    AConfig
  );
  FGCNotifier := AGCNotifier;
  FIsVersioned := AIsVersioned;
  FContentTypeManager := AContentTypeManager;
  FGlobalBerkeleyDBHelper := AGlobalBerkeleyDBHelper;
end;

function TTileStorageTypeBerkeleyDB.BuildStorage(
  const AGeoConverter: ICoordConverter;
  const AMainContentType: IContentTypeInfoBasic;
  const APath: string;
  const ACacheTileInfo: ITileInfoBasicMemCache
): ITileStorage;
begin
  Result :=
    TTileStorageBerkeleyDB.Create(
      FGlobalBerkeleyDBHelper,
      AGeoConverter,
      APath,
      FIsVersioned,
      FGCNotifier,
      ACacheTileInfo,
      FContentTypeManager,
      GetMapVersionFactory,
      AMainContentType
    );
end;

end.
