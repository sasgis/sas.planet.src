unit u_TileStorageTypeBerkeleyDB;

interface

uses
  i_CoordConverter,
  i_ContentTypeInfo,
  i_ContentTypeManager,
  i_NotifierTTLCheck,
  i_TileStorage,
  i_TileStorageTypeConfig,
  i_TileInfoBasicMemCache,
  i_GlobalBerkeleyDBHelper,
  u_TileStorageTypeBase;

type
  TTileStorageTypeBerkeleyDB = class(TTileStorageTypeBase)
  private
    FGCNotifier: INotifierTime;
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
      const AContentTypeManager: IContentTypeManager;
      const AConfig: ITileStorageTypeConfig
    );
  end;

implementation

uses
  u_TileStorageTypeAbilities,
  u_MapVersionFactorySimpleString,
  u_TileStorageBerkeleyDB;

{ TTileStorageTypeBerkeleyDB }

constructor TTileStorageTypeBerkeleyDB.Create(
  const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
  const AGCNotifier: INotifierTime;
  const AContentTypeManager: IContentTypeManager;
  const AConfig: ITileStorageTypeConfig
);
begin
  inherited Create(
    TTileStorageTypeAbilitiesBerkeleyDB.Create,
    TMapVersionFactorySimpleString.Create,
    AConfig
  );
  FGCNotifier := AGCNotifier;
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
      FGCNotifier,
      ACacheTileInfo,
      FContentTypeManager,
      GetMapVersionFactory,
      AMainContentType
    );
end;

end.
