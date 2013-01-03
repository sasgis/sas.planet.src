unit u_TileStorageTypeBerkeleyDB;

interface

uses
  i_CoordConverter,
  i_ContentTypeInfo,
  i_ContentTypeManager,
  i_NotifierTTLCheck,
  i_TileStorage,
  i_TileStorageTypeConfig,
  i_SimpleTileStorageConfig,
  i_GlobalBerkeleyDBHelper,
  u_TileStorageTypeBase;

type
  TTileStorageTypeBerkeleyDB = class(TTileStorageTypeBase)
  private
    FStorageConfig: ISimpleTileStorageConfigStatic;
    FGCList: INotifierTTLCheck;
    FContentTypeManager: IContentTypeManager;
    FGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
  protected
    function BuildStorage(
      const AGeoConverter: ICoordConverter;
      const AMainContentType: IContentTypeInfoBasic;
      const APath: string
    ): ITileStorage; override;
  public
    constructor Create(
      const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
      const AGCList: INotifierTTLCheck;
      const AContentTypeManager: IContentTypeManager;
      const AConfig: ITileStorageTypeConfig;
      const AStorageConfig: ISimpleTileStorageConfigStatic
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
  const AGCList: INotifierTTLCheck;
  const AContentTypeManager: IContentTypeManager;
  const AConfig: ITileStorageTypeConfig;
  const AStorageConfig: ISimpleTileStorageConfigStatic
);
begin
  inherited Create(
    TTileStorageTypeAbilitiesBerkeleyDB.Create,
    TMapVersionFactorySimpleString.Create,
    AConfig
  );
  FStorageConfig := AStorageConfig;
  FGCList := AGCList;
  FContentTypeManager := AContentTypeManager;
  FGlobalBerkeleyDBHelper := AGlobalBerkeleyDBHelper;
end;

function TTileStorageTypeBerkeleyDB.BuildStorage(
  const AGeoConverter: ICoordConverter;
  const AMainContentType: IContentTypeInfoBasic;
  const APath: string
): ITileStorage;
begin
  Result :=
    TTileStorageBerkeleyDB.Create(
      FGlobalBerkeleyDBHelper,
      AGeoConverter,
      APath,
      FGCList,
      FStorageConfig,
      FContentTypeManager,
      GetMapVersionFactory,
      AMainContentType
    );
end;

end.
