unit u_TileStorageTypeDBMS;

interface

uses
  i_CoordConverter,
  i_ContentTypeInfo,
  i_ContentTypeManager,
  i_NotifierTTLCheck,
  i_TileStorage,
  i_TileStorageTypeConfig,
  i_SimpleTileStorageConfig,
  u_TileStorageTypeBase;

type
  TTileStorageTypeDBMS = class(TTileStorageTypeBase)
  private
    FStorageConfig: ISimpleTileStorageConfigStatic;
    FGCNotifier: INotifierTime;
    FContentTypeManager: IContentTypeManager;
  protected
    function BuildStorage(
      const AGeoConverter: ICoordConverter;
      const AMainContentType: IContentTypeInfoBasic;
      const APath: string
    ): ITileStorage; override;
  public
    constructor Create(
      const AGCNotifier: INotifierTime;
      const AContentTypeManager: IContentTypeManager;
      const AConfig: ITileStorageTypeConfig;
      const AStorageConfig: ISimpleTileStorageConfigStatic
    );
  end;

implementation

uses
  u_TileStorageTypeAbilities,
  u_MapVersionFactorySimpleString,
  u_TileStorageDBMS;


{ TTileStorageTypeDBMS }

constructor TTileStorageTypeDBMS.Create(
  const AGCNotifier: INotifierTime;
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
  FGCNotifier := AGCNotifier;
  FContentTypeManager := AContentTypeManager;
end;

function TTileStorageTypeDBMS.BuildStorage(
  const AGeoConverter: ICoordConverter;
  const AMainContentType: IContentTypeInfoBasic;
  const APath: string
): ITileStorage;
begin
  Result :=
    TTileStorageDBMS.Create(
      AGeoConverter,
      GetConfig.BasePath.Path,
      APath,
      FGCNotifier,
      FStorageConfig,
      FContentTypeManager,
      GetMapVersionFactory,
      AMainContentType
    );
end;

end.
