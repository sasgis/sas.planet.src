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
    FGCList: INotifierTTLCheck;
    FContentTypeManager: IContentTypeManager;
  protected
    function BuildStorage(
      const AGeoConverter: ICoordConverter;
      const AMainContentType: IContentTypeInfoBasic;
      const APath: string
    ): ITileStorage; override;
  public
    constructor Create(
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
  u_TileStorageDBMS;


{ TTileStorageTypeDBMS }

constructor TTileStorageTypeDBMS.Create(
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
      FGCList,
      FStorageConfig,
      FContentTypeManager,
      GetMapVersionFactory,
      AMainContentType
    );
end;

end.
