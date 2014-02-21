unit u_TileStorageTypeDBMS;

interface

uses
  i_CoordConverter,
  i_ContentTypeInfo,
  i_ContentTypeManager,
  i_NotifierTime,
  i_MapVersionFactory,
  i_ConfigDataProvider,
  i_TileStorage,
  i_TileStorageAbilities,
  i_TileStorageTypeConfig,
  i_TileInfoBasicMemCache,
  u_TileStorageTypeBase;

type
  TTileStorageTypeDBMS = class(TTileStorageTypeBase)
  private
    FGCNotifier: INotifierTime;
    FContentTypeManager: IContentTypeManager;
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
      const AGCNotifier: INotifierTime;
      const AContentTypeManager: IContentTypeManager;
      const AMapVersionFactory: IMapVersionFactory;
      const AConfig: ITileStorageTypeConfig
    );
  end;

implementation

uses
  SysUtils,
  u_TileStorageAbilities,
  u_TileStorageDBMS;

{ TTileStorageTypeDBMS }

constructor TTileStorageTypeDBMS.Create(
  const AGCNotifier: INotifierTime;
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
      True,
      False
    );
  inherited Create(
    VAbilities,
    AMapVersionFactory,
    AConfig
  );
  FGCNotifier := AGCNotifier;
  FContentTypeManager := AContentTypeManager;
end;

function TTileStorageTypeDBMS.BuildStorageInternal(
  const AStorageConfigData: IConfigDataProvider;
  const AForceAbilities: ITileStorageAbilities;
  const AGeoConverter: ICoordConverter;
  const AMainContentType: IContentTypeInfoBasic;
  const APath: string;
  const ACacheTileInfo: ITileInfoBasicMemCache
): ITileStorage;
begin
  Result :=
    TTileStorageDBMS.Create(
      GetAbilities,
      AForceAbilities,
      AGeoConverter,
      GetConfig.BasePath.Path,
      ExcludeTrailingPathDelimiter(APath),
      FGCNotifier,
      ACacheTileInfo,
      FContentTypeManager,
      GetMapVersionFactory,
      AMainContentType
    );
end;

end.
