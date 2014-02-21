unit u_TileStorageTypeGE deprecated;

interface

uses
  i_CoordConverter,
  i_ContentTypeInfo,
  i_ContentTypeManager,
  i_MapVersionFactory,
  i_ConfigDataProvider,
  i_TileStorage,
  i_TileStorageAbilities,
  i_TileInfoBasicMemCache,
  i_TileStorageTypeConfig,
  u_TileStorageTypeBase;

type
  TTileStorageTypeGC = class(TTileStorageTypeBase)
  private
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
      const AContentTypeManager: IContentTypeManager;
      const AMapVersionFactory: IMapVersionFactory;
      const AConfig: ITileStorageTypeConfig
    );
  end;

implementation

uses
  u_TileStorageAbilities,
  u_TileStorageGE;

{ TTileStorageTypeGC }

constructor TTileStorageTypeGC.Create(
  const AContentTypeManager: IContentTypeManager;
  const AMapVersionFactory: IMapVersionFactory;
  const AConfig: ITileStorageTypeConfig
);
var
  VAbilities: ITileStorageTypeAbilities;
begin
  VAbilities :=
    TTileStorageTypeAbilities.Create(
      TTileStorageAbilities.Create(True, False, False, False),
      True,
      False
    );
  inherited Create(
    VAbilities,
    AMapVersionFactory,
    AConfig
  );
  FContentTypeManager := AContentTypeManager;
end;

function TTileStorageTypeGC.BuildStorageInternal(
  const AStorageConfigData: IConfigDataProvider;
  const AForceAbilities: ITileStorageAbilities;
  const AGeoConverter: ICoordConverter;
  const AMainContentType: IContentTypeInfoBasic;
  const APath: string;
  const ACacheTileInfo: ITileInfoBasicMemCache
): ITileStorage;
begin
  Result :=
    TTileStorageGC.Create(
      GetAbilities,
      AForceAbilities,
      AGeoConverter,
      APath,
      GetMapVersionFactory,
      FContentTypeManager
    );
end;

end.
