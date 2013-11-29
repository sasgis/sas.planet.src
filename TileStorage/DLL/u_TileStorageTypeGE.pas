unit u_TileStorageTypeGE deprecated;

interface

uses
  i_CoordConverter,
  i_ContentTypeInfo,
  i_ContentTypeManager,
  i_MapVersionFactory,
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
  u_TileStorageTypeAbilities,
  u_TileStorageGE;

{ TTileStorageTypeGC }

constructor TTileStorageTypeGC.Create(
  const AContentTypeManager: IContentTypeManager;
  const AMapVersionFactory: IMapVersionFactory;
  const AConfig: ITileStorageTypeConfig
);
begin
  inherited Create(
    TTileStorageTypeAbilitiesGE.Create,
    AMapVersionFactory,
    AConfig
  );
  FContentTypeManager := AContentTypeManager;
end;

function TTileStorageTypeGC.BuildStorageInternal(
  const AForceAbilities: ITileStorageAbilities;
  const AGeoConverter: ICoordConverter;
  const AMainContentType: IContentTypeInfoBasic;
  const APath: string;
  const ACacheTileInfo: ITileInfoBasicMemCache
): ITileStorage;
begin
  Result :=
    TTileStorageGC.Create(
      AGeoConverter,
      APath,
      GetMapVersionFactory,
      FContentTypeManager
    );
end;

end.
