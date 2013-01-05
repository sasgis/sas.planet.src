unit u_TileStorageTypeGE;

interface

uses
  i_CoordConverter,
  i_ContentTypeInfo,
  i_ContentTypeManager,
  i_TileStorage,
  i_TileInfoBasicMemCache,
  i_TileStorageTypeConfig,
  u_TileStorageTypeBase;

type
  TTileStorageTypeGE = class(TTileStorageTypeBase)
  private
    FContentTypeManager: IContentTypeManager;
  protected
    function BuildStorage(
      const AGeoConverter: ICoordConverter;
      const AMainContentType: IContentTypeInfoBasic;
      const APath: string;
      const ACacheTileInfo: ITileInfoBasicMemCache
    ): ITileStorage; override;
  public
    constructor Create(
      const AContentTypeManager: IContentTypeManager;
      const AConfig: ITileStorageTypeConfig
    );
  end;

  TTileStorageTypeGC = class(TTileStorageTypeBase)
  private
    FContentTypeManager: IContentTypeManager;
  protected
    function BuildStorage(
      const AGeoConverter: ICoordConverter;
      const AMainContentType: IContentTypeInfoBasic;
      const APath: string;
      const ACacheTileInfo: ITileInfoBasicMemCache
    ): ITileStorage; override;
  public
    constructor Create(
      const AContentTypeManager: IContentTypeManager;
      const AConfig: ITileStorageTypeConfig
    );
  end;

implementation

uses
  u_TileStorageTypeAbilities,
  u_MapVersionFactoryGE,
  u_TileStorageGE;

{ TTileStorageTypeGE }

constructor TTileStorageTypeGE.Create(
  const AContentTypeManager: IContentTypeManager;
  const AConfig: ITileStorageTypeConfig
);
begin
  inherited Create(
    TTileStorageTypeAbilitiesGE.Create,
    TMapVersionFactoryGE.Create,
    AConfig
  );
  FContentTypeManager := AContentTypeManager;
end;

function TTileStorageTypeGE.BuildStorage(
  const AGeoConverter: ICoordConverter;
  const AMainContentType: IContentTypeInfoBasic;
  const APath: string;
  const ACacheTileInfo: ITileInfoBasicMemCache
): ITileStorage;
begin
  Result :=
    TTileStorageGE.Create(
      AGeoConverter,
      APath,
      GetMapVersionFactory,
      FContentTypeManager
    );
end;

{ TTileStorageTypeGC }

constructor TTileStorageTypeGC.Create(
  const AContentTypeManager: IContentTypeManager;
  const AConfig: ITileStorageTypeConfig
);
begin
  inherited Create(
    TTileStorageTypeAbilitiesGE.Create,
    TMapVersionFactoryGE.Create,
    AConfig
  );
  FContentTypeManager := AContentTypeManager;
end;

function TTileStorageTypeGC.BuildStorage(
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
