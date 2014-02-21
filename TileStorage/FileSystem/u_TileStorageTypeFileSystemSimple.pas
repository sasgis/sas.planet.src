unit u_TileStorageTypeFileSystemSimple;

interface

uses
  i_CoordConverter,
  i_ContentTypeInfo,
  i_TileFileNameGenerator,
  i_TileFileNameParser,
  i_MapVersionFactory,
  i_ConfigDataProvider,
  i_TileStorage,
  i_TileStorageAbilities,
  i_TileInfoBasicMemCache,
  i_TileStorageTypeConfig,
  u_TileStorageTypeBase;

type
  TTileStorageTypeFileSystemSimple = class(TTileStorageTypeBase)
  private
    FNameGenerator: ITileFileNameGenerator;
    FTileNameParser: ITileFileNameParser;
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
      const ANameGenerator: ITileFileNameGenerator;
      const ATileNameParser: ITileFileNameParser;
      const AMapVersionFactory: IMapVersionFactory;
      const AConfig: ITileStorageTypeConfig
    );
  end;

implementation

uses
  u_TileStorageAbilities,
  u_TileStorageFileSystem;

{ TTileStorageTypeFileSystemSimple }

constructor TTileStorageTypeFileSystemSimple.Create(
  const ANameGenerator: ITileFileNameGenerator;
  const ATileNameParser: ITileFileNameParser;
  const AMapVersionFactory: IMapVersionFactory;
  const AConfig: ITileStorageTypeConfig
);
var
  VAbilities: ITileStorageTypeAbilities;
begin
  VAbilities :=
    TTileStorageTypeAbilities.Create(
      TTileStorageAbilities.Create(False, True, True, True),
      False,
      True
    );
  inherited Create(
    VAbilities,
    AMapVersionFactory,
    AConfig
  );
  FNameGenerator := ANameGenerator;
  FTileNameParser := ATileNameParser;
end;

function TTileStorageTypeFileSystemSimple.BuildStorageInternal(
  const AStorageConfigData: IConfigDataProvider;
  const AForceAbilities: ITileStorageAbilities;
  const AGeoConverter: ICoordConverter;
  const AMainContentType: IContentTypeInfoBasic;
  const APath: string;
  const ACacheTileInfo: ITileInfoBasicMemCache
): ITileStorage;
begin
  Result :=
    TTileStorageFileSystem.Create(
      GetAbilities,
      AForceAbilities,
      AGeoConverter,
      APath,
      AMainContentType,
      GetMapVersionFactory,
      FNameGenerator,
      FTileNameParser
    );
end;

end.
