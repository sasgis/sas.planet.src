unit u_TileStorageTypeSQLite;

interface

uses
  i_ProjectionSet,
  i_ContentTypeInfo,
  i_ContentTypeManager,
  i_NotifierTime,
  i_NotifierTilePyramidUpdate,
  i_MapVersionFactory,
  i_ConfigDataProvider,
  i_TileStorage,
  i_TileStorageAbilities,
  i_TileStorageTypeConfig,
  i_TileInfoBasicMemCache,
  u_TileStorageTypeBase;

type
  TTileStorageTypeSQLite = class(TTileStorageTypeBase)
  private
    FGCNotifier: INotifierTime;
    FContentTypeManager: IContentTypeManager;
    FIsVersioned: Boolean;
  protected
    function BuildStorageInternal(
      const AStorageConfigData: IConfigDataProvider;
      const AForceAbilities: ITileStorageAbilities;
      const AProjectionSet: IProjectionSet;
      const AMainContentType: IContentTypeInfoBasic;
      const ATileNotifier: INotifierTilePyramidUpdateInternal;
      const APath: string;
      const ACacheTileInfo: ITileInfoBasicMemCache
    ): ITileStorage; override;
  public
    constructor Create(
      const AGCNotifier: INotifierTime;
      const AContentTypeManager: IContentTypeManager;
      const AMapVersionFactory: IMapVersionFactory;
      const AConfig: ITileStorageTypeConfig;
      const AIsVersioned: Boolean = True
    );
  end;

implementation

uses
  SysUtils,
  t_CommonTypes,
  u_TileStorageAbilities,
  u_TileStorageSQLite;

{ TTileStorageTypeSQLite }

constructor TTileStorageTypeSQLite.Create(
  const AGCNotifier: INotifierTime;
  const AContentTypeManager: IContentTypeManager;
  const AMapVersionFactory: IMapVersionFactory;
  const AConfig: ITileStorageTypeConfig;
  const AIsVersioned: Boolean
);
var
  VAbilities: ITileStorageTypeAbilities;
  VVersionSupport: TTileStorageTypeVersionSupport;
begin
  if AIsVersioned then begin
    VVersionSupport := tstvsMultiVersions;
  end else begin
    VVersionSupport := tstvsVersionStored;
  end;
  VAbilities :=
    TTileStorageTypeAbilities.Create(
      TTileStorageAbilities.Create(CTileStorageReadWrite),
      VVersionSupport,
      True,
      stsUnicode,
      tstcFolder
    );
  inherited Create(
    VAbilities,
    AMapVersionFactory,
    AConfig
  );
  FGCNotifier := AGCNotifier;
  FContentTypeManager := AContentTypeManager;
  FIsVersioned := AIsVersioned;
end;

function TTileStorageTypeSQLite.BuildStorageInternal(
  const AStorageConfigData: IConfigDataProvider;
  const AForceAbilities: ITileStorageAbilities;
  const AProjectionSet: IProjectionSet;
  const AMainContentType: IContentTypeInfoBasic;
  const ATileNotifier: INotifierTilePyramidUpdateInternal;
  const APath: string;
  const ACacheTileInfo: ITileInfoBasicMemCache
): ITileStorage;
begin
  Result :=
    TTileStorageSQLite.Create(
      ACacheTileInfo,
      AMainContentType,
      GetAbilities,
      AForceAbilities,
      GetMapVersionFactory,
      AProjectionSet,
      ATileNotifier,
      APath,
      FGCNotifier,
      FContentTypeManager,
      FIsVersioned
    );
end;

end.
