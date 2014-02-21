unit u_TileStorageTypeGoogleEarth;

interface

uses
  i_CoordConverter,
  i_ContentTypeInfo,
  i_MapVersionFactory,
  i_ConfigDataProvider,
  i_TileStorage,
  i_TileStorageAbilities,
  i_TileInfoBasicMemCache,
  i_TileStorageTypeConfig,
  u_TileStorageTypeBase;

type
  TTileStorageTypeGoogleEarth = class(TTileStorageTypeBase)
  private
    FIsTerrainStorage: Boolean;
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
      const AMapVersionFactory: IMapVersionFactory;
      const AIsTerrainStorage: Boolean;
      const AConfig: ITileStorageTypeConfig
    );
  end;

implementation

uses
  SysUtils,
  c_CoordConverter,
  u_TileStorageAbilities,
  u_TileStorageGoogleEarth;

{ TTileStorageTypeGoogleEarth }

constructor TTileStorageTypeGoogleEarth.Create(
  const AMapVersionFactory: IMapVersionFactory;
  const AIsTerrainStorage: Boolean;
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
  FIsTerrainStorage := AIsTerrainStorage;
end;

function TTileStorageTypeGoogleEarth.BuildStorageInternal(
  const AStorageConfigData: IConfigDataProvider;
  const AForceAbilities: ITileStorageAbilities;
  const AGeoConverter: ICoordConverter;
  const AMainContentType: IContentTypeInfoBasic;
  const APath: string;
  const ACacheTileInfo: ITileInfoBasicMemCache
): ITileStorage;
var
  VNameInCache: string;
begin
  Result := nil;
  if
    (AGeoConverter.GetProjectionEPSG = CGELonLatProjectionEPSG) and
    (AGeoConverter.GetTileSplitCode = CTileSplitQuadrate256x256)
  then begin
    VNameInCache := ExtractFileName(APath);
    if SameText(VNameInCache, 'earth')  then begin
      VNameInCache := 'earth';
    end else if SameText(VNameInCache, 'mars')  then begin
      VNameInCache := 'mars';
    end else if SameText(VNameInCache, 'moon')  then begin
      VNameInCache := 'moon';
    end else if SameText(VNameInCache, 'sky')  then begin
      VNameInCache := 'sky';
    end else begin
      VNameInCache := '';
    end;

    Result :=
      TTileStorageGoogleEarth.Create(
        GetAbilities,
        AForceAbilities,
        AGeoConverter,
        GetConfig.BasePath.FullPath,
        VNameInCache,
        FIsTerrainStorage,
        ACacheTileInfo,
        GetMapVersionFactory,
        AMainContentType
      );
  end;
end;

end.
