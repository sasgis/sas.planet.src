unit u_TileStorageTypeBerkeleyDB;

interface

uses
  i_CoordConverter,
  i_ContentTypeInfo,
  i_ContentTypeManager,
  i_NotifierTTLCheck,
  i_TileStorage,
  i_TileStorageTypeConfig,
  u_TileStorageTypeBase;

type
  TTileStorageTypeBerkeleyDB = class(TTileStorageTypeBase)
  private
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
      const AGUID: TGUID;
      const ACaption: string;
      const AGCList: INotifierTTLCheck;
      const AContentTypeManager: IContentTypeManager;
      const AConfig: ITileStorageTypeConfig
    );
  end;

implementation

uses
  u_TileStorageTypeAbilities,
  u_MapVersionFactorySimpleString,
  u_TileStorageBerkeleyDB;

{ TTileStorageTypeBerkeleyDB }

constructor TTileStorageTypeBerkeleyDB.Create(
  const AGUID: TGUID;
  const ACaption: string;
  const AGCList: INotifierTTLCheck;
  const AContentTypeManager: IContentTypeManager;
  const AConfig: ITileStorageTypeConfig
);
begin
  inherited Create(
    AGUID,
    ACaption,
    TTileStorageTypeAbilitiesBerkeleyDB.Create,
    TMapVersionFactorySimpleString.Create,
    AConfig
  );
  FGCList := AGCList;
  FContentTypeManager := AContentTypeManager;
end;

function TTileStorageTypeBerkeleyDB.BuildStorage(
  const AGeoConverter: ICoordConverter;
  const AMainContentType: IContentTypeInfoBasic;
  const APath: string
): ITileStorage;
begin
  Result :=
    TTileStorageBerkeleyDB.Create(
      AGeoConverter,
      APath,
      FGCList,
      True,
      FContentTypeManager,
      GetMapVersionFactory,
      AMainContentType
    );
end;

end.
