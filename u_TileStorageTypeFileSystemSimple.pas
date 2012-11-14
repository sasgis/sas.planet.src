unit u_TileStorageTypeFileSystemSimple;

interface

uses
  i_CoordConverter,
  i_ContentTypeInfo,
  i_TileFileNameGenerator,
  i_TileStorage,
  i_TileStorageTypeConfig,
  u_TileStorageTypeBase;

type
  TTileStorageTypeFileSystemSimple = class(TTileStorageTypeBase)
  private
    FNameGenerator: ITileFileNameGenerator;
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
      const ANameGenerator: ITileFileNameGenerator;
      const AConfig: ITileStorageTypeConfig
    );
  end;

implementation

uses
  u_TileStorageTypeAbilities,
  u_MapVersionFactorySimpleString;

{ TTileStorageTypeFileSystemSimple }

constructor TTileStorageTypeFileSystemSimple.Create(
  const AGUID: TGUID;
  const ACaption: string;
  const ANameGenerator: ITileFileNameGenerator;
  const AConfig: ITileStorageTypeConfig
);
begin
  inherited Create(
    AGUID,
    ACaption,
    TTileStorageTypeAbilitiesFileFolder.Create,
    TMapVersionFactorySimpleString.Create,
    AConfig
  );
  FNameGenerator := ANameGenerator;
end;

function TTileStorageTypeFileSystemSimple.BuildStorage(
  const AGeoConverter: ICoordConverter;
  const AMainContentType: IContentTypeInfoBasic;
  const APath: string
): ITileStorage;
begin
  Assert(False);
  //TODO: Написать создание хранилища
end;

end.
