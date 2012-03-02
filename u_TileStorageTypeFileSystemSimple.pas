unit u_TileStorageTypeFileSystemSimple;

interface

uses
  i_TileFileNameGenerator,
  i_TileStorage,
  i_TileStorageTypeConfig,
  u_TileStorageTypeBase;

type
  TTileStorageTypeFileSystemSimple = class(TTileStorageTypeBase)
  private
    FNameGenerator: ITileFileNameGenerator;
  protected
    function BuildStorage(APath: string): ITileStorage; override;
  public
    constructor Create(
      AGUID: TGUID;
      ACaption: string;
      ANameGenerator: ITileFileNameGenerator;
      AConfig: ITileStorageTypeConfig
    );
  end;

implementation

uses
  u_TileStorageTypeAbilities,
  u_MapVersionFactorySimpleString;

{ TTileStorageTypeFileSystemSimple }

constructor TTileStorageTypeFileSystemSimple.Create(
  AGUID: TGUID;
  ACaption: string;
  ANameGenerator: ITileFileNameGenerator;
  AConfig: ITileStorageTypeConfig
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
  APath: string
): ITileStorage;
begin
  Assert(False);
  //TODO: Написать создание хранилища
end;

end.
