unit u_TileStorageTypeGE;

interface

uses
  i_TileStorage,
  i_TileStorageTypeConfig,
  u_TileStorageTypeBase;

type
  TTileStorageTypeGE = class(TTileStorageTypeBase)
  protected
    function BuildStorage(const APath: string): ITileStorage; override;
  public
    constructor Create(
      const AGUID: TGUID;
      const ACaption: string;
      const AConfig: ITileStorageTypeConfig
    );
  end;

implementation

uses
  u_TileStorageTypeAbilities,
  u_MapVersionFactoryGE;

{ TTileStorageTypeGE }

constructor TTileStorageTypeGE.Create(
  const AGUID: TGUID;
  const ACaption: string;
  const AConfig: ITileStorageTypeConfig
);
begin
  inherited Create(
    AGUID,
    ACaption,
    TTileStorageTypeAbilitiesGE.Create,
    TMapVersionFactoryGE.Create,
    AConfig
  );
end;

function TTileStorageTypeGE.BuildStorage(const APath: string): ITileStorage;
begin
  Assert(False);
  //TODO: Написать создание хранилища
end;

end.
