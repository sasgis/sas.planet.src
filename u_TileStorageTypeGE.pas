unit u_TileStorageTypeGE;

interface

uses
  i_TileStorage,
  i_TileStorageTypeConfig,
  u_TileStorageTypeBase;

type
  TTileStorageTypeGE = class(TTileStorageTypeBase)
  protected
    function BuildStorage(APath: string): ITileStorage; override;
  public
    constructor Create(
      AGUID: TGUID;
      ACaption: string;
      AConfig: ITileStorageTypeConfig
    );
  end;

implementation

uses
  u_TileStorageTypeAbilities,
  u_MapVersionFactoryGE;

{ TTileStorageTypeGE }

constructor TTileStorageTypeGE.Create(
  AGUID: TGUID;
  ACaption: string;
  AConfig: ITileStorageTypeConfig
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

function TTileStorageTypeGE.BuildStorage(APath: string): ITileStorage;
begin
  Assert(False);
  //TODO: Написать создание хранилища
end;

end.
