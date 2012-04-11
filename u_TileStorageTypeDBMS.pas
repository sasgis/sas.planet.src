unit u_TileStorageTypeDBMS;

interface

uses
  i_TileStorage,
  i_TileStorageTypeConfig,
  u_TileStorageTypeBase;

type
  TTileStorageTypeDBMS = class(TTileStorageTypeBase)
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
  u_MapVersionFactorySimpleString;


{ TTileStorageTypeDBMS }

constructor TTileStorageTypeDBMS.Create(
  const AGUID: TGUID;
  const ACaption: string;
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
end;

function TTileStorageTypeDBMS.BuildStorage(const APath: string): ITileStorage;
begin
  Assert(False);
  //TODO: Написать создание хранилища
end;

end.
