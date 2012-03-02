unit u_TileStorageTypeDBMS;

interface

uses
  i_TileStorage,
  i_TileStorageTypeConfig,
  u_TileStorageTypeBase;

type
  TTileStorageTypeDBMS = class(TTileStorageTypeBase)
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
  u_MapVersionFactorySimpleString;


{ TTileStorageTypeDBMS }

constructor TTileStorageTypeDBMS.Create(AGUID: TGUID; ACaption: string;
  AConfig: ITileStorageTypeConfig);
begin
  inherited Create(
    AGUID,
    ACaption,
    TTileStorageTypeAbilitiesBerkeleyDB.Create,
    TMapVersionFactorySimpleString.Create,
    AConfig
  );
end;

function TTileStorageTypeDBMS.BuildStorage(APath: string): ITileStorage;
begin
  Assert(False);
  //TODO: Написать создание хранилища
end;

end.
