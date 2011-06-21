unit u_TileStorageTypeListItem;

interface

uses
  i_TileStorageTypeConfig,
  i_TileStorageType,
  i_TileStorageTypeListItem;

type
  TTileStorageTypeListItem = class(TInterfacedObject, ITileStorageTypeListItem)
  private
    FGUID: TGUID;
    FStorageType: ITileStorageType;
    FCanUseAsDefault: Boolean;
  protected
    function GetGUID: TGUID;
    function GetStorageType: ITileStorageType;
    function GetCanUseAsDefault: Boolean;
  public
    constructor Create(
      AGUID: TGUID;
      AStorageType: ITileStorageType;
      ACanUseAsDefault: Boolean
    );
  end;

implementation

{ TTileStorageTypeListItem }

constructor TTileStorageTypeListItem.Create(
  AGUID: TGUID;
  AStorageType: ITileStorageType;
  ACanUseAsDefault: Boolean
);
begin
  FGUID := AGUID;
  FStorageType := AStorageType;
  FCanUseAsDefault := ACanUseAsDefault;
end;

function TTileStorageTypeListItem.GetCanUseAsDefault: Boolean;
begin
  Result := FCanUseAsDefault;
end;

function TTileStorageTypeListItem.GetGUID: TGUID;
begin
  Result := FGUID;
end;

function TTileStorageTypeListItem.GetStorageType: ITileStorageType;
begin
  Result := FStorageType;
end;

end.
