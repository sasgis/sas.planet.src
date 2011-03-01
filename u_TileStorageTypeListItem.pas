unit u_TileStorageTypeListItem;

interface

uses
  i_ITileStorageTypeConfig,
  i_ITileStorageType,
  i_ITileStorageTypeListItem;

type
  TTileStorageTypeListItem = class(TInterfacedObject, ITileStorageTypeListItem)
  private
    FGUID: TGUID;
    FStorageType: ITileStorageType;
    FCanUseAsDefault: Boolean;
    FConfig: ITileStorageTypeConfig;
  protected
    function GetGUID: TGUID;
    function GetStorageType: ITileStorageType;
    function GetCanUseAsDefault: Boolean;
    function GetConfig: ITileStorageTypeConfig;
  public
    constructor Create(
      AGUID: TGUID;
      AStorageType: ITileStorageType;
      ACanUseAsDefault: Boolean;
      AConfig: ITileStorageTypeConfig
    );
  end;

implementation

{ TTileStorageTypeListItem }

constructor TTileStorageTypeListItem.Create(AGUID: TGUID;
  AStorageType: ITileStorageType; ACanUseAsDefault: Boolean;
  AConfig: ITileStorageTypeConfig);
begin
  FGUID := AGUID;
  FStorageType := AStorageType;
  FCanUseAsDefault := ACanUseAsDefault;
  FConfig := AConfig;
end;

function TTileStorageTypeListItem.GetCanUseAsDefault: Boolean;
begin
  Result := FCanUseAsDefault;
end;

function TTileStorageTypeListItem.GetConfig: ITileStorageTypeConfig;
begin
  Result := FConfig;
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
