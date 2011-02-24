unit u_TileStorageTypeListSimple;

interface

uses
  i_ITileStorageTypeConfig,
  i_ITileStorageType,
  u_TileStorageTypeList;

type
  TInternalTileStorageTypeInternal = class(TInterfacedObject, IInternalTileStorageTypeInternal)
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

  TTileStorageTypeListSimple = class(TTileStorageTypeList)
  public
    constructor Create;
  end;

implementation

{ TInternalTileStorageTypeInternal }

constructor TInternalTileStorageTypeInternal.Create(AGUID: TGUID;
  AStorageType: ITileStorageType; ACanUseAsDefault: Boolean;
  AConfig: ITileStorageTypeConfig);
begin
  FGUID := AGUID;
  FStorageType := AStorageType;
  FCanUseAsDefault := ACanUseAsDefault;
  FConfig := AConfig;
end;

function TInternalTileStorageTypeInternal.GetCanUseAsDefault: Boolean;
begin
  Result := FCanUseAsDefault;
end;

function TInternalTileStorageTypeInternal.GetConfig: ITileStorageTypeConfig;
begin
  Result := FConfig;
end;

function TInternalTileStorageTypeInternal.GetGUID: TGUID;
begin
  Result := FGUID;
end;

function TInternalTileStorageTypeInternal.GetStorageType: ITileStorageType;
begin
  Result := FStorageType;
end;

{ TTileStorageTypeListSimple }

constructor TTileStorageTypeListSimple.Create;
begin

end;

end.
