unit u_TileStorageTypeBase;

interface

uses
  i_TileStorageTypeInfo,
  i_TileStorageTypeConfig,
  i_TileStorage,
  i_TileStorageType;

type
  TTileStorageTypeBase = class(TInterfacedObject, ITileStorageType)
  private
    FGUID: TGUID;
    FCaption: string;
    FInfo: ITileStorageTypeInfo;
    FConfig: ITileStorageTypeConfig;
  protected
    function GetGUID: TGUID;
    function GetInfo: ITileStorageTypeInfo;
    function GetConfig: ITileStorageTypeConfig;
    function BuildStorage(APath: string): ITileStorage; virtual; abstract;
    function GetCaption: string;
  public
    constructor Create(
      AGUID: TGUID;
      ACaption: string;
      AInfo: ITileStorageTypeInfo;
      AConfig: ITileStorageTypeConfig
    );
  end;

implementation

{ TTileStorageTypeBase }

constructor TTileStorageTypeBase.Create(
  AGUID: TGUID;
  ACaption: string;
  AInfo: ITileStorageTypeInfo;
  AConfig: ITileStorageTypeConfig
);
begin
  FGUID := AGUID;
  FCaption := ACaption;
  FInfo := AInfo;
  FConfig := AConfig;
end;

function TTileStorageTypeBase.GetCaption: string;
begin
  Result := FCaption;
end;

function TTileStorageTypeBase.GetConfig: ITileStorageTypeConfig;
begin
  Result := FConfig;
end;

function TTileStorageTypeBase.GetGUID: TGUID;
begin
  Result := FGUID;
end;

function TTileStorageTypeBase.GetInfo: ITileStorageTypeInfo;
begin
  Result := FInfo;
end;

end.
