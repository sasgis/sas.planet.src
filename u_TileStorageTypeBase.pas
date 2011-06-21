unit u_TileStorageTypeBase;

interface

uses
  i_ConfigDataElement,
  i_TileStorageTypeInfo,
  i_TileStorageTypeConfig,
  i_TileStorage,
  i_TileStorageType;

type
  TTileStorageTypeBase = class(TInterfacedObject, ITileStorageType)
  private
    FCaption: string;
    FInfo: ITileStorageTypeInfo;
    FConfig: ITileStorageTypeConfig;
  protected
    function GetInfo: ITileStorageTypeInfo;
    function GetConfig: ITileStorageTypeConfig;
    function BuildStorage(APath: string): ITileStorage; virtual; abstract;
    function GetCaption: string;
  public
    constructor Create(
      ACaption: string;
      AInfo: ITileStorageTypeInfo;
      AConfig: ITileStorageTypeConfig
    );
  end;

implementation

{ TTileStorageTypeBase }

constructor TTileStorageTypeBase.Create(ACaption: string;
  AInfo: ITileStorageTypeInfo; AConfig: ITileStorageTypeConfig);
begin
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

function TTileStorageTypeBase.GetInfo: ITileStorageTypeInfo;
begin
  Result := FInfo;
end;

end.
