unit u_TileStorageTypeConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_TileStorageTypeConfig,
  u_ConfigDataElementBase;

type
  TTileStorageTypeConfig = class(TConfigDataElementBase, ITileStorageTypeConfig)
  private
    FBasePath: string;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetBasePath: string;
    procedure SetBasePath(AValue: string);
  public
    constructor Create(ABasePath: string);
  end;

implementation

{ TTileStorageTypeConfig }

constructor TTileStorageTypeConfig.Create(ABasePath: string);
begin
  inherited Create;
  FBasePath := ABasePath;
end;

procedure TTileStorageTypeConfig.DoReadConfig(
  AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FBasePath := AConfigData.ReadString('Path', FBasePath);
    SetChanged;
  end;
end;

procedure TTileStorageTypeConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteString('Path', FBasePath);
end;

function TTileStorageTypeConfig.GetBasePath: string;
begin
  LockRead;
  try
    Result := FBasePath;
  finally
    UnlockRead;
  end;
end;

procedure TTileStorageTypeConfig.SetBasePath(AValue: string);
begin
  LockWrite;
  try
    if FBasePath <> AValue then begin
      FBasePath := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
