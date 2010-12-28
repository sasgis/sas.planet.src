unit u_TileGridConfig;

interface

uses
  GR32,
  i_IConfigDataElement,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_MapLayerGridsConfig,
  u_BaseGridConfig;

type
  TTileGridConfig = class(TBaseGridConfig, ITileGridConfig)
  private
    FUseRelativeZoom: Boolean;
    FZoom: Integer;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetUseRelativeZoom: Boolean;
    procedure SetUseRelativeZoom(AValue: Boolean);

    function GetZoom: Integer;
    procedure SetZoom(AValue: Integer);
  public
    constructor Create;
  end;

implementation

{ TTileGridConfig }

constructor TTileGridConfig.Create;
begin
  inherited;
  FUseRelativeZoom := True;
  FZoom := 0;
end;

procedure TTileGridConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FUseRelativeZoom := AConfigData.ReadBool('UseRelativeZoom', FUseRelativeZoom);
    FZoom := AConfigData.ReadInteger('Zoom', FZoom);
  end;
end;

procedure TTileGridConfig.DoWriteConfig(AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('UseRelativeZoom', FUseRelativeZoom);
  AConfigData.WriteInteger('Zoom', FZoom);
end;

function TTileGridConfig.GetUseRelativeZoom: Boolean;
begin
  LockRead;
  try
    Result := FUseRelativeZoom;
  finally
    UnlockRead;
  end;
end;

function TTileGridConfig.GetZoom: Integer;
begin
  LockRead;
  try
    Result := FZoom;
  finally
    UnlockRead;
  end;
end;

procedure TTileGridConfig.SetUseRelativeZoom(AValue: Boolean);
begin
  LockWrite;
  try
    if FUseRelativeZoom <> AValue then begin
      FUseRelativeZoom := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TTileGridConfig.SetZoom(AValue: Integer);
begin
  LockWrite;
  try
    if FZoom <> AValue then begin
      FZoom := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
