unit u_GenShtabGridConfig;

interface

uses
  GR32,
  i_IConfigDataElement,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_MapLayerGridsConfig,
  u_BaseGridConfig;

type
  TGenShtabGridConfig = class(TBaseGridConfig, IGenShtabGridConfig)
  private
    FScale: Integer;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetScale: Integer;
    procedure SetScale(AValue: Integer);
  public
    constructor Create;
  end;

implementation

{ TGenShtabGridConfig }

constructor TGenShtabGridConfig.Create;
begin
  inherited;
  FScale := 0;
end;

procedure TGenShtabGridConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    SetScale(AConfigData.ReadInteger('Scale', FScale));
  end;
end;

procedure TGenShtabGridConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteInteger('Scale', FScale);
end;

function TGenShtabGridConfig.GetScale: Integer;
begin
  LockRead;
  try
    Result := FScale;
  finally
    UnlockRead;
  end;
end;

procedure TGenShtabGridConfig.SetScale(AValue: Integer);
var
  VScale: Integer;
begin
  VScale := AValue;
  if VScale >= 1000000 then begin
    VScale := 1000000;
  end else if VScale >= 500000 then begin
    VScale := 500000;
  end else if VScale >= 200000 then begin
    VScale := 200000;
  end else if VScale >= 100000 then begin
    VScale := 100000;
  end else if VScale >= 50000 then begin
    VScale := 50000;
  end else if VScale >= 25000 then begin
    VScale := 25000;
  end else if VScale >= 10000 then begin
    VScale := 10000;
  end else begin
    VScale := 0;
  end;
  LockWrite;
  try
    if FScale <> VScale then begin
      FScale := VScale;
      if FScale = 0 then begin
        SetVisible(False);
      end;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
