unit u_FullMapMouseCursorLayerConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_FullMapMouseCursorLayerConfig,
  u_ConfigDataElementBase;

type
  TFullMapMouseCursorLayerConfig = class(TConfigDataElementBase, IFullMapMouseCursorLayerConfig)
  private
    FEnabled: Boolean;
    FShowAlways: Boolean;
  private
    function GetEnabled: Boolean;
    procedure SetEnabled(AValue: Boolean);

    function GetShowAlways: Boolean;
    procedure SetShowAlways(AValue: Boolean);
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  public
    constructor Create;
  end;

implementation

{ TFullMapMouseCursorLayerConfig }

constructor TFullMapMouseCursorLayerConfig.Create;
begin
  inherited;
  FEnabled := False;
  FShowAlways := False;
end;

procedure TFullMapMouseCursorLayerConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FEnabled := AConfigData.ReadBool('Enabled', FEnabled);
    FShowAlways := AConfigData.ReadBool('ShowAlways', FShowAlways);

    SetChanged;
  end;
end;

procedure TFullMapMouseCursorLayerConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('Enabled', FEnabled);
  AConfigData.WriteBool('ShowAlways', FShowAlways);
end;

function TFullMapMouseCursorLayerConfig.GetEnabled: Boolean;
begin
  LockRead;
  try
    Result := FEnabled;
  finally
    UnlockRead;
  end;
end;

function TFullMapMouseCursorLayerConfig.GetShowAlways: Boolean;
begin
  LockRead;
  try
    Result := FShowAlways;
  finally
    UnlockRead;
  end;
end;

procedure TFullMapMouseCursorLayerConfig.SetEnabled(AValue: Boolean);
begin
  LockWrite;
  try
    if FEnabled <> AValue then begin
      FEnabled := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TFullMapMouseCursorLayerConfig.SetShowAlways(AValue: Boolean);
begin
  LockWrite;
  try
    if FShowAlways <> AValue then begin
      FShowAlways := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
