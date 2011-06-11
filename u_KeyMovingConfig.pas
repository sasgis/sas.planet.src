unit u_KeyMovingConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_KeyMovingConfig,
  u_ConfigDataElementBase;

type
  TKeyMovingConfig = class(TConfigDataElementBase, IKeyMovingConfig)
  private
    FMinPixelPerSecond: Double;
    FMaxPixelPerSecond: Double;
    FSpeedChangeTime: Double;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetMinPixelPerSecond: Double;
    procedure SetMinPixelPerSecond(AValue: Double);

    function GetMaxPixelPerSecond: Double;
    procedure SetMaxPixelPerSecond(AValue: Double);

    function GetSpeedChangeTime: Double;
    procedure SetSpeedChangeTime(AValue: Double);
  public
    constructor Create;
  end;

implementation

{ TKeyMovingConfig }

constructor TKeyMovingConfig.Create;
begin
  inherited;
  FMinPixelPerSecond := 10;
  FMaxPixelPerSecond := 512;
  FSpeedChangeTime := 30;
end;

procedure TKeyMovingConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FMinPixelPerSecond := AConfigData.ReadFloat('MinPixelPerSecond', FMinPixelPerSecond);
    FMaxPixelPerSecond := AConfigData.ReadFloat('MaxPixelPerSecond', FMaxPixelPerSecond);
    FSpeedChangeTime := AConfigData.ReadFloat('SpeedChangeTime', FSpeedChangeTime);
    SetChanged;
  end;
end;

procedure TKeyMovingConfig.DoWriteConfig(AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteFloat('MinPixelPerSecond', FMinPixelPerSecond);
  AConfigData.WriteFloat('MaxPixelPerSecond', FMaxPixelPerSecond);
  AConfigData.WriteFloat('SpeedChangeTime', FSpeedChangeTime);
end;

function TKeyMovingConfig.GetMaxPixelPerSecond: Double;
begin
  LockRead;
  try
    Result := FMaxPixelPerSecond;
  finally
    UnlockRead;
  end;
end;

function TKeyMovingConfig.GetMinPixelPerSecond: Double;
begin
  LockRead;
  try
    Result := FMinPixelPerSecond;
  finally
    UnlockRead;
  end;
end;

function TKeyMovingConfig.GetSpeedChangeTime: Double;
begin
  LockRead;
  try
    Result := FSpeedChangeTime;
  finally
    UnlockRead;
  end;
end;

procedure TKeyMovingConfig.SetMaxPixelPerSecond(AValue: Double);
begin
  LockWrite;
  try
    if FMaxPixelPerSecond <> AValue then begin
      FMaxPixelPerSecond := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TKeyMovingConfig.SetMinPixelPerSecond(AValue: Double);
begin
  LockWrite;
  try
    if FMinPixelPerSecond <> AValue then begin
      FMinPixelPerSecond := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TKeyMovingConfig.SetSpeedChangeTime(AValue: Double);
begin
  LockWrite;
  try
    if FSpeedChangeTime <> AValue then begin
      FSpeedChangeTime := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
