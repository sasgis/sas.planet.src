unit u_MapLayerGPSTrackConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MapLayerGPSTrackConfig,
  u_ConfigDataElementBase;

type
  TMapLayerGPSTrackConfig = class(TConfigDataElementBase, IMapLayerGPSTrackConfig)
  private
    FVisible: Boolean;
    FLineWidth: Double;
    FLastPointCount: Integer;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);

    function GetLineWidth: Double;
    procedure SetLineWidth(AValue: Double);

    function GetLastPointCount: Integer;
    procedure SetLastPointCount(AValue: Integer);
  public
    constructor Create;
  end;

implementation

{ TMapLayerGPSTrackConfig }

constructor TMapLayerGPSTrackConfig.Create;
begin
  inherited;
  FVisible := True;
  FLineWidth := 5;
  FLastPointCount := 5000;
end;

procedure TMapLayerGPSTrackConfig.DoReadConfig(
  AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FVisible := AConfigData.ReadBool('Visible', FVisible);
    FLineWidth := AConfigData.ReadFloat('LineWidth', FLineWidth);
    FLastPointCount := AConfigData.ReadInteger('LastPointsCount', FLastPointCount);
    SetChanged;
  end;
end;

procedure TMapLayerGPSTrackConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('Visible', FVisible);
  AConfigData.WriteFloat('LineWidth', FLineWidth);
  AConfigData.WriteInteger('LastPointsCount', FLastPointCount);
end;

function TMapLayerGPSTrackConfig.GetLastPointCount: Integer;
begin
  LockRead;
  try
    Result := FLastPointCount;
  finally
    UnlockRead;
  end;
end;

function TMapLayerGPSTrackConfig.GetLineWidth: Double;
begin
  LockRead;
  try
    Result := FLineWidth;
  finally
    UnlockRead;
  end;
end;

function TMapLayerGPSTrackConfig.GetVisible: Boolean;
begin
  LockRead;
  try
    Result := FVisible;
  finally
    UnlockRead;
  end;
end;

procedure TMapLayerGPSTrackConfig.SetLastPointCount(AValue: Integer);
begin
  LockWrite;
  try
    if FLastPointCount <> AValue then begin
      FLastPointCount := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapLayerGPSTrackConfig.SetLineWidth(AValue: Double);
begin
  LockWrite;
  try
    if FLineWidth <> AValue then begin
      FLineWidth := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapLayerGPSTrackConfig.SetVisible(AValue: Boolean);
begin
  LockWrite;
  try
    if FVisible <> AValue then begin
      FVisible := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
