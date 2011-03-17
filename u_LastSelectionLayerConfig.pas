unit u_LastSelectionLayerConfig;

interface

uses
  GR32,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_ILastSelectionLayerConfig,
  u_ConfigDataElementBase;

type
  TLastSelectionLayerConfig = class(TConfigDataElementBase, ILastSelectionLayerConfig)
  private
    FVisible: Boolean;
    FLineWidth: Integer;
    FLineColor: TColor32;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);

    function GetLineWidth: Integer;
    procedure SetLineWidth(AValue: Integer);

    function GetLineColor: TColor32;
    procedure SetLineColor(AValue: TColor32);
  public
    constructor Create;
  end;

implementation

uses
  u_ConfigProviderHelpers;

{ TLastSelectionLayerConfig }

constructor TLastSelectionLayerConfig.Create;
begin
  inherited;
  FVisible := True;
  FLineColor := SetAlpha(clBlack32, 210);
  FLineWidth := 2;
end;

procedure TLastSelectionLayerConfig.DoReadConfig(
  AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FVisible := AConfigData.ReadBool('Visible', FVisible);
    FLineColor := ReadColor32(AConfigData, 'LineColor', FLineColor);
    FLineWidth := AConfigData.ReadInteger('LineWidth', FLineWidth);
    SetChanged;
  end;
end;

procedure TLastSelectionLayerConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('Visible', FVisible);
  WriteColor32(AConfigData, 'LineColor', FLineColor);
  AConfigData.WriteInteger('LineWidth', FLineWidth);
end;

function TLastSelectionLayerConfig.GetLineColor: TColor32;
begin
  LockRead;
  try
    Result := FLineColor;
  finally
    UnlockRead;
  end;
end;

function TLastSelectionLayerConfig.GetLineWidth: Integer;
begin
  LockRead;
  try
    Result := FLineWidth;
  finally
    UnlockRead;
  end;
end;

function TLastSelectionLayerConfig.GetVisible: Boolean;
begin
  LockRead;
  try
    Result := FVisible;
  finally
    UnlockRead;
  end;
end;

procedure TLastSelectionLayerConfig.SetLineColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FLineColor <> AValue then begin
      FLineColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TLastSelectionLayerConfig.SetLineWidth(AValue: Integer);
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

procedure TLastSelectionLayerConfig.SetVisible(AValue: Boolean);
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
