unit u_BaseGridConfig;

interface

uses
  GR32,
  i_IConfigDataElement,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_MapLayerGridsConfig,
  u_ConfigDataElementBase;

type
  TBaseGridConfig = class(TConfigDataElementBase, IBaseGridConfig)
  private
    FVisible: Boolean;
    FGridColor: TColor32;
    FShowText: Boolean;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);

    function GetGridColor: TColor32;
    procedure SetGridColor(AValue: TColor32);

    function GetShowText: Boolean;
    procedure SetShowText(AValue: Boolean);
  public
    constructor Create;
  end;

implementation

uses
  u_ConfigProviderHelpers;

{ TBaseGridConfig }

constructor TBaseGridConfig.Create;
begin
  inherited;
  FVisible := False;
  FShowText := True;
  FGridColor := SetAlpha(clWhite32, 150);
end;

procedure TBaseGridConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FVisible := AConfigData.ReadBool('Visible', FVisible);
    FShowText := AConfigData.ReadBool('ShowText', FShowText);
    LoadColor32(AConfigData, 'GridColor', FGridColor);
    SetChanged;
  end;
end;

procedure TBaseGridConfig.DoWriteConfig(AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('Visible', FVisible);
  AConfigData.WriteBool('ShowText', FShowText);
  WriteColor32(AConfigData, 'GridColor', FGridColor);
end;

function TBaseGridConfig.GetGridColor: TColor32;
begin
  LockRead;
  try
    Result := FGridColor;
  finally
    UnlockRead;
  end;
end;

function TBaseGridConfig.GetShowText: Boolean;
begin
  LockRead;
  try
    Result := FShowText;
  finally
    UnlockRead;
  end;
end;

function TBaseGridConfig.GetVisible: Boolean;
begin
  LockRead;
  try
    Result := FVisible;
  finally
    UnlockRead;
  end;
end;

procedure TBaseGridConfig.SetGridColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FGridColor <> AValue then begin
      FGridColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TBaseGridConfig.SetShowText(AValue: Boolean);
begin
  LockWrite;
  try
    if FShowText <> AValue then begin
      FShowText := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TBaseGridConfig.SetVisible(AValue: Boolean);
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
