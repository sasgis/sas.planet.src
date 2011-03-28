unit u_FillingMapLayerConfig;

interface

uses
  GR32,
  i_ActiveMapsConfig,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_FillingMapLayerConfig,
  u_ConfigDataElementComplexBase;

type
  TFillingMapLayerConfig = class(TConfigDataElementComplexBase, IFillingMapLayerConfig)
  private
    FVisible: Boolean;
    FSourceZoom: Byte;
    FNoTileColor: TColor32;
    FShowTNE: Boolean;
    FTNEColor: TColor32;
    FSourceMap: IFillingMapMapsConfig;
    FStatic: IFillingMapLayerConfigStatic;
    function CreateStatic: IFillingMapLayerConfigStatic;
  protected
    procedure SetChanged; override;
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);

    function GetSourceZoom: Byte;
    procedure SetSourceZoom(AValue: Byte);

    function GetNoTileColor: TColor32;
    procedure SetNoTileColor(AValue: TColor32);

    function GetShowTNE: Boolean;
    procedure SetShowTNE(AValue: Boolean);

    function GetTNEColor: TColor32;
    procedure SetTNEColor(AValue: TColor32);

    function GetSourceMap: IFillingMapMapsConfig;
    function GetStatic: IFillingMapLayerConfigStatic;
  public
    constructor Create(AMapsConfig: IMainMapsConfig);
  end;

implementation

uses
  u_ConfigSaveLoadStrategyBasicUseProvider,
  u_ConfigProviderHelpers,
  u_FillingMapMapsConfig,
  u_FillingMapLayerConfigStatic;

{ TFillingMapLayerConfig }

constructor TFillingMapLayerConfig.Create(AMapsConfig: IMainMapsConfig);
begin
  inherited Create;
  FVisible := False;
  FSourceZoom := 0;
  FShowTNE := True;
  FNoTileColor := SetAlpha(clBlack32, 110);
  FTNEColor := SetAlpha(clRed32, 110);

  FSourceMap := TFillingMapMapsConfig.Create(AMapsConfig);
  Add(FSourceMap, TConfigSaveLoadStrategyBasicUseProvider.Create);
  FStatic := CreateStatic;
end;

function TFillingMapLayerConfig.CreateStatic: IFillingMapLayerConfigStatic;
begin
  Result :=
    TFillingMapLayerConfigStatic.Create(
      FVisible,
      FSourceMap.GetActualMap,
      FSourceZoom,
      FNoTileColor,
      FShowTNE,
      FTNEColor
    );
end;

procedure TFillingMapLayerConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FVisible := AConfigData.ReadBool('Visible', FVisible);
    FSourceZoom := AConfigData.ReadInteger('SourceZoom', FSourceZoom);
    FShowTNE := AConfigData.ReadBool('ShowTNE', FShowTNE);
    FNoTileColor := ReadColor32(AConfigData, 'NoTileColor', FNoTileColor);
    FTNEColor := ReadColor32(AConfigData, 'TNEColor', FTNEColor);

    SetChanged;
  end;
end;

procedure TFillingMapLayerConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('Visible', FVisible);
  AConfigData.WriteInteger('SourceZoom', FSourceZoom);
  AConfigData.WriteBool('ShowTNE', FShowTNE);
  WriteColor32(AConfigData, 'NoTileColor', FNoTileColor);
  WriteColor32(AConfigData, 'TNEColor', FTNEColor);
end;

function TFillingMapLayerConfig.GetNoTileColor: TColor32;
begin
  LockRead;
  try
    Result := FNoTileColor;
  finally
    UnlockRead;
  end;
end;

function TFillingMapLayerConfig.GetShowTNE: Boolean;
begin
  LockRead;
  try
    Result := FShowTNE;
  finally
    UnlockRead;
  end;
end;

function TFillingMapLayerConfig.GetSourceMap: IFillingMapMapsConfig;
begin
  Result := FSourceMap;
end;

function TFillingMapLayerConfig.GetSourceZoom: Byte;
begin
  LockRead;
  try
    Result := FSourceZoom;
  finally
    UnlockRead;
  end;
end;

function TFillingMapLayerConfig.GetStatic: IFillingMapLayerConfigStatic;
begin
  LockRead;
  try
    Result := FStatic;
  finally
    UnlockRead;
  end;
end;

function TFillingMapLayerConfig.GetTNEColor: TColor32;
begin
  LockRead;
  try
    Result := FTNEColor;
  finally
    UnlockRead;
  end;
end;

function TFillingMapLayerConfig.GetVisible: Boolean;
begin
  LockRead;
  try
    Result := FVisible;
  finally
    UnlockRead;
  end;
end;

procedure TFillingMapLayerConfig.SetChanged;
begin
  inherited;
  LockWrite;
  try
    FStatic := CreateStatic;
  finally
    UnlockWrite;
  end;
end;

procedure TFillingMapLayerConfig.SetNoTileColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FNoTileColor <> AValue then begin
      FNoTileColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TFillingMapLayerConfig.SetShowTNE(AValue: Boolean);
begin
  LockWrite;
  try
    if FShowTNE <> AValue then begin
      FShowTNE := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TFillingMapLayerConfig.SetSourceZoom(AValue: Byte);
begin
  LockWrite;
  try
    if FSourceZoom <> AValue then begin
      FSourceZoom := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TFillingMapLayerConfig.SetTNEColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FTNEColor <> AValue then begin
      FTNEColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TFillingMapLayerConfig.SetVisible(AValue: Boolean);
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
