unit u_MainFormMainConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MainFormConfig,
  u_ConfigDataElementBase;

type
  TMainFormMainConfig = class(TConfigDataElementBase, IMainFormMainConfig)
  private
    FShowMapName: Boolean;
    FMouseScrollInvert: Boolean;
    FShowHintOnMarks: Boolean;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetShowMapName: Boolean;
    procedure SetShowMapName(AValue: Boolean);

    function GetMouseScrollInvert: Boolean;
    procedure SetMouseScrollInvert(AValue: Boolean);

    function GetShowHintOnMarks: Boolean;
    procedure SetShowHintOnMarks(AValue: Boolean);
  public
    constructor Create;
  end;

implementation

{ TMainFormMainConfig }

constructor TMainFormMainConfig.Create;
begin
  inherited;
  FShowMapName := True;
  FMouseScrollInvert := False;
  FShowHintOnMarks := True;
end;

procedure TMainFormMainConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FShowMapName := AConfigData.ReadBool('ShowMapNameOnPanel', FShowMapName);
    FMouseScrollInvert := AConfigData.ReadBool('MouseScrollInvert', FMouseScrollInvert);
    FShowHintOnMarks := AConfigData.ReadBool('ShowHintOnMarks', FShowHintOnMarks);
    SetChanged;
  end;
end;

procedure TMainFormMainConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('ShowMapNameOnPanel', FShowMapName);
  AConfigData.WriteBool('MouseScrollInvert', FMouseScrollInvert);
  AConfigData.WriteBool('ShowHintOnMarks', FShowHintOnMarks);
end;

function TMainFormMainConfig.GetMouseScrollInvert: Boolean;
begin
  LockRead;
  try
    Result := FMouseScrollInvert;
  finally
    UnlockRead;
  end;
end;

function TMainFormMainConfig.GetShowHintOnMarks: Boolean;
begin
  LockRead;
  try
    Result := FShowHintOnMarks;
  finally
    UnlockRead;
  end;
end;

function TMainFormMainConfig.GetShowMapName: Boolean;
begin
  LockRead;
  try
    Result := FShowMapName;
  finally
    UnlockRead;
  end;
end;

procedure TMainFormMainConfig.SetMouseScrollInvert(AValue: Boolean);
begin
  LockWrite;
  try
    if FMouseScrollInvert <> AValue then begin
      FMouseScrollInvert := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMainFormMainConfig.SetShowHintOnMarks(AValue: Boolean);
begin
  LockWrite;
  try
    if FShowHintOnMarks <> AValue then begin
      FShowHintOnMarks := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMainFormMainConfig.SetShowMapName(AValue: Boolean);
begin
  LockWrite;
  try
    if FShowMapName <> AValue then begin
      FShowMapName := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
