unit u_GlobalAppConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_GlobalAppConfig,
  u_ConfigDataElementBase;

type
  TGlobalAppConfig = class(TConfigDataElementBase, IGlobalAppConfig)
  private
    FIsShowLogo: Boolean;
    FIsShowIconInTray: Boolean;
    FIsSendStatistic: Boolean;
    FIsShowDebugInfo: Boolean;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetIsShowLogo: Boolean;
    procedure SetIsShowLogo(AValue: Boolean);

    function GetIsShowIconInTray: Boolean;
    procedure SetIsShowIconInTray(AValue: Boolean);

    function GetIsSendStatistic: Boolean;
    procedure SetIsSendStatistic(AValue: Boolean);

    function GetIsShowDebugInfo: Boolean;
    procedure SetIsShowDebugInfo(AValue: Boolean);
  public
    constructor Create;
  end;

implementation

{ TGlobalAppConfig }

constructor TGlobalAppConfig.Create;
begin
  inherited;
  FIsShowLogo := True;
  FIsShowIconInTray := False;

  {$IFDEF DEBUG}
    FIsShowDebugInfo := True;
  {$ELSE}
    FIsShowDebugInfo := False;
  {$ENDIF}
  FIsSendStatistic := True;
end;

procedure TGlobalAppConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FIsShowLogo := AConfigData.ReadBool('ShowLogo', FIsShowLogo);
    FIsShowIconInTray := AConfigData.ReadBool('ShowIconInTray', FIsShowIconInTray);
    FIsShowDebugInfo := AConfigData.ReadBool('ShowDebugInfo', FIsShowDebugInfo);
    FIsSendStatistic := AConfigData.ReadBool('SendStatistic', FIsSendStatistic);
    SetChanged;
  end;
end;

procedure TGlobalAppConfig.DoWriteConfig(AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('ShowLogo', FIsShowLogo);
  AConfigData.WriteBool('ShowIconInTray', FIsShowIconInTray);
end;

function TGlobalAppConfig.GetIsSendStatistic: Boolean;
begin
  LockRead;
  try
    Result := FIsSendStatistic;
  finally
    UnlockRead;
  end;
end;

function TGlobalAppConfig.GetIsShowDebugInfo: Boolean;
begin
  LockRead;
  try
    Result := FIsShowDebugInfo;
  finally
    UnlockRead;
  end;
end;

function TGlobalAppConfig.GetIsShowIconInTray: Boolean;
begin
  LockRead;
  try
    Result := FIsShowIconInTray;
  finally
    UnlockRead;
  end;
end;

function TGlobalAppConfig.GetIsShowLogo: Boolean;
begin
  LockRead;
  try
    Result := FIsShowLogo;
  finally
    UnlockRead;
  end;
end;

procedure TGlobalAppConfig.SetIsSendStatistic(AValue: Boolean);
begin
  LockWrite;
  try
    if FIsSendStatistic <> AValue then begin
      FIsSendStatistic := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TGlobalAppConfig.SetIsShowDebugInfo(AValue: Boolean);
begin
  LockWrite;
  try
    if FIsShowDebugInfo <> AValue then begin
      FIsShowDebugInfo := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TGlobalAppConfig.SetIsShowIconInTray(AValue: Boolean);
begin
  LockWrite;
  try
    if FIsShowIconInTray <> AValue then begin
      FIsShowIconInTray := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TGlobalAppConfig.SetIsShowLogo(AValue: Boolean);
begin
  LockWrite;
  try
    if FIsShowLogo <> AValue then begin
      FIsShowLogo := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
