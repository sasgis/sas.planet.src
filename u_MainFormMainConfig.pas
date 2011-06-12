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
    FZoomingAtMousePos: Boolean;
    FShowMapName: Boolean;
    FMouseScrollInvert: Boolean;
    FAnimateZoom: Boolean;
    FAnimateZoomTime: Cardinal;
    FShowHintOnMarks: Boolean;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetZoomingAtMousePos: Boolean;
    procedure SetZoomingAtMousePos(AValue: Boolean);

    function GetShowMapName: Boolean;
    procedure SetShowMapName(AValue: Boolean);

    function GetMouseScrollInvert: Boolean;
    procedure SetMouseScrollInvert(AValue: Boolean);

    function GetAnimateZoom: Boolean;
    procedure SetAnimateZoom(AValue: Boolean);

    function GetAnimateZoomTime: Cardinal;
    procedure SetAnimateZoomTime(AValue: Cardinal);

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
  FZoomingAtMousePos := True;
  FShowMapName := True;
  FMouseScrollInvert := False;
  FAnimateZoom := True;
  FAnimateZoomTime := 500;
  FShowHintOnMarks := True;
end;

procedure TMainFormMainConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FZoomingAtMousePos := AConfigData.ReadBool('ZoomingAtMousePos', FZoomingAtMousePos);
    FShowMapName := AConfigData.ReadBool('ShowMapNameOnPanel', FShowMapName);
    FMouseScrollInvert := AConfigData.ReadBool('MouseScrollInvert', FMouseScrollInvert);
    FAnimateZoom := AConfigData.ReadBool('AnimateZoom', FAnimateZoom);
    FAnimateZoomTime := AConfigData.ReadInteger('AnimateZoomTime', FAnimateZoomTime);
    FShowHintOnMarks := AConfigData.ReadBool('ShowHintOnMarks', FShowHintOnMarks);
    SetChanged;
  end;
end;

procedure TMainFormMainConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('ZoomingAtMousePos', FZoomingAtMousePos);
  AConfigData.WriteBool('ShowMapNameOnPanel', FShowMapName);
  AConfigData.WriteBool('MouseScrollInvert', FMouseScrollInvert);
  AConfigData.WriteBool('AnimateZoom', FAnimateZoom);
  AConfigData.WriteInteger('AnimateZoomTime', FAnimateZoomTime);
  AConfigData.WriteBool('ShowHintOnMarks', FShowHintOnMarks);
end;

function TMainFormMainConfig.GetAnimateZoom: Boolean;
begin
  LockRead;
  try
    Result := FAnimateZoom;
  finally
    UnlockRead;
  end;
end;

function TMainFormMainConfig.GetAnimateZoomTime: Cardinal;
begin
  LockRead;
  try
    Result := FAnimateZoomTime;
  finally
    UnlockRead;
  end;
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

function TMainFormMainConfig.GetZoomingAtMousePos: Boolean;
begin
  LockRead;
  try
    Result := FZoomingAtMousePos;
  finally
    UnlockRead;
  end;
end;

procedure TMainFormMainConfig.SetAnimateZoom(AValue: Boolean);
begin
  LockWrite;
  try
    if FAnimateZoom <> AValue then begin
      FAnimateZoom := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMainFormMainConfig.SetAnimateZoomTime(AValue: Cardinal);
begin
  LockWrite;
  try
    if FAnimateZoomTime <> AValue then begin
      FAnimateZoomTime := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
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

procedure TMainFormMainConfig.SetZoomingAtMousePos(AValue: Boolean);
begin
  LockWrite;
  try
    if FZoomingAtMousePos <> AValue then begin
      FZoomingAtMousePos := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
