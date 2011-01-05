unit u_MainFormMainConfig;

interface

uses
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_MainFormConfig,
  u_ConfigDataElementBase;

type
  TMainFormMainConfig = class(TConfigDataElementBase, IMainFormMainConfig)
  private
    FZoomingAtMousePos: Boolean;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetZoomingAtMousePos: Boolean;
    procedure SetZoomingAtMousePos(AValue: Boolean);
  public
    constructor Create;
  end;

implementation

{ TMainFormMainConfig }

constructor TMainFormMainConfig.Create;
begin
  inherited;
  FZoomingAtMousePos := True;
end;

procedure TMainFormMainConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FZoomingAtMousePos := AConfigData.ReadBool('ZoomingAtMousePos', FZoomingAtMousePos);
    SetChanged;
  end;
end;

procedure TMainFormMainConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('ZoomingAtMousePos', FZoomingAtMousePos);
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
