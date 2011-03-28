unit u_ScaleLineConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_IScaleLineConfig,
  u_ConfigDataElementBase;

type
  TScaleLineConfig = class(TConfigDataElementBase, IScaleLineConfig)
  private
    FVisible: Boolean;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
  public
    constructor Create;
  end;

implementation

{ TScaleLineConfig }

constructor TScaleLineConfig.Create;
begin
  inherited;
  FVisible := True;
end;

procedure TScaleLineConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FVisible := AConfigData.ReadBool('Visible', FVisible);
    SetChanged;
  end;
end;

procedure TScaleLineConfig.DoWriteConfig(AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('Visible', FVisible);
end;

function TScaleLineConfig.GetVisible: Boolean;
begin
  LockRead;
  try
    Result := FVisible;
  finally
    UnlockRead;
  end;
end;

procedure TScaleLineConfig.SetVisible(AValue: Boolean);
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
