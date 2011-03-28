unit u_MainWindowToolbarsLock;

interface

uses
  Types,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  u_ConfigDataElementBase,
  i_MainFormConfig;

type
  TMainWindowToolbarsLock = class(TConfigDataElementBase, IMainWindowToolbarsLock)
  private
    FIsLock: Boolean;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetIsLock: Boolean;
    procedure SetLock(AValue: Boolean);
  public
    constructor Create;
  end;


implementation

{ TMainWindowToolbarsLock }

constructor TMainWindowToolbarsLock.Create;
begin
  inherited;
  FIsLock := False;
end;

procedure TMainWindowToolbarsLock.DoReadConfig(
  AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FIsLock := AConfigData.ReadBool('lock_toolbars', FIsLock);
    SetChanged;
  end;
end;

procedure TMainWindowToolbarsLock.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('lock_toolbars', FIsLock);
end;

function TMainWindowToolbarsLock.GetIsLock: Boolean;
begin
  LockRead;
  try
    Result := FIsLock;
  finally
    UnlockRead;
  end;
end;

procedure TMainWindowToolbarsLock.SetLock(AValue: Boolean);
begin
  LockWrite;
  try
    if FIsLock <> AValue then begin
      FIsLock := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
