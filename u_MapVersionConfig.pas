unit u_MapVersionConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MapVersionInfo,
  i_MapVersionConfig,
  u_ConfigDataElementBase;

type
  TMapVersionConfig = class(TConfigDataElementBase, IMapVersionConfig)
  private
    FDefConfig: IMapVersionInfo;
    FVersion: Variant;
    FStatic: IMapVersionInfo;
    function CreateStatic: IMapVersionInfo;
  protected
    procedure DoBeforeChangeNotify; override;
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetVersion: Variant;
    procedure SetVersion(const AValue: Variant);

    function GetStatic: IMapVersionInfo;
  public
    constructor Create(ADefConfig: IMapVersionInfo);
  end;


implementation

uses
  u_MapVersionInfo;

{ TMapVersionConfig }

constructor TMapVersionConfig.Create(ADefConfig: IMapVersionInfo);
begin
  inherited Create;
  FDefConfig := ADefConfig;
  FVersion := FDefConfig.Version;
end;

function TMapVersionConfig.CreateStatic: IMapVersionInfo;
begin
  Result := TMapVersionInfo.Create(FVersion);
end;

procedure TMapVersionConfig.DoBeforeChangeNotify;
begin
  inherited;
  LockWrite;
  try
    FStatic := CreateStatic;
  finally
    UnlockWrite;
  end;
end;

procedure TMapVersionConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FVersion := AConfigData.ReadString('Version', FVersion);
    SetChanged;
  end;
end;

procedure TMapVersionConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  if FVersion <> FDefConfig.Version then begin
    AConfigData.WriteString('Version', FVersion);
  end else begin
    AConfigData.DeleteValue('Version');
  end;
end;

function TMapVersionConfig.GetStatic: IMapVersionInfo;
begin
  Result := FStatic;
end;

function TMapVersionConfig.GetVersion: Variant;
begin
  LockRead;
  try
    Result := FVersion;
  finally
    UnlockRead;
  end;
end;

procedure TMapVersionConfig.SetVersion(const AValue: Variant);
begin
  LockWrite;
  try
    if FVersion <> AValue then begin
      FVersion := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
