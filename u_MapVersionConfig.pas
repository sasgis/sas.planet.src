unit u_MapVersionConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MapVersionConfig,
  u_ConfigDataElementBase;

type
  TMapVersionConfigStatic = class(TInterfacedObject, IMapVersionConfigStatic)
  private
    FVersion: Variant;
  protected
    function GetVersion: Variant;
  public
    constructor Create(
      AVersion: Variant
    );
  end;

  TMapVersionConfig = class(TConfigDataElementBase, IMapVersionConfig)
  private
    FVersion: Variant;
    FStatic: IMapVersionConfigStatic;
    function CreateStatic: IMapVersionConfigStatic;
  protected
    procedure SetChanged; override;
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetVersion: Variant;
    procedure SetVersion(const AValue: Variant);

    function GetStatic: IMapVersionConfigStatic;
  public
    constructor Create(ADefConfig: IMapVersionConfigStatic);
  end;


implementation

{ TMapVersionConfigStatic }

constructor TMapVersionConfigStatic.Create(AVersion: Variant);
begin
  FVersion := AVersion;
end;

function TMapVersionConfigStatic.GetVersion: Variant;
begin
  Result := FVersion;
end;

{ TMapVersionConfig }

constructor TMapVersionConfig.Create(ADefConfig: IMapVersionConfigStatic);
begin
  inherited Create;
  FVersion := ADefConfig.Version;
end;

function TMapVersionConfig.CreateStatic: IMapVersionConfigStatic;
begin
  Result := TMapVersionConfigStatic.Create(FVersion);
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
  AConfigData.WriteString('Version', FVersion);
end;

function TMapVersionConfig.GetStatic: IMapVersionConfigStatic;
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

procedure TMapVersionConfig.SetChanged;
begin
  inherited;
  LockWrite;
  try
    FStatic := CreateStatic;
  finally
    UnlockWrite;
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
