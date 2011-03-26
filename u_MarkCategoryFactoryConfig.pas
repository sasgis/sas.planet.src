unit u_MarkCategoryFactoryConfig;

interface

uses
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_IMarkCategoryFactoryConfig,
  u_ConfigDataElementBase;

type
  TMarkCategoryFactoryConfig = class(TConfigDataElementBase, IMarkCategoryFactoryConfig)
  private
    FDefaultName: string;
    FAfterScale: Integer;
    FBeforeScale: Integer;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetDefaultName: string;
    procedure SetDefaultName(AValue: string);

    function GetAfterScale: Integer;
    procedure SetAfterScale(AValue: Integer);

    function GetBeforeScale: Integer;
    procedure SetBeforeScale(AValue: Integer);
  public
    constructor Create(ADefaultName: string);
  end;

implementation

{ TMarkCategoryFactoryConfig }

constructor TMarkCategoryFactoryConfig.Create(ADefaultName: string);
begin
  inherited Create;
  FDefaultName := ADefaultName;
  FAfterScale := 3;
  FBeforeScale := 23;
end;

procedure TMarkCategoryFactoryConfig.DoReadConfig(
  AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FDefaultName := AConfigData.ReadString('DefaultName', FDefaultName);
    FAfterScale := AConfigData.ReadInteger('AfterScale', FAfterScale);
    FBeforeScale := AConfigData.ReadInteger('BeforeScale', FBeforeScale);
    SetChanged;
  end;
end;

procedure TMarkCategoryFactoryConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteString('DefaultName', FDefaultName);
  AConfigData.WriteInteger('AfterScale', FAfterScale);
  AConfigData.WriteInteger('BeforeScale', FBeforeScale);
end;

function TMarkCategoryFactoryConfig.GetAfterScale: Integer;
begin
  LockRead;
  try
    Result := FAfterScale;
  finally
    UnlockRead;
  end;
end;

function TMarkCategoryFactoryConfig.GetBeforeScale: Integer;
begin
  LockRead;
  try
    Result := FBeforeScale;
  finally
    UnlockRead;
  end;
end;

function TMarkCategoryFactoryConfig.GetDefaultName: string;
begin
  LockRead;
  try
    Result := FDefaultName;
  finally
    UnlockRead;
  end;
end;

procedure TMarkCategoryFactoryConfig.SetAfterScale(AValue: Integer);
begin
  LockWrite;
  try
    if FAfterScale <> AValue then begin
      FAfterScale := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMarkCategoryFactoryConfig.SetBeforeScale(AValue: Integer);
begin
  LockWrite;
  try
    if FBeforeScale <> AValue then begin
      FBeforeScale := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMarkCategoryFactoryConfig.SetDefaultName(AValue: string);
begin
  LockWrite;
  try
    if FDefaultName <> AValue then begin
      FDefaultName := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
