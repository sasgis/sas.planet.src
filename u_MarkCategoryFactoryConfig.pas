unit u_MarkCategoryFactoryConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_StringConfigDataElement,
  i_LanguageManager,
  i_MarkCategoryFactoryConfig,
  u_ConfigDataElementComplexBase;

type
  TMarkCategoryFactoryConfig = class(TConfigDataElementComplexBase, IMarkCategoryFactoryConfig)
  private
    FDefaultName: IStringConfigDataElement;
    FAfterScale: Integer;
    FBeforeScale: Integer;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetDefaultName: IStringConfigDataElement;

    function GetAfterScale: Integer;
    procedure SetAfterScale(AValue: Integer);

    function GetBeforeScale: Integer;
    procedure SetBeforeScale(AValue: Integer);
  public
    constructor Create(ALanguageManager: ILanguageManager);
  end;

implementation

uses
  u_ConfigSaveLoadStrategyBasicUseProvider,
  u_StringConfigDataElementWithDefByStringRec,
  u_ResStrings;

{ TMarkCategoryFactoryConfig }

constructor TMarkCategoryFactoryConfig.Create(ALanguageManager: ILanguageManager);
begin
  inherited Create;
  FDefaultName :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager,
      @SAS_STR_NewCategory,
      True,
      'DefaultName',
      True
    );
  Add(FDefaultName, TConfigSaveLoadStrategyBasicUseProvider.Create);
  FAfterScale := 3;
  FBeforeScale := 23;
end;

procedure TMarkCategoryFactoryConfig.DoReadConfig(
  AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FAfterScale := AConfigData.ReadInteger('AfterScale', FAfterScale);
    FBeforeScale := AConfigData.ReadInteger('BeforeScale', FBeforeScale);
    SetChanged;
  end;
end;

procedure TMarkCategoryFactoryConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
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

function TMarkCategoryFactoryConfig.GetDefaultName: IStringConfigDataElement;
begin
  Result := FDefaultName;
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

end.
