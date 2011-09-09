unit u_MarkNameGenerator;

interface


uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_StringConfigDataElement,
  i_MarkNameGenerator,
  u_ConfigDataElementComplexBase;

type
  TMarkNameGenerator = class(TConfigDataElementComplexBase, IMarkNameGenerator)
  private
    FFormatString: IStringConfigDataElement;
    FCounter: Integer;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetFormatString: IStringConfigDataElement;

    function GetCounter: Integer;
    procedure SetCounter(AValue: Integer);

    function GetNewName: string;
  public
    constructor Create(AFormatString: IStringConfigDataElement);
  end;

implementation

uses
  SysUtils,
  u_ConfigSaveLoadStrategyBasicUseProvider;

{ TMarkNameGenerator }

constructor TMarkNameGenerator.Create(AFormatString: IStringConfigDataElement);
begin
  inherited Create;
  FFormatString := AFormatString;
  Add(FFormatString, TConfigSaveLoadStrategyBasicUseProvider.Create);
  FCounter := 0;
end;

procedure TMarkNameGenerator.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FCounter := AConfigData.ReadInteger('Counter', FCounter);
    SetChanged;
  end;
end;

procedure TMarkNameGenerator.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteInteger('Counter', FCounter);
end;

function TMarkNameGenerator.GetCounter: Integer;
begin
  LockRead;
  try
    Result := FCounter;
  finally
    UnlockRead;
  end;
end;

function TMarkNameGenerator.GetFormatString: IStringConfigDataElement;
begin
  Result := FFormatString;
end;

function TMarkNameGenerator.GetNewName: string;
begin
  LockWrite;
  try
    Result := Format(FFormatString.Value, [FCounter]);
    Inc(FCounter);
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TMarkNameGenerator.SetCounter(AValue: Integer);
begin
  LockWrite;
  try
    if FCounter <> AValue then begin
      FCounter := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
