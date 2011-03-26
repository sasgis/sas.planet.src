unit u_MarkNameGenerator;

interface


uses
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_IMarkNameGenerator,
  u_ConfigDataElementBase;

type
  TMarkNameGenerator = class(TConfigDataElementBase, IMarkNameGenerator)
  private
    FFormatString: string;
    FCounter: Integer;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetFormatString: string;
    procedure SetFormatString(AValue: string);

    function GetCounter: Integer;
    procedure SetCounter(AValue: Integer);

    function GetNewName: string;
  public
    constructor Create(AFormatStringDefault: string);
  end;

implementation

uses
  SysUtils;

{ TMarkNameGenerator }

constructor TMarkNameGenerator.Create(AFormatStringDefault: string);
begin
  inherited Create;
  FFormatString := AFormatStringDefault;
  FCounter := 0;
end;

procedure TMarkNameGenerator.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FFormatString := AConfigData.ReadString('FormatString', FFormatString);
    FCounter := AConfigData.ReadInteger('Counter', FCounter);
    SetChanged;
  end;
end;

procedure TMarkNameGenerator.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteString('FormatString', FFormatString);
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

function TMarkNameGenerator.GetFormatString: string;
begin
  LockRead;
  try
    Result := FFormatString;
  finally
    UnlockRead;
  end;
end;

function TMarkNameGenerator.GetNewName: string;
begin
  LockWrite;
  try
    Result := Format(FFormatString, [FCounter]);
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

procedure TMarkNameGenerator.SetFormatString(AValue: string);
begin
  LockWrite;
  try
    if FFormatString <> AValue then begin
      FFormatString := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
