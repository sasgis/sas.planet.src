unit u_BitmapPostProcessingConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_BitmapPostProcessingConfig,
  u_ConfigDataElementBase;

type
  TBitmapPostProcessingConfig = class(TConfigDataElementBase, IBitmapPostProcessingConfig)
  private
    FInvertColor: boolean;
    FGammaN: Integer;
    FContrastN: Integer;
    FStatic: IBitmapPostProcessingConfigStatic;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
    procedure SetChanged; override;
  protected
    function GetInvertColor: boolean;
    procedure SetInvertColor(AValue: boolean);
    function GetGammaN: Integer;
    procedure SetGammaN(AValue: Integer);
    function GetContrastN: Integer;
    procedure SetContrastN(AValue: Integer);
    function GetStatic: IBitmapPostProcessingConfigStatic;
  public
    constructor Create;
  end;

implementation

uses
  u_BitmapPostProcessingConfigStatic;

{ TBitmapPostProcessingConfig }

constructor TBitmapPostProcessingConfig.Create;
begin
  inherited;
  FInvertColor := False;
  FContrastN := 0;
  FGammaN := 50;
  FStatic := TBitmapPostProcessingConfigStatic.Create(FInvertColor, FGammaN, FContrastN);
end;

procedure TBitmapPostProcessingConfig.DoReadConfig(
  AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FInvertColor := AConfigData.ReadBool('InvertColor', FInvertColor);
    FGammaN := AConfigData.ReadInteger('Gamma', FGammaN);
    FContrastN := AConfigData.ReadInteger('Contrast', FContrastN);
    SetChanged;
  end;
end;

procedure TBitmapPostProcessingConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('InvertColor', FInvertColor);
  AConfigData.WriteInteger('Gamma', FGammaN);
  AConfigData.WriteInteger('Contrast', FContrastN);
end;

function TBitmapPostProcessingConfig.GetContrastN: Integer;
begin
  LockRead;
  try
    Result := FContrastN;
  finally
    UnlockRead;
  end;
end;

function TBitmapPostProcessingConfig.GetGammaN: Integer;
begin
  LockRead;
  try
    Result := FGammaN;
  finally
    UnlockRead;
  end;
end;

function TBitmapPostProcessingConfig.GetInvertColor: boolean;
begin
  LockRead;
  try
    Result := FInvertColor;
  finally
    UnlockRead;
  end;
end;

function TBitmapPostProcessingConfig.GetStatic: IBitmapPostProcessingConfigStatic;
begin
  LockRead;
  try
    Result := FStatic;
  finally
    UnlockRead;
  end;
end;

procedure TBitmapPostProcessingConfig.SetChanged;
begin
  inherited;
  FStatic := TBitmapPostProcessingConfigStatic.Create(FInvertColor, FGammaN, FContrastN);
end;

procedure TBitmapPostProcessingConfig.SetContrastN(AValue: Integer);
begin
  LockWrite;
  try
    if FContrastN <> AValue then begin
      FContrastN := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TBitmapPostProcessingConfig.SetGammaN(AValue: Integer);
begin
  LockWrite;
  try
    if FGammaN <> AValue then begin
      FGammaN := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TBitmapPostProcessingConfig.SetInvertColor(AValue: boolean);
begin
  LockWrite;
  try
    if FInvertColor <> AValue then begin
      FInvertColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
