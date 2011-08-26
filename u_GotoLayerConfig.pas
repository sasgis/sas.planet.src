unit u_GotoLayerConfig;

interface
uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_GotoLayerConfig,
  u_ConfigDataElementBase;

type
  TGotoLayerConfig = class(TConfigDataElementBase, IGotoLayerConfig)
  private
    FShowTickCount: Cardinal;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetShowTickCount: Cardinal;
    procedure SetShowTickCount(AValue: Cardinal);
  public
    constructor Create;
  end;

implementation

uses
  Types,
  SysUtils;

{ TGotoLayerConfig }

constructor TGotoLayerConfig.Create;
begin
  inherited;
  FShowTickCount := 20000;
end;

procedure TGotoLayerConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FShowTickCount := AConfigData.ReadInteger('ShowTickCount', FShowTickCount);
    SetChanged;
  end;
end;

procedure TGotoLayerConfig.DoWriteConfig(AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteInteger('ShowTickCount', FShowTickCount);
end;

function TGotoLayerConfig.GetShowTickCount: Cardinal;
begin
  LockRead;
  try
    Result := FShowTickCount;
  finally
    UnlockRead;
  end;
end;

procedure TGotoLayerConfig.SetShowTickCount(AValue: Cardinal);
begin
  LockWrite;
  try
    if FShowTickCount <> AValue then begin
      FShowTickCount := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
