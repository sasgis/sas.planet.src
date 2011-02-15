unit u_CalcLineLayerConfig;

interface

uses
  GR32,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_ICalcLineLayerConfig,
  u_ConfigDataElementBase,
  u_PolyLineLayerConfig;

type
  TCalcLineLayerConfig = class(TPolyLineLayerConfig, ICalcLineLayerConfig)
  private
    FLenShow: Boolean;
    FTextColor: TColor32;
    FTextBGColor: TColor32;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetLenShow: Boolean;
    procedure SetLenShow(AValue: Boolean);

    function GetTextColor: TColor32;
    procedure SetTextColor(AValue: TColor32);

    function GetTextBGColor: TColor32;
    procedure SetTextBGColor(AValue: TColor32);
  public
    constructor Create;
  end;

implementation

uses
  u_ConfigProviderHelpers;

{ TCalcLineLayerConfig }

constructor TCalcLineLayerConfig.Create;
begin
  inherited;
  LockWrite;
  try
    SetLineColor(SetAlpha(ClRed32, 150));
    SetLineWidth(3);

    SetPointFillColor(SetAlpha(ClWhite32, 150));
    SetPointRectColor(SetAlpha(ClRed32, 150));
    SetPointFirstColor(SetAlpha(ClGreen32, 255));
    SetPointActiveColor(SetAlpha(ClRed32, 255));
    SetPointSize(6);

    SetLenShow(True);

    SetTextColor(clBlack32);
    SetTextBGColor(SetAlpha(ClWhite32, 110));
  finally
    UnlockWrite;
  end;
end;

procedure TCalcLineLayerConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FLenShow := AConfigData.ReadBool('LenShow', FLenShow);

    FTextColor := LoadColor32(AConfigData, 'TextColor', FTextColor);
    FTextBGColor := LoadColor32(AConfigData, 'TextBGColor', FTextBGColor);

    SetChanged;
  end;
end;

procedure TCalcLineLayerConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('LenShow', FLenShow);

  WriteColor32(AConfigData, 'TextColor', FTextColor);
  WriteColor32(AConfigData, 'TextBGColor', FTextBGColor);
end;

function TCalcLineLayerConfig.GetLenShow: Boolean;
begin
  LockRead;
  try
    Result := FLenShow;
  finally
    UnlockRead;
  end;
end;

function TCalcLineLayerConfig.GetTextBGColor: TColor32;
begin
  LockRead;
  try
    Result := FTextBGColor;
  finally
    UnlockRead;
  end;
end;

function TCalcLineLayerConfig.GetTextColor: TColor32;
begin
  LockRead;
  try
    Result := FTextColor;
  finally
    UnlockRead;
  end;
end;

procedure TCalcLineLayerConfig.SetLenShow(AValue: Boolean);
begin
  LockWrite;
  try
    if FLenShow <> AValue then begin
      FLenShow := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TCalcLineLayerConfig.SetTextBGColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FTextBGColor <> AValue then begin
      FTextBGColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TCalcLineLayerConfig.SetTextColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FTextColor <> AValue then begin
      FTextColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
