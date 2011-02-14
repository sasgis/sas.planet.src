unit u_CalcLineLayerConfig;

interface

uses
  GR32,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_ICalcLineLayerConfig,
  u_ConfigDataElementBase;

type
  TCalcLineLayerConfig = class(TConfigDataElementBase, ICalcLineLayerConfig)
  private
    FLenShow: Boolean;
    FLineColor: TColor32;
    FLineWidth: integer;
    FPointFillColor: TColor32;
    FPointRectColor: TColor32;
    FPointFirstColor: TColor32;
    FPointActiveColor: TColor32;
    FPointSize: integer;
    FTextColor: TColor32;
    FTextBGColor: TColor32;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetLenShow: Boolean;
    procedure SetLenShow(AValue: Boolean);

    function GetLineColor: TColor32;
    procedure SetLineColor(AValue: TColor32);

    function GetLineWidth: integer;
    procedure SetLineWidth(AValue: integer);

    function GetPointFillColor: TColor32;
    procedure SetPointFillColor(AValue: TColor32);

    function GetPointRectColor: TColor32;
    procedure SetPointRectColor(AValue: TColor32);

    function GetPointFirstColor: TColor32;
    procedure SetPointFirstColor(AValue: TColor32);

    function GetPointActiveColor: TColor32;
    procedure SetPointActiveColor(AValue: TColor32);

    function GetPointSize: integer;
    procedure SetPointSize(AValue: integer);

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
  FLenShow := True;

  FLineColor := SetAlpha(ClRed32, 150);
  FLineWidth := 3;

  FPointFillColor := SetAlpha(ClWhite32, 150);
  FPointRectColor := SetAlpha(ClRed32, 150);
  FPointFirstColor := SetAlpha(ClGreen32, 255);
  FPointActiveColor := SetAlpha(ClRed32, 255);
  FPointSize := 6;

  FTextColor := clBlack32;
  FTextBGColor := SetAlpha(ClWhite32, 110);
end;

procedure TCalcLineLayerConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FLenShow := AConfigData.ReadBool('LenShow', FLenShow);

    FLineColor := LoadColor32(AConfigData, 'LineColor', FLineColor);
    FLineWidth := AConfigData.ReadInteger('LineWidth', FLineWidth);

    FPointFillColor := LoadColor32(AConfigData, 'PointFillColor', FPointFillColor);
    FPointRectColor := LoadColor32(AConfigData, 'PointRectColor', FPointRectColor);
    FPointFirstColor := LoadColor32(AConfigData, 'PointFirstColor', FPointFirstColor);
    FPointActiveColor := LoadColor32(AConfigData, 'PointActiveColor', FPointActiveColor);
    FPointSize := AConfigData.ReadInteger('PointSize', FPointSize);

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

  WriteColor32(AConfigData, 'LineColor', FLineColor);
  AConfigData.WriteInteger('LineWidth', FLineWidth);

  WriteColor32(AConfigData, 'PointFillColor', FPointFillColor);
  WriteColor32(AConfigData, 'PointRectColor', FPointRectColor);
  WriteColor32(AConfigData, 'PointFirstColor', FPointFirstColor);
  WriteColor32(AConfigData, 'PointActiveColor', FPointActiveColor);
  AConfigData.WriteInteger('PointSize', FPointSize);

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

function TCalcLineLayerConfig.GetLineColor: TColor32;
begin
  LockRead;
  try
    Result := FLineColor;
  finally
    UnlockRead;
  end;
end;

function TCalcLineLayerConfig.GetLineWidth: integer;
begin
  LockRead;
  try
    Result := FLineWidth;
  finally
    UnlockRead;
  end;
end;

function TCalcLineLayerConfig.GetPointActiveColor: TColor32;
begin
  LockRead;
  try
    Result := FPointActiveColor;
  finally
    UnlockRead;
  end;
end;

function TCalcLineLayerConfig.GetPointFillColor: TColor32;
begin
  LockRead;
  try
    Result := FPointFillColor;
  finally
    UnlockRead;
  end;
end;

function TCalcLineLayerConfig.GetPointFirstColor: TColor32;
begin
  LockRead;
  try
    Result := FPointFirstColor;
  finally
    UnlockRead;
  end;
end;

function TCalcLineLayerConfig.GetPointRectColor: TColor32;
begin
  LockRead;
  try
    Result := FPointRectColor;
  finally
    UnlockRead;
  end;
end;

function TCalcLineLayerConfig.GetPointSize: integer;
begin
  LockRead;
  try
    Result := FPointSize;
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

procedure TCalcLineLayerConfig.SetLineColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FLineColor <> AValue then begin
      FLineColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TCalcLineLayerConfig.SetLineWidth(AValue: integer);
begin
  LockWrite;
  try
    if FLineWidth <> AValue then begin
      FLineWidth := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TCalcLineLayerConfig.SetPointActiveColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FPointActiveColor <> AValue then begin
      FPointActiveColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TCalcLineLayerConfig.SetPointFillColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FPointFillColor <> AValue then begin
      FPointFillColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TCalcLineLayerConfig.SetPointFirstColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FPointFirstColor <> AValue then begin
      FPointFirstColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TCalcLineLayerConfig.SetPointRectColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FPointRectColor <> AValue then begin
      FPointRectColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TCalcLineLayerConfig.SetPointSize(AValue: integer);
begin
  LockWrite;
  try
    if FPointSize <> AValue then begin
      FPointSize := AValue;
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
