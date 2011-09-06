unit u_SelectionPolylineLayerConfig;

interface

uses
  GR32,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_SelectionPolylineLayerConfig,
  u_PolylineLayerConfig;

type
  TSelectionPolylineLayerConfig = class(TPolylineLayerConfig, ISelectionPolylineLayerConfig)
  private
    FShadowPolygonColor: TColor32;
    FPolygoneRadius: Double;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetRadius: Double;
    procedure SetRadius(AValue: Double);
    function GetShadowPolygonColor: TColor32;
    procedure SetShadowPolygonColor(AValue: TColor32);
  public
    constructor Create;
  end;

implementation

{ TSelectionPolylineLayerConfig }

constructor TSelectionPolylineLayerConfig.Create;
begin
  inherited;
  LockWrite;
  try
    SetLineColor(SetAlpha(clBlue32, 180));

    SetPointFillColor(SetAlpha(clYellow32, 150));
    SetPointRectColor(SetAlpha(ClRed32, 150));
    SetPointFirstColor(SetAlpha(ClGreen32, 255));
    SetPointActiveColor(SetAlpha(ClRed32, 255));

    SetShadowPolygonColor(SetAlpha(clBlack32,150));

    FPolygoneRadius:=100;
  finally
    UnlockWrite;
  end;
end;

procedure TSelectionPolylineLayerConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FPolygoneRadius := AConfigData.ReadFloat('PolygoneRadius', FPolygoneRadius);
    SetChanged;
  end;
end;

procedure TSelectionPolylineLayerConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteFloat('PolygoneRadius', FPolygoneRadius);
end;

function TSelectionPolylineLayerConfig.GetRadius: Double;
begin
  LockRead;
  try
    Result := FPolygoneRadius;
  finally
    UnlockRead;
  end;
end;

function TSelectionPolylineLayerConfig.GetShadowPolygonColor: TColor32;
begin
  LockRead;
  try
    Result := FShadowPolygonColor;
  finally
    UnlockRead;
  end;
end;

procedure TSelectionPolylineLayerConfig.SetRadius(AValue: Double);
begin
  LockWrite;
  try
    if FPolygoneRadius <> AValue then begin
      FPolygoneRadius := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TSelectionPolylineLayerConfig.SetShadowPolygonColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FShadowPolygonColor <> AValue then begin
      FShadowPolygonColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;


end.
