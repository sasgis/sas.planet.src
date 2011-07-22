unit u_GotoLayerConfig;

interface
uses
  GR32,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_GotoLayerConfig,
  u_ConfigDataElementBase;

type
  TGotoLayerConfig = class(TConfigDataElementBase, IGotoLayerConfig)
  private
    FMarker: TCustomBitmap32;
    FMarkerFixedPoint: TPoint;
    FShowTickCount: Cardinal;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetMarker: TCustomBitmap32;
    procedure SetMarker(AValue: TCustomBitmap32);

    function GetMarkerFixedPoint: TPoint;
    procedure SetMarkerFixedPoint(AValue: TPoint);

    function GetShowTickCount: Cardinal;
    procedure SetShowTickCount(AValue: Cardinal);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  Types,
  SysUtils,
  u_GlobalState;

{ TGotoLayerConfig }

constructor TGotoLayerConfig.Create;
begin
  inherited;
  FShowTickCount := 20000;
  FMarker := TCustomBitmap32.Create;
  GState.LoadBitmapFromRes('ICONIII', FMarker);
  FMarkerFixedPoint := Point(7, 6);
end;

destructor TGotoLayerConfig.Destroy;
begin
  FreeAndNil(FMarker);
  inherited;
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

function TGotoLayerConfig.GetMarker: TCustomBitmap32;
begin
  LockRead;
  try
    Result := TCustomBitmap32.Create;
    Result.Assign(FMarker);
  finally
    UnlockRead;
  end;
end;

function TGotoLayerConfig.GetMarkerFixedPoint: TPoint;
begin
  LockRead;
  try
    Result := FMarkerFixedPoint;
  finally
    UnlockRead;
  end;
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

procedure TGotoLayerConfig.SetMarker(AValue: TCustomBitmap32);
begin
  LockWrite;
  try
    FMarker.Assign(AValue);
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TGotoLayerConfig.SetMarkerFixedPoint(AValue: TPoint);
begin
  LockWrite;
  try
    if (FMarkerFixedPoint.X <> AValue.X) or (FMarkerFixedPoint.Y <> AValue.Y) then begin
      FMarkerFixedPoint := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
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
