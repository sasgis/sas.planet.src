unit u_MapLayerGPSMarkerConfig;

interface

uses
  Types,
  GR32,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_IMapLayerGPSMarkerConfig,
  u_ConfigDataElementBase;

type
  TMapLayerGPSMarkerConfig = class(TConfigDataElementBase, IMapLayerGPSMarkerConfig)
  private
    FMinMoveSpeed: Double;
    FMarkerMovedSize: Integer;
    FMarkerMovedColor: TColor32;
    FMarkerStopedSize: Integer;
    FMarkerStopedColor: TColor32;
    FMarkerMoved: TCustomBitmap32;
    FMarkerStoped: TCustomBitmap32;
    procedure PrepareMarker;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
    procedure SetChanged; override;
  protected
    function GetMinMoveSpeed: Double;
    procedure SetMinMoveSpeed(AValue: Double);

    function GetMarkerMovedSize: Integer;
    procedure SetMarkerMovedSize(AValue: Integer);

    function GetMarkerMovedColor: TColor32;
    procedure SetMarkerMovedColor(AValue: TColor32);

    function GetMarkerStopedSize: Integer;
    procedure SetMarkerStopedSize(AValue: Integer);

    function GetMarkerStopedColor: TColor32;
    procedure SetMarkerStopedColor(AValue: TColor32);

    function GetMarkerMoved: TCustomBitmap32;
    function GetMarkerStoped: TCustomBitmap32;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  GR32_Polygons,
  t_GeoTypes,
  u_ConfigProviderHelpers;

{ TMapLayerGPSMarkerConfig }

constructor TMapLayerGPSMarkerConfig.Create;
begin
  inherited;
  FMinMoveSpeed := 1;

  FMarkerMovedSize := 25;
  FMarkerMovedColor := SetAlpha(clRed32, 150);

  FMarkerStopedSize := FMarkerMovedSize div 3;
  FMarkerStopedColor := SetAlpha(clRed32, 200);

  FMarkerMoved := TCustomBitmap32.Create;
  FMarkerMoved.DrawMode:=dmBlend;
  FMarkerMoved.CombineMode:=cmMerge;

  FMarkerStoped := TCustomBitmap32.Create;
  FMarkerStoped.DrawMode:=dmBlend;
  FMarkerStoped.CombineMode:=cmMerge;
end;

destructor TMapLayerGPSMarkerConfig.Destroy;
begin
  FreeAndNil(FMarkerMoved);
  FreeAndNil(FMarkerStoped);

  inherited;
end;

procedure TMapLayerGPSMarkerConfig.DoReadConfig(
  AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FMinMoveSpeed := AConfigData.ReadFloat('MinSpeed', FMinMoveSpeed);
    FMarkerMovedSize := AConfigData.ReadInteger('MarkerMovedSize', FMarkerMovedSize);
    FMarkerMovedColor := ReadColor32(AConfigData, 'MarkerMovedColor', FMarkerMovedColor);
    FMarkerStopedSize := AConfigData.ReadInteger('MarkerStopedSize', FMarkerStopedSize);
    FMarkerStopedColor := ReadColor32(AConfigData, 'MarkerStopedColor', FMarkerStopedColor);
    SetChanged;
  end;
end;

procedure TMapLayerGPSMarkerConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteFloat('MinSpeed', FMinMoveSpeed);
  AConfigData.WriteInteger('MarkerMovedSize', FMarkerMovedSize);
  WriteColor32(AConfigData, 'MarkerMovedColor', FMarkerMovedColor);
  AConfigData.WriteInteger('MarkerStopedSize', FMarkerStopedSize);
  WriteColor32(AConfigData, 'MarkerStopedColor', FMarkerStopedColor);
end;

function TMapLayerGPSMarkerConfig.GetMarkerMoved: TCustomBitmap32;
begin
  LockRead;
  try
    Result := FMarkerMoved;
  finally
    UnlockRead;
  end;
end;

function TMapLayerGPSMarkerConfig.GetMarkerMovedColor: TColor32;
begin
  LockRead;
  try
    Result := FMarkerMovedColor;
  finally
    UnlockRead;
  end;
end;

function TMapLayerGPSMarkerConfig.GetMarkerMovedSize: Integer;
begin
  LockRead;
  try
    Result := FMarkerMovedSize;
  finally
    UnlockRead;
  end;
end;

function TMapLayerGPSMarkerConfig.GetMarkerStoped: TCustomBitmap32;
begin
  LockRead;
  try
    Result := FMarkerStoped;
  finally
    UnlockRead;
  end;
end;

function TMapLayerGPSMarkerConfig.GetMarkerStopedColor: TColor32;
begin
  LockRead;
  try
    Result := FMarkerStopedColor;
  finally
    UnlockRead;
  end;
end;

function TMapLayerGPSMarkerConfig.GetMarkerStopedSize: Integer;
begin
  LockRead;
  try
    Result := FMarkerStopedSize;
  finally
    UnlockRead;
  end;
end;

function TMapLayerGPSMarkerConfig.GetMinMoveSpeed: Double;
begin
  LockRead;
  try
    Result := FMinMoveSpeed;
  finally
    UnlockRead;
  end;
end;

procedure TMapLayerGPSMarkerConfig.PrepareMarker;
var
  VSize: TPoint;
  VPolygon: TPolygon32;
  VPointHalfSize: Double;
  VMarkRect: TRect;
  VCenterPoint: TDoublePoint;
begin
  VSize := Point(FMarkerMovedSize, FMarkerMovedSize);

  VCenterPoint.X := VSize.X / 2;
  VCenterPoint.Y := VSize.Y / 2;

  FMarkerMoved.SetSize(VSize.Y, VSize.Y);
  FMarkerMoved.Clear(0);
  VPolygon := TPolygon32.Create;
  try
    VPolygon.Antialiased := true;
    VPolygon.AntialiasMode := am32times;
    VPolygon.Add(FixedPoint(VCenterPoint.X, 0));
    VPolygon.Add(FixedPoint(VCenterPoint.X - FMarkerMovedSize / 3, VSize.Y));
    VPolygon.Add(FixedPoint(VCenterPoint.X + FMarkerMovedSize / 3, VSize.Y));
    VPolygon.DrawFill(FMarkerMoved, FMarkerMovedColor);
  finally
    FreeAndNil(VPolygon);
  end;

  VPointHalfSize := FMarkerStopedSize / 2;
  FMarkerStoped.SetSize(VSize.Y, VSize.Y);
  FMarkerStoped.Clear(0);
  VMarkRect := Bounds(
    Trunc(VCenterPoint.X - VPointHalfSize),
    Trunc(VCenterPoint.y - VPointHalfSize),
    FMarkerStopedSize,
    FMarkerStopedSize
  );
  FMarkerStoped.FillRectS(VMarkRect, FMarkerStopedColor);
end;

procedure TMapLayerGPSMarkerConfig.SetChanged;
begin
  inherited;
  LockWrite;
  try
    PrepareMarker;
  finally
    UnlockWrite;
  end;
end;

procedure TMapLayerGPSMarkerConfig.SetMarkerMovedColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FMarkerMovedColor <> AValue then begin
      FMarkerMovedColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapLayerGPSMarkerConfig.SetMarkerMovedSize(AValue: Integer);
begin
  LockWrite;
  try
    if FMarkerMovedSize <> AValue then begin
      FMarkerMovedSize := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapLayerGPSMarkerConfig.SetMarkerStopedColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FMarkerStopedColor <> AValue then begin
      FMarkerStopedColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapLayerGPSMarkerConfig.SetMarkerStopedSize(AValue: Integer);
begin
  LockWrite;
  try
    if FMarkerStopedSize <> AValue then begin
      FMarkerStopedSize := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapLayerGPSMarkerConfig.SetMinMoveSpeed(AValue: Double);
begin
  LockWrite;
  try
    if FMinMoveSpeed <> AValue then begin
      FMinMoveSpeed := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
