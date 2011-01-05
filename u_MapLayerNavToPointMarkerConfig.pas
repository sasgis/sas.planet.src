unit u_MapLayerNavToPointMarkerConfig;

interface

uses
  Types,
  GR32,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_IMapLayerNavToPointMarkerConfig,
  u_ConfigDataElementBase;

type
  TMapLayerNavToPointMarkerConfig = class(TConfigDataElementBase, IMapLayerNavToPointMarkerConfig)
  private
    FCrossDistInPixels: Double;
    FMarkerArrowSize: Integer;
    FMarkerArrowColor: TColor32;
    FMarkerCrossSize: Integer;
    FMarkerCrossColor: TColor32;
    FMarkerArrow: TCustomBitmap32;
    FMarkerCross: TCustomBitmap32;
    procedure PrepareMarker;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
    procedure SetChanged; override;
  protected
    function GetCrossDistInPixels: Double;
    procedure SetCrossDistInPixels(AValue: Double);

    function GetMarkerArrowSize: Integer;
    procedure SetMarkerArrowSize(AValue: Integer);

    function GetMarkerArrowColor: TColor32;
    procedure SetMarkerArrowColor(AValue: TColor32);

    function GetMarkerCrossSize: Integer;
    procedure SetMarkerCrossSize(AValue: Integer);

    function GetMarkerCrossColor: TColor32;
    procedure SetMarkerCrossColor(AValue: TColor32);

    function GetMarkerArrow: TCustomBitmap32;
    function GetMarkerCross: TCustomBitmap32;
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

constructor TMapLayerNavToPointMarkerConfig.Create;
begin
  inherited;
  FCrossDistInPixels := 1;

  FMarkerArrowSize := 25;
  FMarkerArrowColor := SetAlpha(clRed32, 150);

  FMarkerCrossSize := FMarkerArrowSize div 3;
  FMarkerCrossColor := SetAlpha(clRed32, 200);

  FMarkerArrow := TCustomBitmap32.Create;
  FMarkerArrow.DrawMode:=dmBlend;
  FMarkerArrow.CombineMode:=cmMerge;

  FMarkerCross := TCustomBitmap32.Create;
  FMarkerCross.DrawMode:=dmBlend;
  FMarkerCross.CombineMode:=cmMerge;
end;

destructor TMapLayerNavToPointMarkerConfig.Destroy;
begin
  FreeAndNil(FMarkerArrow);
  FreeAndNil(FMarkerCross);
  inherited;
end;

procedure TMapLayerNavToPointMarkerConfig.DoReadConfig(
  AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FCrossDistInPixels := AConfigData.ReadFloat('MinSpeed', FCrossDistInPixels);
    FMarkerArrowSize := AConfigData.ReadInteger('MarkerArrowSize', FMarkerArrowSize);
    LoadColor32(AConfigData, 'MarkerArrowColor', FMarkerArrowColor);
    FMarkerCrossSize := AConfigData.ReadInteger('MarkerCrossSize', FMarkerCrossSize);
    LoadColor32(AConfigData, 'MarkerCrossColor', FMarkerCrossColor);
    SetChanged;
  end;
end;

procedure TMapLayerNavToPointMarkerConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteFloat('MinSpeed', FCrossDistInPixels);
  AConfigData.WriteInteger('MarkerArrowSize', FMarkerArrowSize);
  WriteColor32(AConfigData, 'MarkerArrowColor', FMarkerArrowColor);
  AConfigData.WriteInteger('MarkerCrossSize', FMarkerCrossSize);
  WriteColor32(AConfigData, 'MarkerCrossColor', FMarkerCrossColor);
end;

function TMapLayerNavToPointMarkerConfig.GetMarkerArrow: TCustomBitmap32;
begin
  LockRead;
  try
    Result := FMarkerArrow;
  finally
    UnlockRead;
  end;
end;

function TMapLayerNavToPointMarkerConfig.GetMarkerArrowColor: TColor32;
begin
  LockRead;
  try
    Result := FMarkerArrowColor;
  finally
    UnlockRead;
  end;
end;

function TMapLayerNavToPointMarkerConfig.GetMarkerArrowSize: Integer;
begin
  LockRead;
  try
    Result := FMarkerArrowSize;
  finally
    UnlockRead;
  end;
end;

function TMapLayerNavToPointMarkerConfig.GetMarkerCross: TCustomBitmap32;
begin
  LockRead;
  try
    Result := FMarkerCross;
  finally
    UnlockRead;
  end;
end;

function TMapLayerNavToPointMarkerConfig.GetMarkerCrossColor: TColor32;
begin
  LockRead;
  try
    Result := FMarkerCrossColor;
  finally
    UnlockRead;
  end;
end;

function TMapLayerNavToPointMarkerConfig.GetMarkerCrossSize: Integer;
begin
  LockRead;
  try
    Result := FMarkerCrossSize;
  finally
    UnlockRead;
  end;
end;

function TMapLayerNavToPointMarkerConfig.GetCrossDistInPixels: Double;
begin
  LockRead;
  try
    Result := FCrossDistInPixels;
  finally
    UnlockRead;
  end;
end;

procedure TMapLayerNavToPointMarkerConfig.PrepareMarker;
var
  VSize: TPoint;
  VPolygon: TPolygon32;
  VPointHalfSize: Double;
  VRect: TRect;
  VCenterPoint: TDoublePoint;
begin
  VSize := Point(FMarkerArrowSize * 2, FMarkerArrowSize * 2);

  VCenterPoint.X := VSize.X / 2;
  VCenterPoint.Y := VSize.Y / 2;

  FMarkerArrow.SetSize(VSize.Y, VSize.Y);
  FMarkerArrow.Clear(0);
  VPolygon := TPolygon32.Create;
  try
    VPolygon.Antialiased := true;
    VPolygon.AntialiasMode := am32times;
    VPolygon.Add(FixedPoint(VCenterPoint.X, VCenterPoint.Y - FMarkerArrowSize));
    VPolygon.Add(FixedPoint(VCenterPoint.X - FMarkerArrowSize / 3, VCenterPoint.Y));
    VPolygon.Add(FixedPoint(VCenterPoint.X + FMarkerArrowSize / 3, VCenterPoint.Y));
    VPolygon.DrawFill(FMarkerArrow, FMarkerArrowColor);
  finally
    FreeAndNil(VPolygon);
  end;

  VSize := Point(FMarkerCrossSize * 2, FMarkerCrossSize * 2);
  VPointHalfSize := FMarkerCrossSize / 2;
  FMarkerCross.SetSize(VSize.Y, VSize.Y);
  FMarkerCross.Clear(0);
  VRect.Left := Trunc(VPointHalfSize - FMarkerCrossSize /10);
  VRect.Top := Trunc(VPointHalfSize - FMarkerCrossSize /2);
  VRect.Right := Trunc(VPointHalfSize + FMarkerCrossSize /10);
  VRect.Bottom := VRect.Top + FMarkerCrossSize;
  FMarkerCross.FillRectS(VRect, FMarkerCrossColor);
  VRect.Left := Trunc(VPointHalfSize - FMarkerCrossSize /2);
  VRect.Top := Trunc(VPointHalfSize - FMarkerCrossSize /10);
  VRect.Right := VRect.Left + FMarkerCrossSize;
  VRect.Bottom := Trunc(VPointHalfSize + FMarkerCrossSize /10);
  FMarkerCross.FillRectS(VRect, FMarkerCrossColor);
end;

procedure TMapLayerNavToPointMarkerConfig.SetChanged;
begin
  inherited;
  LockWrite;
  try
    PrepareMarker;
  finally
    UnlockWrite;
  end;
end;

procedure TMapLayerNavToPointMarkerConfig.SetMarkerArrowColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FMarkerArrowColor <> AValue then begin
      FMarkerArrowColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapLayerNavToPointMarkerConfig.SetMarkerArrowSize(AValue: Integer);
begin
  LockWrite;
  try
    if FMarkerArrowSize <> AValue then begin
      FMarkerArrowSize := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapLayerNavToPointMarkerConfig.SetMarkerCrossColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FMarkerCrossColor <> AValue then begin
      FMarkerCrossColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapLayerNavToPointMarkerConfig.SetMarkerCrossSize(AValue: Integer);
begin
  LockWrite;
  try
    if FMarkerCrossSize <> AValue then begin
      FMarkerCrossSize := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapLayerNavToPointMarkerConfig.SetCrossDistInPixels(AValue: Double);
begin
  LockWrite;
  try
    if FCrossDistInPixels <> AValue then begin
      FCrossDistInPixels := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
