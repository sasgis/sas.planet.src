unit u_TileGridConfig;

interface

uses
  GR32,
  t_GeoTypes,
  i_ILocalCoordConverter,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_MapLayerGridsConfig,
  u_BaseGridConfig;

type
  TTileGridConfig = class(TBaseGridConfig, ITileGridConfig)
  private
    FUseRelativeZoom: Boolean;
    FZoom: Integer;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetUseRelativeZoom: Boolean;
    procedure SetUseRelativeZoom(AValue: Boolean);

    function GetZoom: Integer;
    procedure SetZoom(AValue: Integer);

    function GetActualZoom(ALocalConverter: ILocalCoordConverter): Byte;
    function GetRectStickToGrid(ALocalConverter: ILocalCoordConverter; ASourceRect: TDoubleRect): TDoubleRect;
  public
    constructor Create;
  end;

implementation

uses
  i_ICoordConverter;

{ TTileGridConfig }

constructor TTileGridConfig.Create;
begin
  inherited;
  FUseRelativeZoom := True;
  FZoom := 0;
end;

procedure TTileGridConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FUseRelativeZoom := AConfigData.ReadBool('UseRelativeZoom', FUseRelativeZoom);
    FZoom := AConfigData.ReadInteger('Zoom', FZoom);
  end;
end;

procedure TTileGridConfig.DoWriteConfig(AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('UseRelativeZoom', FUseRelativeZoom);
  AConfigData.WriteInteger('Zoom', FZoom);
end;

function TTileGridConfig.GetActualZoom(
  ALocalConverter: ILocalCoordConverter): Byte;
var
  VZoom: Integer;
  VRelative: Boolean;
begin
  LockRead;
  try
    VZoom := FZoom;
    VRelative := FUseRelativeZoom;
  finally
    UnlockRead;
  end;
  if VRelative then begin
    VZoom := VZoom + ALocalConverter.GetZoom;
  end;
  if VZoom < 0 then begin
    Result := 0;
  end else begin
    Result := VZoom;
    ALocalConverter.GetGeoConverter.CheckZoom(Result);
  end;
end;

function TTileGridConfig.GetRectStickToGrid(
  ALocalConverter: ILocalCoordConverter; ASourceRect: TDoubleRect): TDoubleRect;
var
  VZoom: Byte;
  VZoomCurr: Byte;
  VSelectedTiles: TRect;
  VConverter: ICoordConverter;
begin
  VZoomCurr := ALocalConverter.GetZoom;
  VConverter := ALocalConverter.GetGeoConverter;
  LockRead;
  try
    if GetVisible  then begin
      VZoom := GetActualZoom(ALocalConverter);
    end else begin
      VZoom := VZoomCurr;
    end;
  finally
    UnlockRead;
  end;
  VSelectedTiles := VConverter.LonLatRect2TileRect(ASourceRect, VZoom);
  Result := VConverter.TileRect2LonLatRect(VSelectedTiles, VZoom);
end;

function TTileGridConfig.GetUseRelativeZoom: Boolean;
begin
  LockRead;
  try
    Result := FUseRelativeZoom;
  finally
    UnlockRead;
  end;
end;

function TTileGridConfig.GetZoom: Integer;
begin
  LockRead;
  try
    Result := FZoom;
  finally
    UnlockRead;
  end;
end;

procedure TTileGridConfig.SetUseRelativeZoom(AValue: Boolean);
begin
  LockWrite;
  try
    if FUseRelativeZoom <> AValue then begin
      FUseRelativeZoom := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TTileGridConfig.SetZoom(AValue: Integer);
begin
  LockWrite;
  try
    if FZoom <> AValue then begin
      FZoom := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
