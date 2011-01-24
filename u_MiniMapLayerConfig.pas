unit u_MiniMapLayerConfig;

interface

uses
  GR32,
  i_JclNotify,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_IMiniMapLayerConfig,
  i_IActiveMapsConfig,
  i_MapTypes,
  u_ConfigDataElementComplexBase;

type
  TMiniMapLayerConfig = class(TConfigDataElementComplexBase, IMiniMapLayerConfig)
  private
    FWidth: Integer;
    FZoomDelta: Integer;
    FMasterAlpha: Integer;
    FVisible: Boolean;
    FDefoultMap: TCustomBitmap32;
    FPlusButton: TCustomBitmap32;
    FMinusButton: TCustomBitmap32;
    FMapsConfig: IActivMapWithLayers;
    FActiveMiniMap: IMapType;
    FMainMapsConfig: IMainMapsConfig;
    FMainMapChangeListener: IJclListener;
    function GetMiniMapMapsList: IMapTypeList;
    procedure OnMainMapChange(Sender: TObject);
  protected
    procedure SetChanged; override;
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetWidth: Integer;
    procedure SetWidth(AValue: Integer);

    function GetZoomDelta: Integer;
    procedure SetZoomDelta(AValue: Integer);

    function GetMasterAlpha: Integer;
    procedure SetMasterAlpha(AValue: Integer);

    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);

    function GetDefoultMap: TCustomBitmap32;
    function GetPlusButton: TCustomBitmap32;
    function GetMinusButton: TCustomBitmap32;

    function GetMapsConfig: IActivMapWithLayers;
    function GetActiveMiniMap: IMapType;
  public
    constructor Create(AMapsConfig: IMainMapsConfig);
    destructor Destroy; override;
  end;

implementation

uses
  ActiveX,
  SysUtils,
  c_ZeroGUID,
  u_MapTypeList,
  u_NotifyEventListener,
  u_ConfigSaveLoadStrategyBasicProviderSubItem,
  u_ActivMapWithLayers;

{ TMiniMapLayerConfig }

constructor TMiniMapLayerConfig.Create(AMapsConfig: IMainMapsConfig);
begin
  inherited Create;
  FMainMapsConfig := AMapsConfig;
  FWidth := 100;
  FZoomDelta := 4;
  FMasterAlpha := 150;
  FVisible := True;
  FDefoultMap := TCustomBitmap32.Create;
  FPlusButton := TCustomBitmap32.Create;
  FMinusButton := TCustomBitmap32.Create;

  FMapsConfig := TActivMapWithLayers.Create(GetMiniMapMapsList, FMainMapsConfig.GetBitmapLayersSet.GetMapsList);
  Add(FMapsConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('Maps'));
  FMainMapChangeListener := TNotifyEventListener.Create(Self.OnMainMapChange);
  FMainMapsConfig.GetActiveMap.GetChangeNotifier.Add(FMainMapChangeListener);
end;

destructor TMiniMapLayerConfig.Destroy;
begin
  FreeAndNil(FDefoultMap);
  FreeAndNil(FPlusButton);
  FreeAndNil(FMinusButton);
  FMainMapsConfig.GetActiveMap.GetChangeNotifier.Remove(FMainMapChangeListener);
  FMainMapChangeListener := nil;
  FMainMapsConfig := nil;
  inherited;
end;

function TMiniMapLayerConfig.GetMiniMapMapsList: IMapTypeList;
var
  VMap: IMapType;
  VList: TMapTypeList;
  VEnun: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
begin
  VList := TMapTypeList.Create(True);
  Result := VList;
  VList.Add(nil);
  VEnun := FMainMapsConfig.GetActiveMap.GetMapsList.GetIterator;
  while VEnun.Next(1, VGUID, i) = S_OK do begin
    VMap := FMainMapsConfig.GetActiveMap.GetMapsList.GetMapTypeByGUID(VGUID);
    VList.Add(VMap);
  end;
end;

procedure TMiniMapLayerConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FWidth := AConfigData.ReadInteger('Width', FWidth);
    FZoomDelta := AConfigData.ReadInteger('ZoomDelta', FZoomDelta);
    FMasterAlpha := AConfigData.ReadInteger('Alpha', FMasterAlpha);
    FVisible := AConfigData.ReadBool('Visible', FVisible);
    SetChanged;
  end;
end;

procedure TMiniMapLayerConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteInteger('Width', FWidth);
  AConfigData.WriteInteger('ZoomDelta', FZoomDelta);
  AConfigData.WriteInteger('Alpha', FMasterAlpha);
  AConfigData.WriteBool('Visible', FVisible);
end;

function TMiniMapLayerConfig.GetActiveMiniMap: IMapType;
begin
  LockRead;
  try
    Result := FActiveMiniMap;
  finally
    UnlockRead;
  end;
end;

function TMiniMapLayerConfig.GetDefoultMap: TCustomBitmap32;
begin
  LockRead;
  try
    Result := FDefoultMap;
  finally
    UnlockRead;
  end;
end;

function TMiniMapLayerConfig.GetMapsConfig: IActivMapWithLayers;
begin
  Result := FMapsConfig;
end;

function TMiniMapLayerConfig.GetMasterAlpha: Integer;
begin
  LockRead;
  try
    Result := FMasterAlpha;
  finally
    UnlockRead;
  end;
end;

function TMiniMapLayerConfig.GetMinusButton: TCustomBitmap32;
begin
  LockRead;
  try
    Result := FMinusButton;
  finally
    UnlockRead;
  end;
end;

function TMiniMapLayerConfig.GetPlusButton: TCustomBitmap32;
begin
  LockRead;
  try
    Result := FPlusButton;
  finally
    UnlockRead;
  end;
end;

function TMiniMapLayerConfig.GetVisible: Boolean;
begin
  LockRead;
  try
    Result := FVisible;
  finally
    UnlockRead;
  end;
end;

function TMiniMapLayerConfig.GetWidth: Integer;
begin
  LockRead;
  try
    Result := FWidth;
  finally
    UnlockRead;
  end;
end;

function TMiniMapLayerConfig.GetZoomDelta: Integer;
begin
  LockRead;
  try
    Result := FZoomDelta;
  finally
    UnlockRead;
  end;
end;

procedure TMiniMapLayerConfig.OnMainMapChange(Sender: TObject);
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TMiniMapLayerConfig.SetChanged;
var
  VGUID: TGUID;
begin
  inherited;
  VGUID := FMapsConfig.GetActiveMap.GetSelectedGUID;
  if IsEqualGUID(VGUID, CGUID_Zero) then begin
    VGUID := FMainMapsConfig.GetActiveMap.GetSelectedGUID;
    FActiveMiniMap := FMainMapsConfig.GetActiveMap.GetMapsList.GetMapTypeByGUID(VGUID);
  end else begin
    FActiveMiniMap := FMapsConfig.GetActiveMap.GetMapsList.GetMapTypeByGUID(VGUID);
  end;
end;

procedure TMiniMapLayerConfig.SetMasterAlpha(AValue: Integer);
begin
  LockWrite;
  try
    if FMasterAlpha <> AValue then begin
      FMasterAlpha := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMiniMapLayerConfig.SetVisible(AValue: Boolean);
begin
  LockWrite;
  try
    if FVisible <> AValue then begin
      FVisible := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMiniMapLayerConfig.SetWidth(AValue: Integer);
begin
  LockWrite;
  try
    if FWidth <> AValue then begin
      FWidth := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMiniMapLayerConfig.SetZoomDelta(AValue: Integer);
begin
  LockWrite;
  try
    if FZoomDelta <> AValue then begin
      FZoomDelta := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
