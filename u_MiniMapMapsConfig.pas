unit u_MiniMapMapsConfig;

interface

uses
  i_JclNotify,
  i_ActiveMapsConfig,
  i_MapTypes,
  i_MiniMapLayerConfig,
  u_ActivMapWithLayers;

type
  TMiniMapMapsConfig = class(TActivMapWithLayers, IMiniMapMapsConfig)
  private
    FActiveMiniMap: IMapType;
    FSelectedMapChangeListener: IJclListener;
    FMainMapsConfig: IMainMapsConfig;
    FMainMapChangeListener: IJclListener;
    function CreateMiniMapMapsList: IMapTypeList;
    function CreateMiniMapLayersList: IMapTypeList;
    procedure OnMainMapChange(Sender: TObject);
    procedure OnSelectedChange(const AGUID: TGUID);
    procedure SetActiveMiniMap(AValue: IMapType);
  protected
    function GetActiveMiniMap: IMapType;
  public
    constructor Create(AMapsConfig: IMainMapsConfig);
    destructor Destroy; override;
  end;

implementation

uses
  ActiveX,
  c_ZeroGUID,
  u_NotifyEventListener,
  u_NotifyWithGUIDEvent,
  u_MapTypeBasic,
  u_MapTypeList;

{ TMiniMapMapsConfig }

constructor TMiniMapMapsConfig.Create(AMapsConfig: IMainMapsConfig);
begin
  FMainMapsConfig := AMapsConfig;
  inherited Create(CreateMiniMapMapsList, CreateMiniMapLayersList);

  FMainMapChangeListener := TNotifyEventListener.Create(Self.OnMainMapChange);
  FMainMapsConfig.GetActiveMap.GetChangeNotifier.Add(FMainMapChangeListener);

  FSelectedMapChangeListener := TNotifyWithGUIDEventListener.Create(Self.OnSelectedChange);
  MainMapChangeNotyfier.Add(FSelectedMapChangeListener);

  OnSelectedChange(GetActiveMap.GetSelectedGUID);
end;

destructor TMiniMapMapsConfig.Destroy;
begin
  FMainMapsConfig.GetActiveMap.GetChangeNotifier.Remove(FMainMapChangeListener);
  FMainMapChangeListener := nil;

  MainMapChangeNotyfier.Remove(FSelectedMapChangeListener);
  FSelectedMapChangeListener := nil;

  FMainMapsConfig := nil;
  inherited;
end;

function TMiniMapMapsConfig.GetActiveMiniMap: IMapType;
begin
  LockRead;
  try
    Result := FActiveMiniMap;
  finally
    UnlockRead;
  end;
end;

function TMiniMapMapsConfig.CreateMiniMapLayersList: IMapTypeList;
var
  VSourceList: IMapTypeList;
  VMap: IMapType;
  VList: TMapTypeList;
  VEnun: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
begin
  VSourceList := FMainMapsConfig.GetBitmapLayersSet.GetMapsList;
  VList := TMapTypeList.Create(True);
  Result := VList;
  VEnun := VSourceList.GetIterator;
  while VEnun.Next(1, VGUID, i) = S_OK do begin
    VMap := VSourceList.GetMapTypeByGUID(VGUID);
    if VMap.MapType.IsCanShowOnSmMap then begin
      VList.Add(VMap);
    end;
  end;
end;

function TMiniMapMapsConfig.CreateMiniMapMapsList: IMapTypeList;
var
  VSourceList: IMapTypeList;
  VMap: IMapType;
  VList: TMapTypeList;
  VEnun: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
begin
  VSourceList := FMainMapsConfig.GetActiveMap.GetMapsList;
  VList := TMapTypeList.Create(True);
  Result := VList;
  VList.Add(TMapTypeBasic.Create(nil));
  VEnun := VSourceList.GetIterator;
  while VEnun.Next(1, VGUID, i) = S_OK do begin
    VMap := VSourceList.GetMapTypeByGUID(VGUID);
    if VMap.MapType.IsCanShowOnSmMap then begin
      VList.Add(VMap);
    end;
  end;
end;

procedure TMiniMapMapsConfig.OnMainMapChange(Sender: TObject);
var
  VGUID: TGUID;
begin
  VGUID := GetActiveMap.GetSelectedGUID;
  if IsEqualGUID(VGUID, CGUID_Zero) then begin
    SetActiveMiniMap(FMainMapsConfig.GetSelectedMapType);
  end;
end;

procedure TMiniMapMapsConfig.OnSelectedChange(const AGUID: TGUID);
begin
  if IsEqualGUID(AGUID, CGUID_Zero) then begin
    SetActiveMiniMap(FMainMapsConfig.GetSelectedMapType);
  end else begin
    SetActiveMiniMap(GetActiveMap.GetMapsList.GetMapTypeByGUID(AGUID));
  end;
end;

procedure TMiniMapMapsConfig.SetActiveMiniMap(AValue: IMapType);
begin
  LockWrite;
  try
    if FActiveMiniMap <> AValue then begin
      FActiveMiniMap := AValue;
      inherited SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
