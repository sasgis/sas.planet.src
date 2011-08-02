unit u_MainMapsConfig;

interface

uses
  i_JclNotify,
  i_ActiveMapsConfig,
  i_MapTypes,
  u_ActivMapWithLayers;

type
  TMainMapsConfig = class(TActivMapWithLayers, IMainMapsConfig)
  private
    FDefaultMapGUID: TGUID;
    FSelectedMapType: IMapType;
    FActiveBitmapLayersSet: IActiveMapsSet;
    FActiveKmlLayersSet: IActiveMapsSet;
    FSelectedMapChangeListener: IJclListener;
    procedure OnSelectedChange(const AGUID: TGUID);
  protected
    function GetSelectedMapType: IMapType;
    function GetActiveBitmapLayersSet: IActiveMapsSet;
    function GetActiveKmlLayersSet: IActiveMapsSet;
  public
    constructor Create(AMapsSet, ALayersSet: IMapTypeSet; ADefaultMapGUID: TGUID);
    destructor Destroy; override;
  end;

implementation

uses
  ActiveX,
  u_NotifyWithGUIDEvent,
  u_MapTypeList,
  u_ActiveMapsSet;

{ TMainMapsConfig }

constructor TMainMapsConfig.Create(AMapsSet, ALayersSet: IMapTypeSet; ADefaultMapGUID: TGUID);
var
  VEnun: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
  VMapType: IMapType;
  VBitmapLayersList: TMapTypeSet;
  VKmlLayersList: TMapTypeSet;
begin
  FDefaultMapGUID := ADefaultMapGUID;
  inherited Create(AMapsSet, ALayersSet);

  VBitmapLayersList := TMapTypeSet.Create(True);
  VKmlLayersList := TMapTypeSet.Create(True);

  VEnun := ALayersSet.GetIterator;
  while VEnun.Next(1, VGUID, i) = S_OK do begin
    VMapType := ALayersSet.GetMapTypeByGUID(VGUID);
    if VMapType.MapType.IsBitmapTiles then begin
      VBitmapLayersList.Add(VMapType);
    end;
    if VMapType.MapType.IsKmlTiles then begin
      VKmlLayersList.Add(VMapType);
    end;
  end;

  FActiveBitmapLayersSet := TActiveMapsSet.Create(
    VBitmapLayersList,
    AllMapsSingleList,
    nil,
    LayerSetSelectNotyfier,
    LayerSetUnselectNotyfier
  );
  Add(FActiveBitmapLayersSet, nil);

  FActiveKmlLayersSet := TActiveMapsSet.Create(
    VKmlLayersList,
    AllMapsSingleList,
    nil,
    LayerSetSelectNotyfier,
    LayerSetUnselectNotyfier
  );
  Add(FActiveKmlLayersSet, nil);

  FSelectedMapChangeListener := TNotifyWithGUIDEventListener.Create(Self.OnSelectedChange);
  MainMapChangeNotyfier.Add(FSelectedMapChangeListener);

  SelectMainByGUID(FDefaultMapGUID);
  FDefaultMapGUID := GetActiveMap.GetSelectedGUID;
  OnSelectedChange(FDefaultMapGUID);
end;

destructor TMainMapsConfig.Destroy;
begin
  MainMapChangeNotyfier.Remove(FSelectedMapChangeListener);
  FSelectedMapChangeListener := nil;

  FActiveBitmapLayersSet := nil;
  FActiveKmlLayersSet := nil;
  inherited;
end;

function TMainMapsConfig.GetActiveBitmapLayersSet: IActiveMapsSet;
begin
  Result := FActiveBitmapLayersSet;
end;

function TMainMapsConfig.GetActiveKmlLayersSet: IActiveMapsSet;
begin
  Result := FActiveKmlLayersSet;
end;

function TMainMapsConfig.GetSelectedMapType: IMapType;
begin
  LockRead;
  try
    Result := FSelectedMapType;
  finally
    UnlockRead;
  end;
end;

procedure TMainMapsConfig.OnSelectedChange(const AGUID: TGUID);
begin
  LockWrite;
  try
    FSelectedMapType := GetActiveMap.GetMapsSet.GetMapTypeByGUID(AGUID);
    Assert(FSelectedMapType <> nil);
  finally
    UnlockWrite;
  end;
end;

end.
