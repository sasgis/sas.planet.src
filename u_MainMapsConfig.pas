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
    FBitmapLayersSet: IActiveMapsSet;
    FKmlLayersSet: IActiveMapsSet;
    FSelectedMapChangeListener: IJclListener;
    procedure OnSelectedChange(AGUID: TGUID);
  protected
    function GetSelectedMapType: IMapType;
    function GetBitmapLayersSet: IActiveMapsSet;
    function GetKmlLayersSet: IActiveMapsSet;
  public
    constructor Create(AMapsList, ALayersList: IMapTypeList; ADefaultMapGUID: TGUID);
    destructor Destroy; override;
  end;

implementation

uses
  ActiveX,
  u_NotifyWithGUIDEvent,
  u_MapTypeList,
  u_ActiveMapsSet;

{ TMainMapsConfig }

constructor TMainMapsConfig.Create(AMapsList, ALayersList: IMapTypeList; ADefaultMapGUID: TGUID);
var
  VEnun: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
  VMapType: IMapType;
  VBitmapLayersList: TMapTypeList;
  VKmlLayersList: TMapTypeList;
begin
  FDefaultMapGUID := ADefaultMapGUID;
  inherited Create(AMapsList, ALayersList);

  VBitmapLayersList := TMapTypeList.Create(True);
  VKmlLayersList := TMapTypeList.Create(True);

  VEnun := ALayersList.GetIterator;
  while VEnun.Next(1, VGUID, i) = S_OK do begin
    VMapType := ALayersList.GetMapTypeByGUID(VGUID);
    if VMapType.MapType.IsBitmapTiles then begin
      VBitmapLayersList.Add(VMapType);
    end;
    if VMapType.MapType.IsKmlTiles then begin
      VKmlLayersList.Add(VMapType);
    end;
  end;

  FBitmapLayersSet := TActiveMapsSet.Create(
    VBitmapLayersList,
    AllMapsSingleList,
    nil,
    LayerSetSelectNotyfier,
    LayerSetUnselectNotyfier
  );
  Add(FBitmapLayersSet, nil);

  FKmlLayersSet := TActiveMapsSet.Create(
    VKmlLayersList,
    AllMapsSingleList,
    nil,
    LayerSetSelectNotyfier,
    LayerSetUnselectNotyfier
  );
  Add(FKmlLayersSet, nil);

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

  FBitmapLayersSet := nil;
  FKmlLayersSet := nil;
  inherited;
end;

function TMainMapsConfig.GetBitmapLayersSet: IActiveMapsSet;
begin
  Result := FBitmapLayersSet;
end;

function TMainMapsConfig.GetKmlLayersSet: IActiveMapsSet;
begin
  Result := FKmlLayersSet;
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

procedure TMainMapsConfig.OnSelectedChange(AGUID: TGUID);
begin
  LockWrite;
  try
    FSelectedMapType := GetActiveMap.GetMapsList.GetMapTypeByGUID(AGUID);
    Assert(FSelectedMapType <> nil);
  finally
    UnlockWrite;
  end;
end;

end.
