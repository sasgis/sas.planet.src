unit u_MainMapsConfig;

interface

uses
  i_IActiveMapsConfig,
  i_MapTypes,
  u_ActivMapWithLayers;

type
  TMainMapsConfig = class(TActivMapWithLayers, IMainMapsConfig)
  private
    FBitmapLayersSet: IActiveMapsSet;
    FKmlLayersSet: IActiveMapsSet;
  protected
    function GetBitmapLayersSet: IActiveMapsSet;
    function GetKmlLayersSet: IActiveMapsSet;
  public
    constructor Create(AMapsList, ALayersList: IMapTypeList);
    destructor Destroy; override;
  end;

implementation

uses
  ActiveX,
  u_MapTypeList,
  u_ActiveMapsSet;

{ TMainMapsConfig }

constructor TMainMapsConfig.Create(AMapsList, ALayersList: IMapTypeList);
var
  VEnun: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
  VMapType: IMapType;
  VBitmapLayersList: TMapTypeList;
  VKmlLayersList: TMapTypeList;
begin
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
end;

destructor TMainMapsConfig.Destroy;
begin
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

end.
