unit u_ActivMapWithLayers;

interface

uses
  i_IGUIDList,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_MapTypes,
  i_ActiveMapsConfig,
  u_NotifyWithGUIDEvent,
  u_MainActiveMap;

type
  TActivMapWithLayers = class(TMainActiveMap, IActivMapWithLayers)
  private
    FLayerSetSelectNotyfier: INotifierWithGUID;
    FLayerSetUnselectNotyfier: INotifierWithGUID;

    FAllMapsList: IMapTypeList;
    FAllMapsSingleList: IGUIDInterfaceList;
    FLayersSet: IActiveMapsSet;
    FAllMapsSet: IActiveMapsSet;
  protected
    property LayerSetSelectNotyfier: INotifierWithGUID read FLayerSetSelectNotyfier;
    property LayerSetUnselectNotyfier: INotifierWithGUID read FLayerSetUnselectNotyfier;
    property AllMapsSingleList: IGUIDInterfaceList read FAllMapsSingleList;
  protected
    procedure SelectLayerByGUID(AMapGUID: TGUID);
    procedure UnSelectLayerByGUID(AMapGUID: TGUID);

    function GetLayers: IActiveMapsSet;
    function GetAllActiveMapsSet: IActiveMapsSet;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  public
    constructor Create(AMapsList, ALayersList: IMapTypeList);
    destructor Destroy; override;
  end;

implementation

uses
  Classes,
  StrUtils,
  SysUtils,
  ActiveX,
  c_ZeroGUID,
  u_GUIDInterfaceList,
  u_MapTypeList,
  u_ActiveMapSingleAbstract,
  u_ActiveMapsSet;

const
  CKeyNameLayer = 'Layer';

{ TActivMapWithLayers }

constructor TActivMapWithLayers.Create(AMapsList, ALayersList: IMapTypeList);
var
  VEnun: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
  VMapType: IMapType;
  VSingleMap: IActiveMapSingle;
  VAllMapsList: TMapTypeList;
begin
  inherited Create(AMapsList);
  FLayerSetSelectNotyfier := TNotifierWithGUID.Create;
  FLayerSetUnselectNotyfier := TNotifierWithGUID.Create;

  FAllMapsSingleList := TGUIDInterfaceList.Create(False);
  VAllMapsList := TMapTypeList.Create(True);

  VEnun := AMapsList.GetIterator;
  while VEnun.Next(1, VGUID, i) = S_OK do begin
    VMapType := AMapsList.GetMapTypeByGUID(VGUID);
    VSingleMap := IActiveMapSingle(SingeMapsList.GetByGUID(VGUID));
    VAllMapsList.Add(VMapType);
    FAllMapsSingleList.Add(VGUID, VSingleMap);
  end;

  VEnun := ALayersList.GetIterator;
  while VEnun.Next(1, VGUID, i) = S_OK do begin
    VMapType := ALayersList.GetMapTypeByGUID(VGUID);
    VSingleMap := TActiveMapSingleLayer.Create(
      VMapType,
      FLayerSetSelectNotyfier,
      FLayerSetUnselectNotyfier
    );
    VAllMapsList.Add(VMapType);
    FAllMapsSingleList.Add(VGUID, VSingleMap);
    Add(VSingleMap, nil);
  end;


  FAllMapsList := VAllMapsList;
  
  FLayersSet := TActiveMapsSet.Create(
    ALayersList,
    FAllMapsSingleList,
    nil,
    FLayerSetSelectNotyfier,
    FLayerSetUnselectNotyfier
  );
  Add(FLayersSet, nil);

  FAllMapsSet :=  TActiveMapsSet.Create(
    FAllMapsList,
    FAllMapsSingleList,
    MainMapChangeNotyfier,
    FLayerSetSelectNotyfier,
    FLayerSetUnselectNotyfier
  );
  Add(FAllMapsSet, nil);
end;

destructor TActivMapWithLayers.Destroy;
begin
  FLayerSetSelectNotyfier := nil;
  FLayerSetUnselectNotyfier := nil;

  FAllMapsList := nil;
  FAllMapsSingleList := nil;
  FLayersSet := nil;
  FAllMapsSet := nil;
  inherited;
end;

procedure TActivMapWithLayers.DoReadConfig(AConfigData: IConfigDataProvider);
var
  VList: TStringList;
  i: Integer;
  VKeyName: string;
  VGUIDString: string;
  VGUID: TGUID;
  VMap: IMapType;
begin
  inherited;
  if AConfigData <> nil then begin
    VList := TStringList.Create;
    try
      AConfigData.ReadValuesList(VList);
      for i := 0 to VList.Count - 1 do begin
        VKeyName := VList.Strings[i];
        if SameText(LeftStr(VKeyName, length(CKeyNameLayer)), CKeyNameLayer) then begin
          VGUIDString := AConfigData.ReadString(VKeyName, '');
          if VGUIDString <> '' then begin
            try
              VGUID := StringToGUID(VGUIDString);
            except
              VGUID := CGUID_Zero;
            end;
          end else begin
            VGUID := CGUID_Zero;
          end;
          if not IsEqualGUID(VGUID, CGUID_Zero) then begin
            VMap := FLayersSet.GetMapsList.GetMapTypeByGUID(VGUID);
            if VMap <> nil then begin
              SelectLayerByGUID(VGUID)
            end;
          end;
        end;
      end;
    finally
      VList.Free;
    end;
  end;
end;

procedure TActivMapWithLayers.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
var
  VList: TStringList;
  i: Cardinal;
  VKeyName: string;
  VGUIDString: string;
  VGUID: TGUID;
  VIndex: Integer;
  VEnum: IEnumGUID;
begin
  inherited;
  VList := TStringList.Create;
  try
    AConfigData.ReadValuesList(VList);
    for i := 0 to VList.Count - 1 do begin
      VKeyName := VList.Strings[i];
      if SameText(LeftStr(VKeyName, length(CKeyNameLayer)), CKeyNameLayer) then begin
        AConfigData.DeleteValue(VKeyName);
      end;
    end;
  finally
    VList.Free;
  end;

  VIndex := 0;
  VEnum := FLayersSet.GetSelectedMapsList.GetIterator;
  while VEnum.Next(1, VGUID, i) = S_OK do begin
    VGUIDString := GUIDToString(VGUID);
    AConfigData.WriteString(CKeyNameLayer + IntToStr(VIndex), VGUIDString);
    Inc(VIndex);
  end;
end;

function TActivMapWithLayers.GetAllActiveMapsSet: IActiveMapsSet;
begin
  Result := FAllMapsSet;
end;

function TActivMapWithLayers.GetLayers: IActiveMapsSet;
begin
  Result := FLayersSet;
end;

procedure TActivMapWithLayers.SelectLayerByGUID(AMapGUID: TGUID);
begin
  LockWrite;
  try
    FLayerSetSelectNotyfier.NotifyByGUID(AMapGUID);
  finally
    UnlockWrite;
  end;
end;

procedure TActivMapWithLayers.UnSelectLayerByGUID(AMapGUID: TGUID);
begin
  LockWrite;
  try
    FLayerSetUnselectNotyfier.NotifyByGUID(AMapGUID);
  finally
    UnlockWrite;
  end;
end;

end.
