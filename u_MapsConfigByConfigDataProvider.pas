unit u_MapsConfigByConfigDataProvider;

interface

uses
  i_IGUIDList,
  i_IActiveMapsConfig,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_ActiveMapsConfigSaveLoad;

type
  TMapsConfigLoaderByConfigDataProvider = class(TInterfacedObject, IActiveMapsConfigLoader)
  protected
    FProvider: IConfigDataProvider;
    procedure LoadMap(AConfig: IActiveMapWithHybrConfig);
    procedure LoadHybrids(AConfig: IActiveMapWithHybrConfig);
    procedure LoadHybridGUIDs(AConfig: IActiveMapWithHybrConfig; AGUIDList: IGUIDInterfaceList);
    procedure LoadHybridByList(AConfig: IActiveMapWithHybrConfig; AGUIDList: IGUIDInterfaceList);
  protected
    procedure Load(AConfig: IActiveMapWithHybrConfig);
  public
    constructor Create(AProvider: IConfigDataProvider);
    destructor Destroy; override;
  end;

  TMapsConfigSaverByConfigDataProvider = class(TInterfacedObject, IActiveMapsConfigSaver)
  protected
    FProvider: IConfigDataWriteProvider;
    procedure SaveMap(AConfig: IActiveMapWithHybrConfig);
    procedure SaveHybrids(AConfig: IActiveMapWithHybrConfig);
  protected
    procedure Save(AConfig: IActiveMapWithHybrConfig);
  public
    constructor Create(AProvider: IConfigDataWriteProvider);
    destructor Destroy; override;
  end;

implementation

uses
  ActiveX,
  Classes,
  StrUtils,
  SysUtils,
  u_GUIDInterfaceList,
  c_ZeroGUID,
  i_MapTypes;

const
  CKeyNameMap = 'Map';
  CKeyNameLayer = 'Layer';

{ TMapsConfigByConfigDataProvider }

constructor TMapsConfigLoaderByConfigDataProvider.Create(
  AProvider: IConfigDataProvider);
begin
  FProvider := AProvider;
end;

destructor TMapsConfigLoaderByConfigDataProvider.Destroy;
begin
  FProvider := nil;
  inherited;
end;

procedure TMapsConfigLoaderByConfigDataProvider.Load(
  AConfig: IActiveMapWithHybrConfig);
begin
  LoadMap(AConfig);
  LoadHybrids(AConfig);
end;

procedure TMapsConfigLoaderByConfigDataProvider.LoadHybridByList(
  AConfig: IActiveMapWithHybrConfig; AGUIDList: IGUIDInterfaceList);
var
  VEnum: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
begin
  VEnum := AConfig.HybrList.GetIterator;
  while VEnum.Next(1, VGUID, i) = S_OK do begin
    if AGUIDList.GetByGUID(VGUID) <> nil then begin
      AConfig.SelectHybrByGUID(VGUID);
    end else begin
      AConfig.UnSelectHybrByGUID(VGUID);
    end;
  end;
end;

procedure TMapsConfigLoaderByConfigDataProvider.LoadHybridGUIDs(
  AConfig: IActiveMapWithHybrConfig; AGUIDList: IGUIDInterfaceList);
var
  VList: TStringList;
  i: Integer;
  VKeyName: string;
  VGUIDString: string;
  VGUID: TGUID;
  VMap: IMapType;
begin
  if FProvider <> nil then begin
    VList := TStringList.Create;
    try
      FProvider.ReadValuesList(VList);
      for i := 0 to VList.Count - 1 do begin
        VKeyName := VList.Strings[i];
        if SameText(LeftStr(VKeyName, length(CKeyNameLayer)), CKeyNameLayer) then begin
          VGUIDString := FProvider.ReadString(VKeyName, '');
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
            VMap := AConfig.HybrList.GetMapTypeByGUID(VGUID);
            if VMap <> nil then begin
              AGUIDList.Add(VGUID, VMap);
            end;
          end;
        end;
      end;
    finally
      VList.Free;
    end;
  end;
end;

procedure TMapsConfigLoaderByConfigDataProvider.LoadHybrids(
  AConfig: IActiveMapWithHybrConfig);
var
  VGUIDList: IGUIDInterfaceList;
begin
  VGUIDList := TGUIDInterfaceList.Create;
  LoadHybridGUIDs(AConfig, VGUIDList);
  LoadHybridByList(AConfig, VGUIDList);
  VGUIDList := nil;
end;

procedure TMapsConfigLoaderByConfigDataProvider.LoadMap(
  AConfig: IActiveMapWithHybrConfig);
var
  VGUIDString: string;
  VGUID: TGUID;
begin
  if FProvider <> nil then begin
    VGUIDString := FProvider.ReadString(CKeyNameMap, '');
    if VGUIDString <> '' then begin
      try
        VGUID := StringToGUID(VGUIDString);
      except
        VGUID := CGUID_Zero;
      end;
    end else begin
      VGUID := CGUID_Zero;
    end;
  end else begin
    VGUID := CGUID_Zero;
  end;
  AConfig.SelectMapByGUID(VGUID);
end;

constructor TMapsConfigSaverByConfigDataProvider.Create(
  AProvider: IConfigDataWriteProvider);
begin
  FProvider := AProvider;
end;

destructor TMapsConfigSaverByConfigDataProvider.Destroy;
begin
  FProvider := nil;
  inherited;
end;

procedure TMapsConfigSaverByConfigDataProvider.Save(
  AConfig: IActiveMapWithHybrConfig);
begin
  FProvider.DeleteValues;
  SaveMap(AConfig);
  SaveHybrids(AConfig);
end;

procedure TMapsConfigSaverByConfigDataProvider.SaveHybrids(
  AConfig: IActiveMapWithHybrConfig);
var
  VEnum: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
  VGUIDString: string;
  VIndex: Integer;
begin
  VIndex := 0;
  VEnum := AConfig.HybrList.GetIterator;
  while VEnum.Next(1, VGUID, i) = S_OK do begin
    if AConfig.IsHybrGUIDSelected(VGUID) then begin
      VGUIDString := GUIDToString(VGUID);
      FProvider.WriteString(CKeyNameLayer + IntToStr(VIndex), VGUIDString);
      Inc(VIndex);
    end;
  end;
end;

procedure TMapsConfigSaverByConfigDataProvider.SaveMap(
  AConfig: IActiveMapWithHybrConfig);
var
  VGUIDString: string;
  VGUID: TGUID;
begin
  VGUID := AConfig.GetSelectedMapGUID;
  VGUIDString := GUIDToString(VGUID);
  FProvider.WriteString(CKeyNameMap, VGUIDString);
end;

end.
