unit u_MapsConfigByConfigDataProvider;

interface

uses
  IniFiles,
  i_IGUIDList,
  i_IActiveMapsConfig,
  i_IConfigDataWriteProvider,
  i_ActiveMapsConfigSaveLoad;

type
  TMapsConfigByConfigDataProvider = class(TInterfacedObject, IActiveMapsConfigSaver, IActiveMapsConfigLoader)
  protected
    FProvider: IConfigDataWriteProvider;
    procedure SaveMap(AConfig: IActiveMapWithHybrConfig);
    procedure SaveHybrids(AConfig: IActiveMapWithHybrConfig);
    procedure LoadMap(AConfig: IActiveMapWithHybrConfig);
    procedure LoadHybrids(AConfig: IActiveMapWithHybrConfig);
    procedure LoadHybridGUIDs(AConfig: IActiveMapWithHybrConfig; AGUIDList: IGUIDInterfaceList);
    procedure LoadHybridByList(AConfig: IActiveMapWithHybrConfig; AGUIDList: IGUIDInterfaceList);
  protected
    procedure Save(AConfig: IActiveMapWithHybrConfig);
    procedure Load(AConfig: IActiveMapWithHybrConfig);
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
  i_MapTypes;

const
  CKeyNameMap = 'Map';
  CKeyNameLayer = 'Layer';

{ TMapsConfigByConfigDataProvider }

constructor TMapsConfigByConfigDataProvider.Create(
  AProvider: IConfigDataWriteProvider);
begin
  FProvider := AProvider;
end;

destructor TMapsConfigByConfigDataProvider.Destroy;
begin
  FProvider := nil;
  inherited;
end;

procedure TMapsConfigByConfigDataProvider.Load(
  AConfig: IActiveMapWithHybrConfig);
begin
  LoadMap(AConfig);
  LoadHybrids(AConfig);
end;

procedure TMapsConfigByConfigDataProvider.LoadHybridByList(
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

procedure TMapsConfigByConfigDataProvider.LoadHybridGUIDs(
  AConfig: IActiveMapWithHybrConfig; AGUIDList: IGUIDInterfaceList);
var
  VList: TStringList;
  i: Integer;
  VKeyName: string;
  VGUIDString: string;
  VGUID: TGUID;
  VMap: IMapType;
begin
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

procedure TMapsConfigByConfigDataProvider.LoadHybrids(
  AConfig: IActiveMapWithHybrConfig);
var
  VGUIDList: IGUIDInterfaceList;
begin
  VGUIDList := TGUIDInterfaceList.Create;
  LoadHybridGUIDs(AConfig, VGUIDList);
  LoadHybridByList(AConfig, VGUIDList);
  VGUIDList := nil;
end;

procedure TMapsConfigByConfigDataProvider.LoadMap(
  AConfig: IActiveMapWithHybrConfig);
var
  VGUIDString: string;
  VGUID: TGUID;
begin
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
  AConfig.SelectMapByGUID(VGUID);
end;

procedure TMapsConfigByConfigDataProvider.Save(
  AConfig: IActiveMapWithHybrConfig);
begin
  FProvider.DeleteValues;
  SaveMap(AConfig);
  SaveHybrids(AConfig);
end;

procedure TMapsConfigByConfigDataProvider.SaveHybrids(
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

procedure TMapsConfigByConfigDataProvider.SaveMap(
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
