unit u_MainActiveMap;

interface

uses
  i_IGUIDList,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_MapTypes,
  i_IActiveMapsConfig,
  u_ConfigDataElementComplexBase,
  u_NotifyWithGUIDEvent;

type
  TMainActiveMap = class(TConfigDataElementComplexBase, IMainActiveMap)
  private
    FMapsList: IMapTypeList;
    FMainMapChangeNotyfier: INotifierWithGUID;
    FSingeMapsList: IGUIDInterfaceList;
    FActiveMap: IActiveMap;
    FMapsSet: IActiveMapsSet;
  protected
    property MainMapChangeNotyfier: INotifierWithGUID read FMainMapChangeNotyfier;
    property SingeMapsList: IGUIDInterfaceList read FSingeMapsList;
  protected
    procedure SelectMainByGUID(AMapGUID: TGUID);
    function GetActiveMap: IActiveMap;
    function GetMapsSet: IActiveMapsSet;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  public
    constructor Create(AMapsList: IMapTypeList);
    destructor Destroy; override;
  end;
implementation

uses
  SysUtils,
  ActiveX,
  u_GUIDInterfaceList,
  u_ActiveMapSingleAbstract,
  u_ActiveMapsSet,
  u_ActiveMapConfig;

const
  CKeyNameMap = 'Map';

{ TMainActiveMap }

constructor TMainActiveMap.Create(AMapsList: IMapTypeList);
var
  VEnun: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
  VMapType: IMapType;
  VSingleMap: IActiveMapSingle;
begin
  inherited Create;
  FMapsList := AMapsList;
  FMainMapChangeNotyfier := TNotifierWithGUID.Create;
  FSingeMapsList := TGUIDInterfaceList.Create(False);

  VEnun := FMapsList.GetIterator;
  while VEnun.Next(1, VGUID, i) = S_OK do begin
    VMapType := FMapsList.GetMapTypeByGUID(VGUID);
    VSingleMap := TActiveMapSingleMainMap.Create(VMapType, FMainMapChangeNotyfier);
    FSingeMapsList.Add(VGUID, VSingleMap);
    Add(VSingleMap, nil);
  end;
  FActiveMap := TActiveMapConfigNew.Create(FMainMapChangeNotyfier, FSingeMapsList, FMapsList);
  Add(FActiveMap, nil);

  FMapsSet :=  TActiveMapsSet.Create(
    FMapsList,
    FSingeMapsList,
    MainMapChangeNotyfier,
    nil,
    nil
  );
  Add(FMapsSet, nil);
end;

destructor TMainActiveMap.Destroy;
begin
  FMainMapChangeNotyfier := nil;
  FMapsList := nil;
  FSingeMapsList := nil;
  FActiveMap := nil;
  inherited;
end;

procedure TMainActiveMap.DoReadConfig(AConfigData: IConfigDataProvider);
var
  VGUIDString: string;
  VGUID: TGUID;
  VValidGUID: Boolean;
begin
  inherited;
  VValidGUID := False;
  if AConfigData <> nil then begin
    VGUIDString := AConfigData.ReadString(CKeyNameMap, '');
    if VGUIDString <> '' then begin
      try
        VGUID := StringToGUID(VGUIDString);
        VValidGUID := True;
      except
      end;
    end;
  end;
  if VValidGUID then begin
    if FMapsList.GetMapTypeByGUID(VGUID) <> nil then begin
      SelectMainByGUID(VGUID);
    end;
  end;
end;

procedure TMainActiveMap.DoWriteConfig(AConfigData: IConfigDataWriteProvider);
var
  VGUIDString: string;
  VGUID: TGUID;
begin
  inherited;
  VGUID := FActiveMap.GetSelectedGUID;
  VGUIDString := GUIDToString(VGUID);
  AConfigData.WriteString(CKeyNameMap, VGUIDString);
end;

function TMainActiveMap.GetActiveMap: IActiveMap;
begin
  Result := FActiveMap;
end;

function TMainActiveMap.GetMapsSet: IActiveMapsSet;
begin
  Result := FMapsSet;
end;

procedure TMainActiveMap.SelectMainByGUID(AMapGUID: TGUID);
begin
  LockWrite;
  try
    FMainMapChangeNotyfier.NotifyByGUID(AMapGUID);
  finally
    UnlockWrite;
  end;
end;

end.
