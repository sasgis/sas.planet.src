unit u_ActiveMapConfig;

interface

uses
  Windows,
  SysUtils,
  i_JclNotify,
  i_IGUIDList,
  i_MapTypes,
  i_IActiveMapsConfig,
  u_ConfigDataElementBase,
  UMapType;

type
  TActiveMapConfig = class(TInterfacedObject, IActiveMapConfig)
  protected
    FAllowNil: Boolean;
    FMapsList: IMapTypeList;
    FSelectedMap: TMapType;
    FMapChangeNotifier: IJclNotifier;
  public
    constructor Create(AAllowNil: Boolean; AMapGUID: TGUID; AMapsList: IMapTypeList);
    destructor Destroy; override;
    procedure SelectMapByGUID(AMapGUID: TGUID);
    function GetSelectedMapGUID: TGUID;
    function GetMapsList: IMapTypeList;
    function GetMapChangeNotifier: IJclNotifier;
  end;

  TActiveMapConfigNew = class(TConfigDataElementBase, IActiveMap)
  private
    FSelectedGUID: TGUID;
    FMapsList: IMapTypeList;
    FSingeMapsList: IGUIDInterfaceList;
  protected
    FMainMapChangeNotyfier: IJclNotifier;
    FMainMapListener: IJclListener;
    procedure OnMainMapChange(AGUID: TGUID);
  protected
    function GetSelectedGUID: TGUID;
    function GetMapSingle(AMapGUID: TGUID): IActiveMapSingle;
    function GetMapsList: IMapTypeList;
  public
    constructor Create(AMainMapChangeNotyfier: IJclNotifier; ASingeMapsList: IGUIDInterfaceList; AMapsList: IMapTypeList);
    destructor Destroy; override;
  end;

implementation

uses
  ActiveX,
  u_JclNotify,
  c_ZeroGUID,
  u_NotifyWithGUIDEvent,
  u_MapChangeMessage;

{ TActiveMapConfig }

constructor TActiveMapConfig.Create(AAllowNil: Boolean; AMapGUID: TGUID; AMapsList: IMapTypeList);
var
  VMap: IMapType;
  VGUID: TGUID;
  VEnum: IEnumGUID;
  i: Cardinal;
begin
  FAllowNil := AAllowNil;
  FMapsList := AMapsList;
  FSelectedMap := nil;
  if not IsEqualGUID(AMapGUID, CGUID_Zero) then begin
    VMap := FMapsList.GetMapTypeByGUID(AMapGUID);
    if VMap <> nil then begin
      FSelectedMap := VMap.MapType;
    end;
  end;
  if (FSelectedMap = nil) and not FAllowNil then begin
    VEnum := FMapsList.GetIterator;
    if VEnum.Next(1, VGUID, i) = S_OK then begin
      VMap := FMapsList.GetMapTypeByGUID(VGUID);
      if VMap <> nil then begin
        FSelectedMap := VMap.MapType;
      end;
    end;
  end;
  FMapChangeNotifier := TJclBaseNotifier.Create;
end;

destructor TActiveMapConfig.Destroy;
begin
  FMapChangeNotifier := nil;
  FSelectedMap := nil;
  FMapsList := nil;
  inherited;
end;

procedure TActiveMapConfig.SelectMapByGUID(AMapGUID: TGUID);
var
  VOldSelected: TMapType;
  VMessage: IJclNotificationMessage;
  VMap: IMapType;
begin
  if not IsEqualGUID(AMapGUID, CGUID_Zero) then begin
    VMap := FMapsList.GetMapTypeByGUID(AMapGUID);
    if VMap <> nil then begin
      VOldSelected := TMapType(InterlockedExchange(Integer(FSelectedMap), Integer(VMap.MapType)));
      if VOldSelected <> VMap.MapType then begin
        VMessage := TMapChangeMessage.Create(VOldSelected, VMap.MapType);
        FMapChangeNotifier.Notify(VMessage);
        VMessage := nil;
      end;
    end;
  end else begin
    if FAllowNil then begin
      VOldSelected := TMapType(InterlockedExchange(Integer(FSelectedMap), 0));
      if VOldSelected <> nil then begin
        VMessage := TMapChangeMessage.Create(VOldSelected, nil);
        FMapChangeNotifier.Notify(VMessage);
        VMessage := nil;
      end;
    end;
  end;
end;

function TActiveMapConfig.GetMapChangeNotifier: IJclNotifier;
begin
  Result := FMapChangeNotifier;
end;

function TActiveMapConfig.GetSelectedMapGUID: TGUID;
var
  VMap: TMapType;
begin
  VMap := FSelectedMap;
  if VMap <> nil then begin
    Result := VMap.GUID;
  end else begin
    Result := CGUID_Zero;
  end;
end;

function TActiveMapConfig.GetMapsList: IMapTypeList;
begin
  Result := FMapsList;
end;

{ TActiveMapConfigNew }

constructor TActiveMapConfigNew.Create(AMainMapChangeNotyfier: IJclNotifier;
  ASingeMapsList: IGUIDInterfaceList; AMapsList: IMapTypeList);
begin
  FMapsList := AMapsList;
  FSingeMapsList := ASingeMapsList;
  FMainMapChangeNotyfier := AMainMapChangeNotyfier;
  FMainMapListener := TNotifyWithGUIDEventListener.Create(Self.OnMainMapChange);
  FMainMapChangeNotyfier.Add(FMainMapListener);
end;

destructor TActiveMapConfigNew.Destroy;
begin
  FMainMapChangeNotyfier.Remove(FMainMapListener);
  FMainMapListener := nil;
  FMainMapChangeNotyfier := nil;
  FMapsList := nil;
  FSingeMapsList := nil;
  inherited;
end;

function TActiveMapConfigNew.GetMapSingle(AMapGUID: TGUID): IActiveMapSingle;
begin
  if FMapsList.GetMapTypeByGUID(AMapGUID) <> nil then begin
    Result := IActiveMapSingle(FSingeMapsList.GetByGUID(AMapGUID));
  end;
end;

function TActiveMapConfigNew.GetMapsList: IMapTypeList;
begin
  Result := FMapsList;
end;

function TActiveMapConfigNew.GetSelectedGUID: TGUID;
begin
  LockRead;
  try
    Result := FSelectedGUID;
  finally
    UnlockRead;
  end;
end;

procedure TActiveMapConfigNew.OnMainMapChange(AGUID: TGUID);
begin
  LockWrite;
  try
    if not IsEqualGUID(FSelectedGUID, AGUID) then begin
      FSelectedGUID := AGUID;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
