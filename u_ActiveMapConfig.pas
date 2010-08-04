unit u_ActiveMapConfig;

interface

uses
  Windows,
  SysUtils,
  i_JclNotify,
  i_MapTypes,
  i_IActiveMapsConfig,
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

implementation

uses
  ActiveX,
  u_JclNotify,
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

end.
