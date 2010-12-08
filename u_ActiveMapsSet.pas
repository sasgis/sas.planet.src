unit u_ActiveMapsSet;

interface

uses
  i_JclNotify,
  i_IGUIDList,
  i_MapTypes,
  i_IActiveMapsConfig,
  u_ConfigDataElementBase;

type
  TActiveMapsSet = class(TConfigDataElementBase, IActiveMapsSet)
  private
    FMapsList: IMapTypeList;

    FSingeMapsList: IGUIDInterfaceList;
    FSelectedMapsList: IMapTypeList;

    FMainMapChangeNotyfier: IJclNotifier;
    FMainMapListener: IJclListener;

    FLayerSetSelectNotyfier: IJclNotifier;
    FLayerSetUnselectNotyfier: IJclNotifier;

    FLayerSetSelectListener: IJclListener;
    FLayerSetUnselectListener: IJclListener;

    procedure OnMainMapChange(AGUID: TGUID);
    procedure OnLayerSetSelectChange(AGUID: TGUID);
    procedure OnLayerSetUnselectChange(AGUID: TGUID);
  protected
    function IsGUIDSelected(AMapGUID: TGUID): Boolean;
    function GetMapSingle(AMapGUID: TGUID): IActiveMapSingle;
    function GetSelectedMapsList: IMapTypeList;
    function GetMapsList: IMapTypeList;
  public
    constructor Create(
      AMapsList: IMapTypeList;
      ASingeMapsList: IGUIDInterfaceList;
      AMainMapChangeNotyfier: IJclNotifier;
      ALayerSetSelectNotyfier: IJclNotifier;
      ALayerSetUnselectNotyfier: IJclNotifier
    );
    destructor Destroy; override;
  end;

implementation

uses
  ActiveX,
  u_MapTypeList,
  u_NotifyWithGUIDEvent;

{ TActiveMapsSet }

constructor TActiveMapsSet.Create(AMapsList: IMapTypeList;
  ASingeMapsList: IGUIDInterfaceList; AMainMapChangeNotyfier,
  ALayerSetSelectNotyfier, ALayerSetUnselectNotyfier: IJclNotifier);
begin
  FMapsList := AMapsList;
  FSingeMapsList := ASingeMapsList;

  FMainMapChangeNotyfier := AMainMapChangeNotyfier;
  if FMainMapChangeNotyfier <> nil then begin
    FMainMapListener := TNotifyWithGUIDEventListener.Create(Self.OnMainMapChange);
    FMainMapChangeNotyfier.Add(FMainMapListener);
  end;

  FLayerSetSelectNotyfier := ALayerSetSelectNotyfier;
  if FLayerSetSelectNotyfier <> nil then begin
    FLayerSetSelectListener := TNotifyWithGUIDEventListener.Create(Self.OnLayerSetSelectChange);
    FLayerSetSelectNotyfier.Add(FLayerSetSelectListener);
  end;

  FLayerSetUnselectNotyfier := ALayerSetUnselectNotyfier;
  if FLayerSetUnselectNotyfier <> nil then begin
    FLayerSetUnselectListener := TNotifyWithGUIDEventListener.Create(Self.OnLayerSetUnselectChange);
    FLayerSetUnselectNotyfier.Add(FLayerSetUnselectListener);
  end;
end;

destructor TActiveMapsSet.Destroy;
begin
  if FMainMapChangeNotyfier <> nil then begin
    FMainMapChangeNotyfier.Remove(FMainMapListener);
  end;
  FMainMapListener := nil;
  FMainMapChangeNotyfier := nil;

  if FLayerSetSelectNotyfier <> nil then begin
    FLayerSetSelectNotyfier.Remove(FLayerSetSelectListener);
  end;
  FLayerSetSelectListener := nil;
  FLayerSetSelectNotyfier := nil;

  if FLayerSetUnselectNotyfier <> nil then begin
    FLayerSetUnselectNotyfier.Remove(FLayerSetUnselectListener);
  end;
  FLayerSetUnselectListener := nil;
  FLayerSetUnselectNotyfier := nil;

  inherited;
end;

function TActiveMapsSet.GetMapSingle(AMapGUID: TGUID): IActiveMapSingle;
begin
  if FMapsList.GetMapTypeByGUID(AMapGUID) <> nil then begin
    Result := IActiveMapSingle(FSingeMapsList.GetByGUID(AMapGUID));
  end;
end;

function TActiveMapsSet.GetMapsList: IMapTypeList;
begin
  Result := FMapsList;
end;

function TActiveMapsSet.GetSelectedMapsList: IMapTypeList;
begin
  LockRead;
  try
    Result := FSelectedMapsList;
  finally
    UnlockRead;
  end;
end;

function TActiveMapsSet.IsGUIDSelected(AMapGUID: TGUID): Boolean;
begin
  LockRead;
  try
    Result := FSelectedMapsList.GetMapTypeByGUID(AMapGUID) <> nil;
  finally
    UnlockRead;
  end;
end;

procedure TActiveMapsSet.OnLayerSetSelectChange(AGUID: TGUID);
var
  VMapType: IMapType;
  VList: TMapTypeList;
  VEnun: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
begin
  VMapType := FMapsList.GetMapTypeByGUID(AGUID);
  if VMapType <> nil then begin
    LockWrite;
    try
      if FSelectedMapsList.GetMapTypeByGUID(AGUID) = nil then begin
        VList := TMapTypeList.Create(True);
        VEnun := FSelectedMapsList.GetIterator;
        while VEnun.Next(1, VGUID, i) = S_OK do begin
          VList.Add(FMapsList.GetMapTypeByGUID(VGUID));
        end;
        VList.Add(VMapType);
        FSelectedMapsList := VList;
        SetChanged;
      end;
    finally
      UnlockWrite;
    end;
  end;
end;

procedure TActiveMapsSet.OnLayerSetUnselectChange(AGUID: TGUID);
var
  VMapType: IMapType;
  VList: TMapTypeList;
  VEnun: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
begin
  VMapType := FMapsList.GetMapTypeByGUID(AGUID);
  if VMapType <> nil then begin
    LockWrite;
    try
      if FSelectedMapsList.GetMapTypeByGUID(AGUID) <> nil then begin
        VList := TMapTypeList.Create(True);
        VEnun := FSelectedMapsList.GetIterator;
        while VEnun.Next(1, VGUID, i) = S_OK do begin
          if not IsEqualGUID(VGUID, AGUID) then begin
            VList.Add(FMapsList.GetMapTypeByGUID(VGUID));
          end;
        end;
        FSelectedMapsList := VList;
        SetChanged;
      end;
    finally
      UnlockWrite;
    end;
  end;
end;

procedure TActiveMapsSet.OnMainMapChange(AGUID: TGUID);
var
  VMapSingle: IActiveMapSingle;
  VList: TMapTypeList;
  VEnun: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
begin
  LockWrite;
  try
    VList := TMapTypeList.Create(True);
    VEnun := FMapsList.GetIterator;
    while VEnun.Next(1, VGUID, i) = S_OK do begin
      VMapSingle := IActiveMapSingle(FSingeMapsList.GetByGUID(VGUID));
      if VMapSingle.GetIsActive then begin
        VList.Add(VMapSingle.GetMapType);
      end;
    end;
    FSelectedMapsList := VList;
  finally
    UnlockWrite;
  end;
end;

end.
