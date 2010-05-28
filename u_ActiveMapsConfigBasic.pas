unit u_ActiveMapsConfigBasic;

interface

uses
  Windows,
  SysUtils,
  i_JclNotify,
  i_MapTypes,
  i_IActiveMapsConfig,
  UMapType;

type
  TActiveMapsConfigBasic = class(TInterfacedObject, IActiveMapWithHybrConfig)
  protected
    FSynchronizer: TMultiReadExclusiveWriteSynchronizer;
    FMapsList: IMapTypeList;
    FLayersList: IMapTypeList;
    FSelectedMap: TMapType;
    FSelectedHybr: array of TMapType;
    FMapChangeNotifier: IJclNotifier;
    FHybrChangeNotifier: IJclNotifier;
    function _IsHybrSelected(AMap: TMapType): Boolean; overload;
    function _IsHybrSelected(AMapGUID: TGUID): Boolean; overload;
  public
    constructor Create(AMap: TMapType; AMapsList: IMapTypeList;
      ALayersList: IMapTypeList);
    destructor Destroy; override;
    procedure SelectMap(AMap: TMapType);
    procedure SelectMapByGUID(AMapGUID: TGUID);
    function GetSelectedMap: TMapType;
    function GetSelectedMapGUID: TGUID;
    procedure SelectHybr(AMap: TMapType);
    procedure SelectHybrByGUID(AMapGUID: TGUID);
    procedure UnSelectHybr(AMap: TMapType);
    procedure UnSelectHybrByGUID(AMapGUID: TGUID);
    function IsHybrSelected(AMap: TMapType): Boolean;
    function IsHybrGUIDSelected(AMapGUID: TGUID): Boolean;
    function GetMapsList: IMapTypeList;
    function GetHybrList: IMapTypeList;
    function GetMapChangeNotifier: IJclNotifier;
    function GetHybrChangeNotifier: IJclNotifier;
  end;

implementation

uses
  u_JclNotify,
  i_IHybrChangeMessage,
  u_MapChangeMessage,
  u_HybrChangeMessage;

{ TActiveMapsConfigBasic }

constructor TActiveMapsConfigBasic.Create(AMap: TMapType; AMapsList: IMapTypeList;
      ALayersList: IMapTypeList);
begin
  FSynchronizer := TMultiReadExclusiveWriteSynchronizer.Create;
  FMapsList := AMapsList;
  FLayersList := ALayersList;
  FSelectedMap := nil;
  if AMap <> nil then begin
    if FMapsList.GetMapTypeByGUID(AMap.GUID) <> nil then begin
      FSelectedMap := AMap;
    end;
  end;
  FSelectedHybr := nil;
  FMapChangeNotifier := TJclBaseNotifier.Create;
  FHybrChangeNotifier := TJclBaseNotifier.Create;
end;

destructor TActiveMapsConfigBasic.Destroy;
begin
  FSynchronizer.BeginWrite;
  try
    FMapChangeNotifier := nil;
    FHybrChangeNotifier := nil;
    FSelectedHybr := nil;
    FSelectedMap := nil;
    FMapsList := nil;
    FLayersList := nil;
  finally
    FSynchronizer.EndWrite;
    FreeAndNil(FSynchronizer);
  end;
  inherited;
end;

function TActiveMapsConfigBasic._IsHybrSelected(AMap: TMapType): Boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to Length(FSelectedHybr) - 1 do begin
    if FSelectedHybr[i] = AMap then begin
      Result := True;
      Break;
    end;
  end;
end;

function TActiveMapsConfigBasic._IsHybrSelected(AMapGUID: TGUID): Boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to Length(FSelectedHybr) - 1 do begin
    if IsEqualGUID(FSelectedHybr[i].GUID, AMapGUID) then begin
      Result := True;
      Break;
    end;
  end;
end;

function TActiveMapsConfigBasic.IsHybrSelected(AMap: TMapType): Boolean;
begin
  Result := False;
  if AMap <> nil then begin
    if FLayersList.GetMapTypeByGUID(AMap.GUID) <> nil then begin
      FSynchronizer.BeginRead;
      try
        Result := _IsHybrSelected(AMap);
      finally
        FSynchronizer.EndRead;
      end;
    end;
  end;
end;

function TActiveMapsConfigBasic.IsHybrGUIDSelected(
  AMapGUID: TGUID): Boolean;
begin
  Result := False;
  if not IsEqualGUID(AMapGUID, CGUID_Zero) then begin
    if FLayersList.GetMapTypeByGUID(AMapGUID) <> nil then begin
      FSynchronizer.BeginRead;
      try
        Result := _IsHybrSelected(AMapGUID);
      finally
        FSynchronizer.EndRead;
      end;
    end;
  end;
end;

procedure TActiveMapsConfigBasic.SelectHybr(AMap: TMapType);
var
  VIndex: integer;
  VMessage: IJclNotificationMessage;
begin
  VMessage := nil;
  if AMap <> nil then begin
    if FLayersList.GetMapTypeByGUID(AMap.GUID) <> nil then begin
      FSynchronizer.BeginWrite;
      try
        if not _IsHybrSelected(AMap) then begin
          VIndex := length(FSelectedHybr);
          SetLength(FSelectedHybr, VIndex + 1);
          FSelectedHybr[VIndex] := AMap;
          VMessage := THybrChangeMessage.Create(AMap, hcaSelect);
        end;
      finally
        FSynchronizer.EndWrite;
      end;
      if VMessage <> nil then begin
        FHybrChangeNotifier.Notify(VMessage);
        VMessage := nil;
      end;
    end;
  end;
end;

procedure TActiveMapsConfigBasic.SelectHybrByGUID(AMapGUID: TGUID);
var
  VIndex: integer;
  VMessage: IJclNotificationMessage;
  VMap: IMapType;
begin
  VMessage := nil;
  if not IsEqualGUID(AMapGUID, CGUID_Zero) then begin
    VMap := FLayersList.GetMapTypeByGUID(AMapGUID);
    if VMap <> nil then begin
      FSynchronizer.BeginWrite;
      try
        if not _IsHybrSelected(AMapGUID) then begin
          VIndex := length(FSelectedHybr);
          SetLength(FSelectedHybr, VIndex + 1);
          FSelectedHybr[VIndex] := VMap.MapType;
          VMessage := THybrChangeMessage.Create(VMap.MapType, hcaSelect);
        end;
      finally
        FSynchronizer.EndWrite;
      end;
      if VMessage <> nil then begin
        FHybrChangeNotifier.Notify(VMessage);
        VMessage := nil;
      end;
    end;
  end;
end;

procedure TActiveMapsConfigBasic.SelectMap(AMap: TMapType);
var
  VOldSelected: TMapType;
  VMessage: IJclNotificationMessage;
begin
  if AMap <> nil then begin
    if FMapsList.GetMapTypeByGUID(AMap.GUID) <> nil then begin
      VOldSelected := TMapType(InterlockedExchange(Integer(FSelectedMap), Integer(AMap)));
      if VOldSelected <> AMap then begin
        VMessage := TMapChangeMessage.Create(VOldSelected, AMap);
        FMapChangeNotifier.Notify(VMessage);
        VMessage := nil;
      end;
    end;
  end else begin
    VOldSelected := TMapType(InterlockedExchange(Integer(FSelectedMap), Integer(AMap)));
    if VOldSelected <> AMap then begin
      VMessage := TMapChangeMessage.Create(VOldSelected, AMap);
      FMapChangeNotifier.Notify(VMessage);
      VMessage := nil;
    end;
  end;
end;

procedure TActiveMapsConfigBasic.SelectMapByGUID(AMapGUID: TGUID);
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
    VOldSelected := TMapType(InterlockedExchange(Integer(FSelectedMap), 0));
    if VOldSelected <> nil then begin
      VMessage := TMapChangeMessage.Create(VOldSelected, nil);
      FMapChangeNotifier.Notify(VMessage);
      VMessage := nil;
    end;
  end;
end;

procedure TActiveMapsConfigBasic.UnSelectHybr(AMap: TMapType);
var
  VIndex: integer;
  VMessage: IJclNotificationMessage;
  i: integer;
  VCount: Integer;
begin
  VMessage := nil;
  if AMap <> nil then begin
    if FLayersList.GetMapTypeByGUID(AMap.GUID) <> nil then begin
      FSynchronizer.BeginWrite;
      try
        VIndex := -1;
        VCount := Length(FSelectedHybr);
        for i := 0 to VCount - 1 do begin
          if FSelectedHybr[i] = AMap then begin
            VIndex := i;
            Break;
          end;
        end;
        if VIndex >= 0 then begin
          for i := VIndex to VCount - 2 do begin
            FSelectedHybr[i] := FSelectedHybr[i + 1];
          end;
          SetLength(FSelectedHybr, VCount - 1);
          VMessage := THybrChangeMessage.Create(AMap, hcaUnselect);
        end;
      finally
        FSynchronizer.EndWrite;
      end;
      if VMessage <> nil then begin
        FHybrChangeNotifier.Notify(VMessage);
        VMessage := nil;
      end;
    end;
  end;
end;

procedure TActiveMapsConfigBasic.UnSelectHybrByGUID(AMapGUID: TGUID);
var
  VIndex: integer;
  VMessage: IJclNotificationMessage;
  i: integer;
  VCount: Integer;
  VMap: IMapType;
begin
  VMessage := nil;
  if not IsEqualGUID(AMapGUID, CGUID_Zero) then begin
    VMap := FLayersList.GetMapTypeByGUID(AMapGUID);
    if VMap <> nil then begin
      FSynchronizer.BeginWrite;
      try
        VIndex := -1;
        VCount := Length(FSelectedHybr);
        for i := 0 to VCount - 1 do begin
          if FSelectedHybr[i] = VMap.MapType then begin
            VIndex := i;
            Break;
          end;
        end;
        if VIndex >= 0 then begin
          for i := VIndex to VCount - 2 do begin
            FSelectedHybr[i] := FSelectedHybr[i + 1];
          end;
          SetLength(FSelectedHybr, VCount - 1);
          VMessage := THybrChangeMessage.Create(VMap.MapType, hcaUnselect);
        end;
      finally
        FSynchronizer.EndWrite;
      end;
      if VMessage <> nil then begin
        FHybrChangeNotifier.Notify(VMessage);
        VMessage := nil;
      end;
    end;
  end;
end;

function TActiveMapsConfigBasic.GetHybrChangeNotifier: IJclNotifier;
begin
  Result := FHybrChangeNotifier;
end;

function TActiveMapsConfigBasic.GetMapChangeNotifier: IJclNotifier;
begin
  Result := FMapChangeNotifier;
end;

function TActiveMapsConfigBasic.GetSelectedMap: TMapType;
begin
  Result := FSelectedMap;
end;

function TActiveMapsConfigBasic.GetSelectedMapGUID: TGUID;
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

function TActiveMapsConfigBasic.GetHybrList: IMapTypeList;
begin
  Result := FLayersList;
end;

function TActiveMapsConfigBasic.GetMapsList: IMapTypeList;
begin
  Result := FMapsList;
end;

end.
