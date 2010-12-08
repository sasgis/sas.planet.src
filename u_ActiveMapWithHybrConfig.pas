unit u_ActiveMapWithHybrConfig;

interface

uses
  Windows,
  SysUtils,
  i_JclNotify,
  i_MapTypes,
  i_IActiveMapsConfig,
  u_ActiveMapConfig,
  UMapType;

type
  TActiveMapWithHybrConfig = class(TActiveMapConfig, IActiveMapWithHybrConfig)
  protected
    FSynchronizer: TMultiReadExclusiveWriteSynchronizer;
    FLayersList: IMapTypeList;
    FSelectedHybr: array of TMapType;
    FHybrChangeNotifier: IJclNotifier;
    function _IsHybrSelected(AMap: TMapType): Boolean; overload;
    function _IsHybrSelected(AMapGUID: TGUID): Boolean; overload;
  public
    constructor Create(AAllowNil: Boolean; AMapGUID: TGUID; AMapsList: IMapTypeList;
      ALayersList: IMapTypeList);
    destructor Destroy; override;
    procedure SelectHybrByGUID(AMapGUID: TGUID);
    procedure UnSelectHybrByGUID(AMapGUID: TGUID);
    function IsHybrGUIDSelected(AMapGUID: TGUID): Boolean;
    function GetHybrList: IMapTypeList;
    function GetHybrChangeNotifier: IJclNotifier;
  end;

implementation

uses
  u_JclNotify,
  c_ZeroGUID,
  i_IHybrChangeMessage,
  u_HybrChangeMessage;

{ TActiveMapWithHybrConfig }

constructor TActiveMapWithHybrConfig.Create(AAllowNil: Boolean; AMapGUID: TGUID; AMapsList: IMapTypeList;
  ALayersList: IMapTypeList);
begin
  inherited Create(AAllowNil, AMapGUID, AMapsList);
  FSynchronizer := TMultiReadExclusiveWriteSynchronizer.Create;
  FLayersList := ALayersList;
  FSelectedHybr := nil;
  FHybrChangeNotifier := TJclBaseNotifier.Create;
end;

destructor TActiveMapWithHybrConfig.Destroy;
begin
  FSynchronizer.BeginWrite;
  try
    FHybrChangeNotifier := nil;
    FSelectedHybr := nil;
    FLayersList := nil;
  finally
    FSynchronizer.EndWrite;
    FreeAndNil(FSynchronizer);
  end;
  inherited;
end;

function TActiveMapWithHybrConfig._IsHybrSelected(AMap: TMapType): Boolean;
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

function TActiveMapWithHybrConfig._IsHybrSelected(AMapGUID: TGUID): Boolean;
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

function TActiveMapWithHybrConfig.IsHybrGUIDSelected(
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

procedure TActiveMapWithHybrConfig.SelectHybrByGUID(AMapGUID: TGUID);
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

procedure TActiveMapWithHybrConfig.UnSelectHybrByGUID(AMapGUID: TGUID);
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

function TActiveMapWithHybrConfig.GetHybrChangeNotifier: IJclNotifier;
begin
  Result := FHybrChangeNotifier;
end;

function TActiveMapWithHybrConfig.GetHybrList: IMapTypeList;
begin
  Result := FLayersList;
end;

end.
