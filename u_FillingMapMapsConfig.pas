unit u_FillingMapMapsConfig;

interface

uses
  i_JclNotify,
  i_ActiveMapsConfig,
  i_MapTypes,
  i_FillingMapLayerConfig,
  u_MainActiveMap;

type
  TFillingMapMapsConfig = class(TMainActiveMap, IFillingMapMapsConfig)
  private
    FActualMap: IMapType;
    FSelectedMapChangeListener: IJclListener;
    FMainMapsConfig: IMainMapsConfig;
    FMainMapChangeListener: IJclListener;
    function CreateMapsList: IMapTypeList;
    procedure OnMainMapChange(Sender: TObject);
    procedure OnSelectedChange(const AGUID: TGUID);
    procedure SetActualMap(AValue: IMapType);
  protected
    function GetActualMap: IMapType;
  public
    constructor Create(AMapsConfig: IMainMapsConfig);
    destructor Destroy; override;
  end;

implementation

uses
  ActiveX,
  c_ZeroGUID,
  u_NotifyEventListener,
  u_NotifyWithGUIDEvent,
  u_MapTypeBasic,
  u_MapTypeList;

{ TFillingMapMapsConfig }

constructor TFillingMapMapsConfig.Create(AMapsConfig: IMainMapsConfig);
begin
  FMainMapsConfig := AMapsConfig;
  inherited Create(CreateMapsList);

  FMainMapChangeListener := TNotifyEventListener.Create(Self.OnMainMapChange);
  FMainMapsConfig.GetActiveMap.GetChangeNotifier.Add(FMainMapChangeListener);

  FSelectedMapChangeListener := TNotifyWithGUIDEventListener.Create(Self.OnSelectedChange);
  MainMapChangeNotyfier.Add(FSelectedMapChangeListener);

  SetActualMap(FMainMapsConfig.GetSelectedMapType);
end;

destructor TFillingMapMapsConfig.Destroy;
begin
  FMainMapsConfig.GetActiveMap.GetChangeNotifier.Remove(FMainMapChangeListener);
  FMainMapChangeListener := nil;

  MainMapChangeNotyfier.Remove(FSelectedMapChangeListener);
  FSelectedMapChangeListener := nil;

  FMainMapsConfig := nil;
  inherited;
end;

function TFillingMapMapsConfig.CreateMapsList: IMapTypeList;
var
  VSourceList: IMapTypeList;
  VMap: IMapType;
  VList: TMapTypeList;
  VEnun: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
begin
  VSourceList := FMainMapsConfig.GetAllActiveMapsSet.GetMapsList;
  VList := TMapTypeList.Create(True);
  Result := VList;
  VList.Add(TMapTypeBasic.Create(nil));
  VEnun := VSourceList.GetIterator;
  while VEnun.Next(1, VGUID, i) = S_OK do begin
    VMap := VSourceList.GetMapTypeByGUID(VGUID);
    VList.Add(VMap);
  end;
end;

function TFillingMapMapsConfig.GetActualMap: IMapType;
begin
  LockRead;
  try
    Result := FActualMap;
  finally
    UnlockRead;
  end;
end;

procedure TFillingMapMapsConfig.OnMainMapChange(Sender: TObject);
var
  VGUID: TGUID;
begin
  VGUID := GetActiveMap.GetSelectedGUID;
  if IsEqualGUID(VGUID, CGUID_Zero) then begin
    SetActualMap(FMainMapsConfig.GetSelectedMapType);
  end;
end;

procedure TFillingMapMapsConfig.OnSelectedChange(const AGUID: TGUID);
begin
  if IsEqualGUID(AGUID, CGUID_Zero) then begin
    SetActualMap(FMainMapsConfig.GetSelectedMapType);
  end else begin
    SetActualMap(GetActiveMap.GetMapsList.GetMapTypeByGUID(AGUID));
  end;
end;

procedure TFillingMapMapsConfig.SetActualMap(AValue: IMapType);
begin
  LockWrite;
  try
    if FActualMap <> AValue then begin
      FActualMap := AValue;
      inherited SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
