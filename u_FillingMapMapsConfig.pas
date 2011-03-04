unit u_FillingMapMapsConfig;

interface

uses
  i_JclNotify,
  i_IActiveMapsConfig,
  i_MapTypes,
  i_IFillingMapLayerConfig,
  u_MainActiveMap;

type
  TFillingMapMapsConfig = class(TMainActiveMap, IFillingMapMapsConfig)
  private
    FActualMap: IMapType;
    FSelectedMapChangeListener: IJclListener;
    FMainMapsConfig: IMainMapsConfig;
    FMainMapChangeListener: IJclListener;
    procedure OnMainMapChange(Sender: TObject);
    procedure OnSelectedChange(AGUID: TGUID);
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
  inherited Create(FMainMapsConfig.GetAllActiveMapsSet.GetMapsList);

  FMainMapChangeListener := TNotifyEventListener.Create(Self.OnMainMapChange);
  FMainMapsConfig.GetActiveMap.GetChangeNotifier.Add(FMainMapChangeListener);

  FSelectedMapChangeListener := TNotifyWithGUIDEventListener.Create(Self.OnSelectedChange);
  MainMapChangeNotyfier.Add(FSelectedMapChangeListener);
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
    SetActualMap(
      FMainMapsConfig.GetActiveMap.GetMapsList.GetMapTypeByGUID(
        FMainMapsConfig.GetActiveMap.GetSelectedGUID
      )
    );
  end;
end;

procedure TFillingMapMapsConfig.OnSelectedChange(AGUID: TGUID);
begin
  if IsEqualGUID(AGUID, CGUID_Zero) then begin
    SetActualMap(
      FMainMapsConfig.GetActiveMap.GetMapsList.GetMapTypeByGUID(
        FMainMapsConfig.GetActiveMap.GetSelectedGUID
      )
    );
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
