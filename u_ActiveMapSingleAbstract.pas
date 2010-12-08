unit u_ActiveMapSingleAbstract;

interface

uses
  Windows,
  i_JclNotify,
  i_MapTypes,
  i_IConfigDataElement,
  i_IActiveMapsConfig,
  u_ConfigDataElementBase;

type
  TActiveMapSingleAbstract = class(TConfigDataElementBaseEmptySaveLoad, IActiveMapSingle)
  private
    FMapType: IMapType;
    FMapGUID: TGUID;
    FIsActive: Boolean;
  protected
    procedure SetIsActive(AValue: Boolean);
    property MapGUID: TGUID read FMapGUID;
  protected
    function GetMapType: IMapType;
    function GetIsActive: Boolean; virtual;
  public
    constructor Create(AMapType: IMapType);
    destructor Destroy; override;
  end;

type
  TActiveMapSingleMainMap = class(TActiveMapSingleAbstract)
  private
    FMainMapChangeNotyfier: IJclNotifier;
    FMainMapListener: IJclListener;
    procedure OnMainMapChange(AGUID: TGUID);
  public
    constructor Create(AMapType: IMapType; AMainMapChangeNotyfier: IJclNotifier);
    destructor Destroy; override;
  end;

type
  TActiveMapSingleLayer = class(TActiveMapSingleAbstract)
  private
    FLayerSetSelectNotyfier: IJclNotifier;
    FLayerSetUnselectNotyfier: IJclNotifier;
    FLayerSetSelectListener: IJclListener;
    FLayerSetUnselectListener: IJclListener;
    procedure OnLayerSetSelectChange(AGUID: TGUID);
    procedure OnLayerSetUnselectChange(AGUID: TGUID);
  public
    constructor Create(
      AMapType: IMapType;
      ALayerSetSelectNotyfier: IJclNotifier;
      ALayerSetUnselectNotyfier: IJclNotifier
    );
    destructor Destroy; override;
  end;



implementation

uses
  SysUtils,
  c_ZeroGUID,
  u_NotifyWithGUIDEvent;

{ TActiveMapSingleAbstract }

constructor TActiveMapSingleAbstract.Create(AMapType: IMapType);
begin
  FMapType := AMapType;
  if FMapType <> nil then begin
    FMapGUID := FMapType.MapType.GUID;
  end else begin
    FMapGUID := CGUID_Zero;
  end;
end;

destructor TActiveMapSingleAbstract.Destroy;
begin
  FMapType := nil;
  inherited;
end;

function TActiveMapSingleAbstract.GetIsActive: Boolean;
begin
  LockRead;
  try
    Result := FIsActive;
  finally
    UnlockRead;
  end;
end;

function TActiveMapSingleAbstract.GetMapType: IMapType;
begin
  Result := FMapType;
end;

procedure TActiveMapSingleAbstract.SetIsActive(AValue: Boolean);
begin
  LockWrite;
  try
    if AValue <> FIsActive then begin
      FIsActive := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

{ TActiveMapSingleMainMap }

constructor TActiveMapSingleMainMap.Create(AMapType: IMapType;
  AMainMapChangeNotyfier: IJclNotifier);
begin
  inherited Create(AMapType);
  FMainMapChangeNotyfier := AMainMapChangeNotyfier;
  FMainMapListener := TNotifyWithGUIDEventListener.Create(Self.OnMainMapChange);
  FMainMapChangeNotyfier.Add(FMainMapListener);
end;

destructor TActiveMapSingleMainMap.Destroy;
begin
  FMainMapChangeNotyfier.Remove(FMainMapListener);
  FMainMapListener := nil;
  FMainMapChangeNotyfier := nil;
  inherited;
end;

procedure TActiveMapSingleMainMap.OnMainMapChange(AGUID: TGUID);
begin
  SetIsActive(IsEqualGUID(FMapGUID, AGUID));
end;

{ TActiveMapSingleLayer }

constructor TActiveMapSingleLayer.Create(AMapType: IMapType;
  ALayerSetSelectNotyfier, ALayerSetUnselectNotyfier: IJclNotifier);
begin
  inherited Create(AMapType);
  FLayerSetSelectNotyfier := ALayerSetSelectNotyfier;
  FLayerSetSelectListener := TNotifyWithGUIDEventListener.Create(Self.OnLayerSetSelectChange);
  FLayerSetSelectNotyfier.Add(FLayerSetSelectListener);

  FLayerSetUnselectNotyfier := ALayerSetUnselectNotyfier;
  FLayerSetUnselectListener := TNotifyWithGUIDEventListener.Create(Self.OnLayerSetUnselectChange);
  FLayerSetUnselectNotyfier.Add(FLayerSetUnselectListener);
end;

destructor TActiveMapSingleLayer.Destroy;
begin
  FLayerSetSelectNotyfier.Remove(FLayerSetSelectListener);
  FLayerSetSelectListener := nil;
  FLayerSetSelectNotyfier := nil;

  FLayerSetUnselectNotyfier.Remove(FLayerSetUnselectListener);
  FLayerSetUnselectListener := nil;
  FLayerSetUnselectNotyfier := nil;

  inherited;
end;

procedure TActiveMapSingleLayer.OnLayerSetSelectChange(AGUID: TGUID);
begin
  if IsEqualGUID(MapGUID, AGUID) then begin
    SetIsActive(True);
  end;
end;

procedure TActiveMapSingleLayer.OnLayerSetUnselectChange(AGUID: TGUID);
begin
  if IsEqualGUID(MapGUID, AGUID) then begin
    SetIsActive(False);
  end;
end;

end.
