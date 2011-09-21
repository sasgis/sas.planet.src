unit u_ActiveMapConfig;

interface

uses
  Windows,
  SysUtils,
  i_JclNotify,
  i_GUIDSet,
  i_MapTypes,
  i_ActiveMapsConfig,
  u_ConfigDataElementBase;

type
  TActiveMapConfig = class(TConfigDataElementBaseEmptySaveLoad, IActiveMap)
  private
    FSelectedGUID: TGUID;
    FMapsSet: IMapTypeSet;
    FSingeMapsList: IGUIDInterfaceSet;
  protected
    FMainMapChangeNotyfier: IJclNotifier;
    FMainMapListener: IJclListener;
    procedure OnMainMapChange(const AGUID: TGUID);
  protected
    function GetSelectedGUID: TGUID;
    function GetMapSingle(const AMapGUID: TGUID): IActiveMapSingle;
    function GetMapsSet: IMapTypeSet;
  public
    constructor Create(AMainMapChangeNotyfier: IJclNotifier; ASingeMapsList: IGUIDInterfaceSet; AMapsSet: IMapTypeSet);
    destructor Destroy; override;
  end;

implementation

uses
  ActiveX,
  u_NotifyWithGUIDEvent;

{ TActiveMapConfigNew }

constructor TActiveMapConfig.Create(AMainMapChangeNotyfier: IJclNotifier;
  ASingeMapsList: IGUIDInterfaceSet; AMapsSet: IMapTypeSet);
var
  i: Cardinal;
begin
  inherited Create;
  FMapsSet := AMapsSet;
  FSingeMapsList := ASingeMapsList;
  FMainMapChangeNotyfier := AMainMapChangeNotyfier;
  FMainMapListener := TNotifyWithGUIDEventListener.Create(Self.OnMainMapChange);
  FMainMapChangeNotyfier.Add(FMainMapListener);
  if FMapsSet.GetIterator.Next(1, FSelectedGUID, i) <> S_OK then begin
    raise Exception.Create('Empty maps list');
  end;
end;

destructor TActiveMapConfig.Destroy;
begin
  FMainMapChangeNotyfier.Remove(FMainMapListener);
  FMainMapListener := nil;
  FMainMapChangeNotyfier := nil;
  FMapsSet := nil;
  FSingeMapsList := nil;
  inherited;
end;

function TActiveMapConfig.GetMapSingle(const AMapGUID: TGUID): IActiveMapSingle;
begin
  if FMapsSet.GetMapTypeByGUID(AMapGUID) <> nil then begin
    Result := IActiveMapSingle(FSingeMapsList.GetByGUID(AMapGUID));
  end;
end;

function TActiveMapConfig.GetMapsSet: IMapTypeSet;
begin
  Result := FMapsSet;
end;

function TActiveMapConfig.GetSelectedGUID: TGUID;
begin
  LockRead;
  try
    Result := FSelectedGUID;
  finally
    UnlockRead;
  end;
end;

procedure TActiveMapConfig.OnMainMapChange(const AGUID: TGUID);
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
