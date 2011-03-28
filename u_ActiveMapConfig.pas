unit u_ActiveMapConfig;

interface

uses
  Windows,
  SysUtils,
  i_JclNotify,
  i_IGUIDList,
  i_MapTypes,
  i_ActiveMapsConfig,
  u_ConfigDataElementBase;

type
  TActiveMapConfig = class(TConfigDataElementBaseEmptySaveLoad, IActiveMap)
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
  u_NotifyWithGUIDEvent;

{ TActiveMapConfigNew }

constructor TActiveMapConfig.Create(AMainMapChangeNotyfier: IJclNotifier;
  ASingeMapsList: IGUIDInterfaceList; AMapsList: IMapTypeList);
var
  i: Cardinal;
begin
  inherited Create;
  FMapsList := AMapsList;
  FSingeMapsList := ASingeMapsList;
  FMainMapChangeNotyfier := AMainMapChangeNotyfier;
  FMainMapListener := TNotifyWithGUIDEventListener.Create(Self.OnMainMapChange);
  FMainMapChangeNotyfier.Add(FMainMapListener);
  if FMapsList.GetIterator.Next(1, FSelectedGUID, i) <> S_OK then begin
    raise Exception.Create('Empty maps list');
  end;
end;

destructor TActiveMapConfig.Destroy;
begin
  FMainMapChangeNotyfier.Remove(FMainMapListener);
  FMainMapListener := nil;
  FMainMapChangeNotyfier := nil;
  FMapsList := nil;
  FSingeMapsList := nil;
  inherited;
end;

function TActiveMapConfig.GetMapSingle(AMapGUID: TGUID): IActiveMapSingle;
begin
  if FMapsList.GetMapTypeByGUID(AMapGUID) <> nil then begin
    Result := IActiveMapSingle(FSingeMapsList.GetByGUID(AMapGUID));
  end;
end;

function TActiveMapConfig.GetMapsList: IMapTypeList;
begin
  Result := FMapsList;
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

procedure TActiveMapConfig.OnMainMapChange(AGUID: TGUID);
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
