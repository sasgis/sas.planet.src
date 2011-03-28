unit u_GeoCoderListBase;

interface

uses
  SyncObjs,
  ActiveX,
  i_JclNotify,
  i_GeoCoderList,
  i_GUIDList;

type
  TGeoCoderListBase = class(TInterfacedObject, IGeoCoderList)
  private
    FList: IGUIDInterfaceList;
    FCS: TCriticalSection;
    FAddNotifier: IJclNotifier;
  protected
    procedure Add(AItem: IGeoCoderListEntity);
  protected
    function GetGUIDEnum: IEnumGUID;
    function Get(AGUID: TGUID): IGeoCoderListEntity;
    function GetAddNotifier: IJclNotifier;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_JclNotify,
  u_GUIDInterfaceList;

{ TGeoCoderListBase }

constructor TGeoCoderListBase.Create;
begin
  FCS := TCriticalSection.Create;
  FList := TGUIDInterfaceList.Create(False);
  FAddNotifier := TJclBaseNotifier.Create;
end;

destructor TGeoCoderListBase.Destroy;
begin
  FreeAndNil(FCS);
  FList := nil;
  inherited;
end;

procedure TGeoCoderListBase.Add(AItem: IGeoCoderListEntity);
begin
  FCS.Acquire;
  try
    FList.Add(AItem.GetGUID, AItem);
  finally
    FCS.Release;
  end;
  FAddNotifier.Notify(nil);
end;

function TGeoCoderListBase.Get(AGUID: TGUID): IGeoCoderListEntity;
begin
  FCS.Acquire;
  try
    Result := IGeoCoderListEntity(FList.GetByGUID(AGUID));
  finally
    FCS.Release;
  end;
end;

function TGeoCoderListBase.GetAddNotifier: IJclNotifier;
begin
  Result := FAddNotifier;
end;

function TGeoCoderListBase.GetGUIDEnum: IEnumGUID;
begin
  FCS.Acquire;
  try
    Result := FList.GetGUIDEnum;
  finally
    FCS.Release;
  end;
end;

end.
