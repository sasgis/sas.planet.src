unit u_MapTypeMenuItmesList;

interface

uses
  ActiveX,
  i_IGUIDList,
  i_IMapTypeMenuItem,
  i_IMapTypeMenuItmesList,
  UMapType;

type
  TMapTypeMenuItmesList = class(TInterfacedObject, IMapTypeMenuItmesList)
  private
    FList: IGUIDInterfaceList;
    function GetMapTypeItemByGUID(AGUID: TGUID): IMapTypeMenuItem;
    function GetIterator: IEnumGUID;
  public
    procedure Add(AMapItem: IMapTypeMenuItem);
    constructor Create;
    destructor Destroy; override;
  end;


implementation

uses
  u_GUIDInterfaceList;

{ TMapTypeMenuItmesList }

procedure TMapTypeMenuItmesList.Add(AMapItem: IMapTypeMenuItem);
begin
  FList.Add(AMapItem.GetMapType.GUID, AMapItem);
end;

constructor TMapTypeMenuItmesList.Create;
begin
  FList := TGUIDInterfaceList.Create(False);
end;

destructor TMapTypeMenuItmesList.Destroy;
begin
  FList := nil;
  inherited;
end;

function TMapTypeMenuItmesList.GetIterator: IEnumGUID;
begin
  Result := FList.GetGUIDEnum;
end;

function TMapTypeMenuItmesList.GetMapTypeItemByGUID(AGUID: TGUID): IMapTypeMenuItem;
begin
  Result := Flist.GetByGUID(AGUID) as IMapTypeMenuItem;
end;

end.
