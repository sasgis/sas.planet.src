unit uMapTypeIconsList;

interface

uses
  Windows,
  ActiveX,
  ImgList,
  i_IGUIDList,
  i_MapTypeIconsList,
  UMapType;

type
  TMapTypeIconsList = class(TInterfacedObject, IMapTypeIconsList)
  private
    FList: IGUIDInterfaceList;
    FImageList18: TCustomImageList;
    FImageList24: TCustomImageList;
    function GetImageList18: TCustomImageList;
    function GetImageList24: TCustomImageList;
    function GetMapTypeByGUID(AGUID: TGUID): IMapTypeIconsListItem;
    function GetIterator: IEnumGUID;
  public
    procedure Add(AMap: TMapType);
    constructor Create();
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_GUIDInterfaceList;

{ TMapTypeIconsListItem }

type
  TMapTypeIconsListItem = class(TInterfacedObject, IMapTypeIconsListItem)
  private
    FIcon18Index: Integer;
    FIcon24Index: Integer;
    function GetIcon18Index: Integer;
    function GetIcon24Index: Integer;
  public
    constructor Create(AIcon18Index: Integer; AIcon24Index: Integer);
  end;

constructor TMapTypeIconsListItem.Create(AIcon18Index, AIcon24Index: Integer);
begin
  FIcon18Index := AIcon18Index;
  FIcon24Index := AIcon24Index;
end;

function TMapTypeIconsListItem.GetIcon18Index: Integer;
begin
  Result := FIcon18Index;
end;

function TMapTypeIconsListItem.GetIcon24Index: Integer;
begin
  Result := FIcon24Index;
end;

{ TMapTypeIconsList }

procedure TMapTypeIconsList.Add(AMap: TMapType);
var
  VItem: IMapTypeIconsListItem;
  VIndex18: Integer;
  VIndex24: Integer;
begin
  VItem := GetMapTypeByGUID(AMap.GUID);
  if VItem = nil then begin
    VIndex18 := FImageList18.AddMasked(AMap.bmp18, RGB(255,0,255));
    VIndex24 := FImageList24.AddMasked(AMap.bmp24, RGB(255,0,255));
    VItem := TMapTypeIconsListItem.Create(VIndex18, VIndex24);
    FList.Add(AMap.GUID, VItem);
  end else begin
    VIndex18 := VItem.GetIcon18Index;
    VIndex24 := VItem.GetIcon24Index;
    FImageList18.ReplaceMasked(VIndex18, AMap.bmp18, RGB(255,0,255));
    FImageList24.ReplaceMasked(VIndex24, AMap.bmp24, RGB(255,0,255));
  end;
end;

constructor TMapTypeIconsList.Create;
begin
  FImageList18 := TCustomImageList.Create(nil);
  FImageList24 := TCustomImageList.Create(nil);
  FList := TGUIDInterfaceList.Create(False);
end;

destructor TMapTypeIconsList.Destroy;
begin
  FreeAndNil(FImageList18);
  FreeAndNil(FImageList24);
  FList := nil;
  inherited;
end;

function TMapTypeIconsList.GetImageList18: TCustomImageList;
begin
  Result := FImageList18;
end;

function TMapTypeIconsList.GetImageList24: TCustomImageList;
begin
  Result := FImageList24;
end;

function TMapTypeIconsList.GetIterator: IEnumGUID;
begin
  Result := FList.GetGUIDEnum;
end;

function TMapTypeIconsList.GetMapTypeByGUID(
  AGUID: TGUID): IMapTypeIconsListItem;
begin
  Result := Flist.GetByGUID(AGUID) as IMapTypeIconsListItem;
end;

end.
