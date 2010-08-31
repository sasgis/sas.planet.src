unit uMapTypeIconsList;

interface

uses
  Windows,
  ActiveX,
  Graphics,
  ImgList,
  i_IGUIDList,
  i_MapTypeIconsList;

type
  TMapTypeIconsList = class(TInterfacedObject, IMapTypeIconsList)
  private
    FList: IGUIDInterfaceList;
    FImageList: TCustomImageList;
    function GetImageList: TCustomImageList;
    function GetMapTypeByGUID(AGUID: TGUID): IMapTypeIconsListItem;
    function GetIterator: IEnumGUID;
  public
    procedure Add(AGUID: TGUID; ABmp: TBitmap);
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
    FIconIndex: Integer;
    function GetIconIndex: Integer;
  public
    constructor Create(AIconIndex: Integer);
  end;

constructor TMapTypeIconsListItem.Create(AIconIndex: Integer);
begin
  FIconIndex := AIconIndex;
end;

function TMapTypeIconsListItem.GetIconIndex: Integer;
begin
  Result := FIconIndex;
end;

{ TMapTypeIconsList }

procedure TMapTypeIconsList.Add(AGUID: TGUID; ABmp: TBitmap);
var
  VItem: IMapTypeIconsListItem;
  VIndex: Integer;
begin
  VItem := GetMapTypeByGUID(AGUID);
  if VItem = nil then begin
    VIndex := FImageList.AddMasked(Abmp, RGB(255,0,255));
    FList.Add(AGUID, VItem);
  end else begin
    VIndex := VItem.GetIconIndex;
    FImageList.ReplaceMasked(VIndex, ABmp, RGB(255,0,255));
  end;
end;

constructor TMapTypeIconsList.Create;
begin
  FImageList := TCustomImageList.Create(nil);
  FList := TGUIDInterfaceList.Create(False);
end;

destructor TMapTypeIconsList.Destroy;
begin
  FreeAndNil(FImageList);
  FList := nil;
  inherited;
end;

function TMapTypeIconsList.GetImageList: TCustomImageList;
begin
  Result := FImageList;
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
