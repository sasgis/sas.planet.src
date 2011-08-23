unit u_TreeByPathDetalizeProviderList;

interface

uses
  Classes,
  u_TreeChangeableBase,
  i_PathDetalizeProviderList;

type
  TTreeByPathDetalizeProviderList = class(TTreeChangeableBase)
  private
    FProviderList: IPathDetalizeProviderList;
  protected
    procedure ProcessItems(AList: TStringList); override;
    function GetNameFromItem(AItem: IInterface): string; override;
    function GetLevelName(const AName: string; out ACurLevelName, ATrailName: string): Boolean; override;
    procedure GetGroupAndVisibleName(const AName: string; out AGroupName, AVisibleName: string); override;
  public
    constructor Create(AProviderList: IPathDetalizeProviderList);
  end;


implementation

uses
  SysUtils,
  ActiveX;

{ TTreeByPathDetalizeProviderList }

constructor TTreeByPathDetalizeProviderList.Create(
  AProviderList: IPathDetalizeProviderList);
begin
  FProviderList := AProviderList;
  inherited Create(FProviderList.GetChangeNotifier);
end;

procedure TTreeByPathDetalizeProviderList.ProcessItems(AList: TStringList);
var
  VList: IPathDetalizeProviderList;
  VEnum: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
  VItem: IPathDetalizeProviderListEntity;
begin
  VList := FProviderList;
  VEnum := VList.GetGUIDEnum;
  while VEnum.Next(1, VGUID, i) = S_OK do begin
    VItem := VList.Get(VGUID);
    ProcessItem(VItem, AList);
  end;
end;

function TTreeByPathDetalizeProviderList.GetLevelName(
  const AName: string;
  out ACurLevelName, ATrailName: string
): Boolean;
var
  VPos: Integer;
begin
  VPos := Pos('\', AName);
  if VPos > 0 then begin
    ACurLevelName := Copy(AName, 1, VPos - 1);
    ATrailName := Copy(AName, VPos + 1, Length(AName));
    Result := True;
  end else begin
    ACurLevelName := AName;
    ATrailName := '';
    Result := False;
  end;
end;

function TTreeByPathDetalizeProviderList.GetNameFromItem(
  AItem: IInterface): string;
begin
  Result := (AItem as IPathDetalizeProviderListEntity).MenuItemName;
end;

procedure TTreeByPathDetalizeProviderList.GetGroupAndVisibleName(
  const AName: string;
  out AGroupName, AVisibleName: string
);
var
  VPos: Integer;
begin
  VPos := Pos('|', AName);
  if VPos > 0 then begin
    AVisibleName := Copy(AName, 1, VPos - 1);
    AGroupName := Copy(AName, VPos + 1, Length(AName));
  end else begin
    AVisibleName := AName;
    AGroupName := AName;
  end;
end;

end.
