unit u_MarkId;

interface

uses
  i_MarksSimple,
  i_MarkCategory,
  i_MarksDbSmlInternal;

type
  TMarkId = class(TInterfacedObject, IMarkID, IMarkSMLInternal)
  private
    FDbCode: Integer;
    FName: string;
    FId: Integer;
    FCategory: ICategory;
    FCategoryId: Integer;
    FVisible: Boolean;
  protected
    function IsNew: Boolean;
  protected
    function GetName: string;
  protected
    function GetDbCode: Integer;
    function GetId: Integer;
    function GetCategory: ICategory;
    function GetCategoryId: Integer;
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    function IsSameId(AMarkId: IMarkID): Boolean;
  public
    constructor Create(
      ADbCode: Integer;
      AName: string;
      AId: Integer;
      ACategory: ICategory;
      AVisible: Boolean
    );
  end;

implementation

uses
  SysUtils;

{ TMarkId }

constructor TMarkId.Create(
  ADbCode: Integer;
  AName: string;
  AId: Integer;
  ACategory: ICategory;
  AVisible: Boolean
);
var
  VCategory: IMarkCategorySMLInternal;
begin
  FDbCode := ADbCode;
  FName := AName;
  FId := AId;
  FCategory := ACategory;
  FCategoryId := -1;
  if FCategory <> nil then begin
    if Supports(FCategory, IMarkCategorySMLInternal, VCategory) then begin
      FCategoryId := VCategory.Id;
    end;
  end;
  FVisible := AVisible;
end;

function TMarkId.GetCategory: ICategory;
begin
  Result := FCategory;
end;

function TMarkId.GetCategoryId: Integer;
begin
  Result := FCategoryId;
end;

function TMarkId.GetDbCode: Integer;
begin
  Result := FDbCode;
end;

function TMarkId.GetId: Integer;
begin
  Result := FId;
end;

function TMarkId.GetName: string;
begin
  Result := FName;
end;

function TMarkId.GetVisible: Boolean;
begin
  Result := FVisible;
end;

function TMarkId.IsNew: Boolean;
begin
  Result := FId < 0;
end;

function TMarkId.IsSameId(AMarkId: IMarkID): Boolean;
var
  VMarkInternal: IMarkSMLInternal;
begin
  Result := False;
  if AMarkId <> nil then begin
    if Supports(AMarkId, IMarkSMLInternal, VMarkInternal) then begin
      Result := FId = VMarkInternal.Id;
    end;
  end;
end;

procedure TMarkId.SetVisible(AValue: Boolean);
begin
  FVisible := AValue;
end;

end.
