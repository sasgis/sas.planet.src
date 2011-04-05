unit u_MarkId;

interface

uses
  GR32,
  i_MarksSimple,
  i_MarkCategory,
  i_MarksDbSmlInternal;

type
  TMarkId = class(TInterfacedObject, IMarkID, IMarkSMLInternal)
  private
    FName: string;
    FId: Integer;
    FCategory: IMarkCategory;
    FCategoryId: Integer;
    FVisible: Boolean;
  protected
    function IsNew: Boolean;
  protected
    function GetName: string;
  protected
    function GetId: Integer;
    function GetCategory: IMarkCategory;
    function GetCategoryId: Integer;
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
  public
    constructor Create(
      AName: string;
      AId: Integer;
      ACategory: IMarkCategory;
      AVisible: Boolean
    );
  end;

implementation

uses
  SysUtils;

{ TMarkId }

constructor TMarkId.Create(
  AName: string;
  AId: Integer;
  ACategory: IMarkCategory;
  AVisible: Boolean
);
var
  VCategory: IMarkCategorySMLInternal;
begin
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

function TMarkId.GetCategory: IMarkCategory;
begin
  Result := FCategory;
end;

function TMarkId.GetCategoryId: Integer;
begin
  Result := FCategoryId;
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

procedure TMarkId.SetVisible(AValue: Boolean);
begin
  FVisible := AValue;
end;

end.
