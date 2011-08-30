unit u_MarkCategory;

interface

uses
  i_MarkCategory,
  i_MarksDbSmlInternal;

type
  TMarkCategory = class(TInterfacedObject, ICategory, IMarkCategory, IMarkCategorySMLInternal)
  private
    FDbCode: Integer;
    FId: Integer;
    FName: string;
    FVisible: Boolean;
    FAfterScale: integer;
    FBeforeScale: integer;
  protected
    function GetDbCode: Integer;
    function GetId: integer; stdcall;
  protected
    function GetName: string; stdcall;
    function IsSame(ACategory: ICategory): Boolean;
  protected
    function GetVisible: boolean; stdcall;
    function GetAfterScale: integer; stdcall;
    function GetBeforeScale: integer; stdcall;
    function IsNew: Boolean;
  public
    constructor Create(
      ADbCode: Integer;
      AId: Integer;
      AName: string;
      AVisible: Boolean;
      AAfterScale: integer;
      ABeforeScale: integer
    );
  end;

implementation

uses
  SysUtils;

{ TMarkCategory }

constructor TMarkCategory.Create(
  ADbCode: Integer;
  AId: Integer; AName: string; AVisible: Boolean;
  AAfterScale, ABeforeScale: integer);
begin
  FDbCode := ADbCode;
  FId := AId;
  FName := AName;
  FVisible := AVisible;
  FAfterScale := AAfterScale;
  FBeforeScale := ABeforeScale;
end;

function TMarkCategory.GetAfterScale: integer;
begin
  Result := FAfterScale;
end;

function TMarkCategory.GetBeforeScale: integer;
begin
  Result := FBeforeScale;
end;

function TMarkCategory.GetDbCode: Integer;
begin
  Result := FDbCode;
end;

function TMarkCategory.GetId: integer;
begin
  Result := FId;
end;

function TMarkCategory.GetName: string;
begin
  Result := FName;
end;

function TMarkCategory.GetVisible: boolean;
begin
  Result := FVisible;
end;

function TMarkCategory.IsNew: Boolean;
begin
  Result := FId < 0;
end;

function TMarkCategory.IsSame(ACategory: ICategory): Boolean;
var
  VCategoryInternal: IMarkCategorySMLInternal;
begin
  Result := False;
  if ACategory <> nil then begin
    if Supports(ACategory, IMarkCategorySMLInternal, VCategoryInternal) then begin
      Result := FId = VCategoryInternal.Id;
    end;
  end;
end;

end.
