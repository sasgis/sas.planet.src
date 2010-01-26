unit u_MarkCategory;

interface

uses
  i_Marks;

type
  TMarkCategory = class(TInterfacedObject, IMarkCategory)
  public
    FId: Integer;
    FName: string;
    FVisible: Boolean;
    FAfterScale: integer;
    FBeforeScale: integer;

    constructor Create(ACategory: IMarkCategory);
    function GetId: integer; stdcall;
    function GetName: string; stdcall;
    function GetVisible: boolean; stdcall;
    function GetAfterScale: integer; stdcall;
    function GetBeforeScale: integer; stdcall;

    property Id: integer read GetId;
    property Name: string read GetName;
    property Visible: boolean read GetVisible;
    property AfterScale: integer read GetAfterScale;
    property BeforeScale: integer read GetBeforeScale;
  end;
implementation

{ TMarkCategory }

constructor TMarkCategory.Create(ACategory: IMarkCategory);
begin
  FId := ACategory.Id;
  FName := ACategory.Name;
  FVisible := ACategory.Visible;
  FAfterScale := ACategory.AfterScale;
  FBeforeScale := ACategory.BeforeScale;
end;

function TMarkCategory.GetAfterScale: integer;
begin
  Result := FAfterScale;
end;

function TMarkCategory.GetBeforeScale: integer;
begin
  Result := FBeforeScale;
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

end.
