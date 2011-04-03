unit u_MarkId;

interface

uses
  GR32,
  i_MarksSimple,
  i_MarksDbSmlInternal;

type
  TMarkId = class(TInterfacedObject, IMarkID, IMarkSMLInternal)
  private
    FName: string;
    FId: Integer;
    FCategoryId: Integer;
    FVisible: Boolean;
  protected
    function IsNew: Boolean;
  protected
    function GetName: string;
  protected
    function GetId: Integer;
    function GetCategoryId: Integer;
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
  public
    constructor Create(
      AName: string;
      AId: Integer;
      ACategoryId: Integer;
      AVisible: Boolean
    );
  end;

implementation

{ TMarkId }

constructor TMarkId.Create(
  AName: string;
  AId: Integer;
  ACategoryId: Integer;
  AVisible: Boolean
);
begin
  FName := AName;
  FId := AId;
  FCategoryId := ACategoryId;
  FVisible := AVisible;
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
