unit u_MarksSimple;

interface

uses
  GR32,
  i_MarksSimple;

type
  TCategoryId = class
    id: integer;
    name: string;
    AfterScale: Integer;
    BeforeScale: Integer;
    visible: boolean;
  end;

type
  TMarkId = class(TInterfacedObject, IMarkID, IMarkVisible)
  private
    FName: string;
    FId: Integer;
    FVisible: Boolean;
  protected
    function GetId: Integer;
    function GetName: string;
  protected
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
  public
    constructor Create(
      AName: string;
      AId: Integer;
      AVisible: Boolean
    );
  end;

implementation

{ TMarkId }

constructor TMarkId.Create(AName: string; AId: Integer; AVisible: Boolean);
begin
  FName := AName;
  FId := AId;
  FVisible := AVisible;
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

procedure TMarkId.SetVisible(AValue: Boolean);
begin
  FVisible := AValue;
end;

end.
