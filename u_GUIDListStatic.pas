unit u_GUIDListStatic;

interface

uses
  i_GUIDListStatic;

type
  TGUIDListStatic =  class(TInterfacedObject, IGUIDListStatic)
  private
    FList: array of TGUID;
  protected
    function GetItem(AIndex: Integer): TGUID;
    function GetCount: Integer;
  public
    constructor Create(
      AList: array of TGUID;
      ACount: Integer
    );
    destructor Destroy; override;
  end;

implementation

{ TGUIDListStatic }

constructor TGUIDListStatic.Create(AList: array of TGUID; ACount: Integer);
var
  i: Integer;
begin
  SetLength(FList, ACount);
  for i := 0 to ACount - 1 do begin
    FList[i] := AList[i];
  end;
end;

destructor TGUIDListStatic.Destroy;
begin
  FList := nil;
  inherited;
end;

function TGUIDListStatic.GetCount: Integer;
begin
  Result := Length(FList);
end;

function TGUIDListStatic.GetItem(AIndex: Integer): TGUID;
begin
  Result := FList[AIndex];
end;

end.
