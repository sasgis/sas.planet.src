unit u_MapTypeListStatic;

interface

uses
  i_MapTypes;

type
  TMapTypeListStatic = class(TInterfacedObject, IMapTypeListStatic)
  private
    FCount: Integer;
    FItems: array of IMapType;
  private
    function GetCount: Integer;
    function GetItem(AIndex: Integer): IMapType;
  public
    constructor Create(AItems: array of IMapType);
    destructor Destroy; override;
  end;

implementation

{ TMapTypeListStatic }

constructor TMapTypeListStatic.Create(AItems: array of IMapType);
var
  i: Integer;
begin
  FCount := Length(AItems);
  SetLength(FItems, FCount);
  for i := 0 to FCount - 1 do begin
    FItems[i] := AItems[i];
  end;
end;

destructor TMapTypeListStatic.Destroy;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do begin
    FItems[i] := nil;
  end;
  FItems := nil;
  FCount := 0;

  inherited;
end;

function TMapTypeListStatic.GetCount: Integer;
begin
  Result := FCount;
end;

function TMapTypeListStatic.GetItem(AIndex: Integer): IMapType;
begin
  Result := FItems[AIndex];
end;

end.
