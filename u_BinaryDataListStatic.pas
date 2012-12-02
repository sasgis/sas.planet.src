unit u_BinaryDataListStatic;

interface

uses
  i_BinaryData,
  i_BinaryDataListStatic,
  u_BaseInterfacedObject;

type
  TBinaryDataListStatic = class(TBaseInterfacedObject, IBinaryDataListStatic)
  private
    FCount: Integer;
    FItems: array of IBinaryData;
  private
    function GetCount: Integer;
    function GetItem(AIndex: Integer): IBinaryData;
  public
    constructor Create(const AItems: array of IBinaryData);
    constructor CreateBySource(const ASource: IBinaryDataListStatic; const AItem: IBinaryData);
    constructor CreateByTwoItems(const AItem1, AItem2: IBinaryData);
    destructor Destroy; override;
  end;

implementation

{ TBinaryDataListStatic }

constructor TBinaryDataListStatic.Create(const AItems: array of IBinaryData);
var
  VCount: Integer;
  i: Integer;
begin
  inherited Create;
  VCount := Length(AItems);
  FCount := VCount;
  SetLength(FItems, VCount);
  for i := 0 to VCount - 1 do begin
    FItems[i] := AItems[i];
  end;
end;

constructor TBinaryDataListStatic.CreateBySource(
  const ASource: IBinaryDataListStatic; const AItem: IBinaryData);
var
  VCount: Integer;
  i: Integer;
begin
  inherited Create;
  VCount := ASource.Count;
  FCount := VCount + 1;
  SetLength(FItems, FCount);
  for i := 0 to VCount - 1 do begin
    FItems[i] := ASource.Item[i];
  end;
  FItems[VCount] := AItem;
end;

constructor TBinaryDataListStatic.CreateByTwoItems(const AItem1,
  AItem2: IBinaryData);
begin
  inherited Create;
  FCount := 2;
  SetLength(FItems, 2);
  FItems[0] := AItem1;
  FItems[1] := AItem2;
end;

destructor TBinaryDataListStatic.Destroy;
var
  i: Integer;
begin
  for i := 0 to Length(FItems) - 1 do begin
    FItems[i] := nil;
  end;
  FItems := nil;

  inherited;
end;

function TBinaryDataListStatic.GetCount: Integer;
begin
  Result := FCount;
end;

function TBinaryDataListStatic.GetItem(AIndex: Integer): IBinaryData;
begin
  Result := FItems[AIndex];
end;

end.
