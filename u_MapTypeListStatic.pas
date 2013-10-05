unit u_MapTypeListStatic;

interface

uses
  i_HashFunction,
  i_MapTypeListBuilder,
  u_BaseInterfacedObject;

type
  TMapTypeListBuilderFactory = class(TBaseInterfacedObject, IMapTypeListBuilderFactory)
  private
    FHashFunction: IHashFunction;
  private
    function Build: IMapTypeListBuilder;
  public
    constructor Create(const AHashFunction: IHashFunction);
  end;

implementation

uses
  t_Hash,
  i_InterfaceListStatic,
  i_InterfaceListSimple,
  i_MapTypes,
  i_MapTypeListStatic,
  u_InterfaceListSimple;

const
  CInitialHash : THashValue = $c74bb7e2dee15036;

{ TMapTypeListStatic }

type
  TMapTypeListStatic = class(TBaseInterfacedObject, IMapTypeListStatic)
  private
    FHash: THashValue;
    FItems: IInterfaceListStatic;
  private
    function GetHash: THashValue;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): IMapType;
    function IsEqual(const AValue: IMapTypeListStatic): Boolean;
  public
    constructor Create(const AHash: THashValue; const AItems: IInterfaceListStatic);
  end;

constructor TMapTypeListStatic.Create(
  const AHash: THashValue;
  const AItems: IInterfaceListStatic
);
begin
  inherited Create;
  FHash := AHash;
  FItems := AItems;
end;

function TMapTypeListStatic.GetCount: Integer;
begin
  if Assigned(FItems) then begin
    Result := FItems.Count;
  end else begin
    Result := 0;
  end;
end;

function TMapTypeListStatic.GetHash: THashValue;
begin
  Result := FHash;
end;

function TMapTypeListStatic.GetItem(AIndex: Integer): IMapType;
begin
  Result := IMapType(FItems[AIndex]);
end;

function TMapTypeListStatic.IsEqual(const AValue: IMapTypeListStatic): Boolean;
var
  i: Integer;
begin
  if AValue = nil then begin
    Result := False;
    Exit;
  end;
  if AValue = IMapTypeListStatic(Self) then begin
    Result := True;
    Exit;
  end;
  if (FHash <> 0) and (AValue.Hash <> 0) and (FHash <> AValue.Hash) then begin
    Result := False;
    Exit;
  end;

  if AValue.GetCount <> GetCount then begin
    Result := False;
    Exit;
  end;
  for i := 0 to GetCount - 1 do begin
    if AValue.Items[i] <> GetItem(i) then begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

{ TMapTypeListBuilder }

type
  TMapTypeListBuilder = class(TBaseInterfacedObject, IMapTypeListBuilder)
  private
    FHashFunction: IHashFunction;
    FList: IInterfaceListSimple;
  private
    function GetCount: Integer;
    function GetCapacity: Integer;
    procedure SetCapacity(ANewCapacity: Integer);

    function GetItem(AIndex: Integer): IMapType;
    procedure SetItem(AIndex: Integer; const AItem: IMapType);

    procedure Add(const AItem: IMapType);
    procedure Clear;
    procedure Delete(AIndex: Integer);
    procedure Exchange(AIndex1, AIndex2: Integer);
    function MakeCopy: IMapTypeListStatic;
    function MakeAndClear: IMapTypeListStatic;
  public
    constructor Create(
      const AHashFunction: IHashFunction
    );
  end;

constructor TMapTypeListBuilder.Create(const AHashFunction: IHashFunction);
begin
  inherited Create;
  FHashFunction := AHashFunction;
  FList := TInterfaceListSimple.Create;
end;

procedure TMapTypeListBuilder.Add(const AItem: IMapType);
begin
  FList.Add(AItem);
end;

procedure TMapTypeListBuilder.Clear;
begin
  FList.Clear;
end;

procedure TMapTypeListBuilder.Delete(AIndex: Integer);
begin
  FList.Delete(AIndex);
end;

procedure TMapTypeListBuilder.Exchange(AIndex1, AIndex2: Integer);
begin
  FList.Exchange(AIndex1, AIndex2);
end;

function TMapTypeListBuilder.GetCapacity: Integer;
begin
  Result := FList.Capacity;
end;

function TMapTypeListBuilder.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TMapTypeListBuilder.GetItem(AIndex: Integer): IMapType;
begin
  Result := IMapType(FList[AIndex]);
end;

function TMapTypeListBuilder.MakeAndClear: IMapTypeListStatic;
var
  VHash: THashValue;
  i: Integer;
begin
  VHash := CInitialHash;
  for i := 0 to FList.Count - 1 do begin
    FHashFunction.UpdateHashByGUID(VHash, GetItem(i).GUID);
  end;
  Result := TMapTypeListStatic.Create(VHash, FList.MakeStaticAndClear);
end;

function TMapTypeListBuilder.MakeCopy: IMapTypeListStatic;
var
  VHash: THashValue;
  i: Integer;
begin
  VHash := CInitialHash;
  for i := 0 to FList.Count - 1 do begin
    FHashFunction.UpdateHashByGUID(VHash, GetItem(i).GUID);
  end;
  Result := TMapTypeListStatic.Create(VHash, FList.MakeStaticCopy);
end;

procedure TMapTypeListBuilder.SetCapacity(ANewCapacity: Integer);
begin
  FList.Capacity := ANewCapacity;
end;

procedure TMapTypeListBuilder.SetItem(AIndex: Integer; const AItem: IMapType);
begin
  FList[AIndex] := AItem;
end;

{ TMapTypeListBuilderFactory }

function TMapTypeListBuilderFactory.Build: IMapTypeListBuilder;
begin
  Result := TMapTypeListBuilder.Create(FHashFunction);
end;

constructor TMapTypeListBuilderFactory.Create(
  const AHashFunction: IHashFunction
);
begin
  inherited Create;
  FHashFunction := AHashFunction;
end;

end.
