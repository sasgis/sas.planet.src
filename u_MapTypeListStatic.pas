unit u_MapTypeListStatic;

interface

uses
  i_HashFunction,
  i_InterfaceListStatic,
  i_InterfaceListSimple,
  i_MapTypes,
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
  u_InterfaceListSimple;

{ TMapTypeListStatic }

type
  TMapTypeListStatic = class(TBaseInterfacedObject, IMapTypeListStatic)
  private
    FItems: IInterfaceListStatic;
  private
    function GetCount: Integer;
    function GetItem(AIndex: Integer): IMapType;
  public
    constructor Create(const AItems: IInterfaceListStatic);
  end;

constructor TMapTypeListStatic.Create(const AItems: IInterfaceListStatic);
begin
  inherited Create;
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

function TMapTypeListStatic.GetItem(AIndex: Integer): IMapType;
begin
  Result := IMapType(FItems[AIndex]);
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
begin
  Result := TMapTypeListStatic.Create(FList.MakeStaticAndClear);
end;

function TMapTypeListBuilder.MakeCopy: IMapTypeListStatic;
begin
  Result := TMapTypeListStatic.Create(FList.MakeStaticCopy);
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
