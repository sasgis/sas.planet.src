unit u_VectorItemSubsetBuilder;

interface

uses
  i_VectorDataItemSimple,
  i_VectorItemSubset,
  i_VectorItemSubsetBuilder,
  i_HashFunction,
  i_InterfaceListSimple,
  u_BaseInterfacedObject;

type
  TVectorItemSubsetBuilder = class(TBaseInterfacedObject, IVectorItemSubsetBuilder)
  private
    FHashFunction: IHashFunction;
    FList: IInterfaceListSimple;
  private
    procedure Clear;
    function Add(const AItem: IVectorDataItemSimple): Integer;

    function GetItem(AIndex: Integer): IVectorDataItemSimple;
    function GetCapacity: Integer;
    procedure SetCapacity(ANewCapacity: Integer);
    function GetCount: Integer;

    function MakeStaticAndClear: IVectorItemSubset;
    function MakeStaticCopy: IVectorItemSubset;
  public
    constructor Create(const AHashFunction: IHashFunction);
  end;

  TVectorItemSubsetBuilderFactory = class(TBaseInterfacedObject, IVectorItemSubsetBuilderFactory)
  private
    FHashFunction: IHashFunction;
  private
    function Build: IVectorItemSubsetBuilder;
  public
    constructor Create(const AHashFunction: IHashFunction);
  end;

implementation

uses
  u_InterfaceListSimple,
  u_VectorDataItemSubset;

{ TVectorItemSubsetBuilder }

constructor TVectorItemSubsetBuilder.Create(const AHashFunction: IHashFunction);
begin
  inherited Create;
  FHashFunction := AHashFunction;
end;

function TVectorItemSubsetBuilder.Add(
  const AItem: IVectorDataItemSimple): Integer;
begin
  Result := -1;
  Assert(Assigned(AItem));
  if Assigned(AItem) then begin
    if not Assigned(FList) then begin
      FList := TInterfaceListSimple.Create;
    end;
    Result := FList.Add(AItem);
  end;
end;

procedure TVectorItemSubsetBuilder.Clear;
begin
  if Assigned(FList) then begin
    FList.Clear;
  end;
end;

function TVectorItemSubsetBuilder.GetCapacity: Integer;
begin
  if Assigned(FList) then begin
    Result := FList.Capacity;
  end else begin
    Result := 0;
  end;
end;

function TVectorItemSubsetBuilder.GetCount: Integer;
begin
  if Assigned(FList) then begin
    Result := FList.Count;
  end else begin
    Result := 0;
  end;
end;

function TVectorItemSubsetBuilder.GetItem(
  AIndex: Integer
): IVectorDataItemSimple;
begin
  Result := IVectorDataItemSimple(FList[AIndex]);
end;

function TVectorItemSubsetBuilder.MakeStaticAndClear: IVectorItemSubset;
begin
  Result := nil;
  if Assigned(FList) then begin
    if FList.Count > 0 then begin
      Result := TVectorItemSubset.Create(FList.MakeStaticAndClear);
    end;
  end;
end;

function TVectorItemSubsetBuilder.MakeStaticCopy: IVectorItemSubset;
begin
  Result := nil;
  if Assigned(FList) then begin
    if FList.Count > 0 then begin
      Result := TVectorItemSubset.Create(FList.MakeStaticCopy);
    end;
  end;
end;

procedure TVectorItemSubsetBuilder.SetCapacity(ANewCapacity: Integer);
begin
  if (not Assigned(FList)) and (ANewCapacity > 0) then begin
    FList := TInterfaceListSimple.Create;
    FList.Capacity := ANewCapacity;
  end else if Assigned(FList) then begin
    FList.Capacity := ANewCapacity;
  end;
end;

{ TVectorItemSubsetBuilderFactory }

constructor TVectorItemSubsetBuilderFactory.Create(
  const AHashFunction: IHashFunction);
begin
  inherited Create;
  FHashFunction := AHashFunction;
end;

function TVectorItemSubsetBuilderFactory.Build: IVectorItemSubsetBuilder;
begin
  Result := TVectorItemSubsetBuilder.Create(FHashFunction);
end;

end.
