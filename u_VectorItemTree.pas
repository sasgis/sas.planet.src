unit u_VectorItemTree;

interface

uses
  Classes,
  i_Category,
  i_VectorItemSubset,
  i_VectorItemTree,
  u_BaseInterfacedObject;

type
  TVectorItemTree = class(TBaseInterfacedObject, IVectorItemTree)
  private
    FCategory: ICategory;
    FItems: IVectorItemSubset;
    FSubTreeItemList: IInterfaceList;
  private
    function GetCategory: ICategory;
    function GetSubTreeItemCount: Integer;
    function GetSubTreeItem(const AIndex: Integer): IVectorItemTree;
    function GetItems: IVectorItemSubset;
  public
    constructor Create(
      const ACategory: ICategory;
      const AItems: IVectorItemSubset;
      const ASubTreeItemList: IInterfaceList
    );
  end;

implementation

{ TVectorItemTree }

constructor TVectorItemTree.Create(
  const ACategory: ICategory;
  const AItems: IVectorItemSubset;
  const ASubTreeItemList: IInterfaceList
);
begin
  Assert(Assigned(ACategory));
  inherited Create;
  FCategory := ACategory;
  FItems := AItems;
  FSubTreeItemList := ASubTreeItemList;
end;

function TVectorItemTree.GetCategory: ICategory;
begin
  Result := FCategory;
end;

function TVectorItemTree.GetItems: IVectorItemSubset;
begin
  Result := FItems;
end;

function TVectorItemTree.GetSubTreeItem(const AIndex: Integer): IVectorItemTree;
begin
  Result := nil;
  if Assigned(FSubTreeItemList) then begin
    if (AIndex >= 0) and (AIndex < FSubTreeItemList.Count) then begin
      Result := IVectorItemTree(FSubTreeItemList[AIndex]);
    end;
  end;
end;

function TVectorItemTree.GetSubTreeItemCount: Integer;
begin
  if Assigned(FSubTreeItemList) then begin
    Result := FSubTreeItemList.Count;
  end else begin
    Result := 0;
  end;

end;

end.
