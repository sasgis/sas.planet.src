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
    FName: string;
    FItems: IVectorItemSubset;
    FSubTreeItemList: IInterfaceList;
  private
    function GetName: string;
    function GetSubTreeItemCount: Integer;
    function GetSubTreeItem(const AIndex: Integer): IVectorItemTree;
    function GetItems: IVectorItemSubset;
  public
    constructor Create(
      const AName: string;
      const AItems: IVectorItemSubset;
      const ASubTreeItemList: IInterfaceList
    );
  end;

implementation

{ TVectorItemTree }

constructor TVectorItemTree.Create(
  const AName: string;
  const AItems: IVectorItemSubset;
  const ASubTreeItemList: IInterfaceList
);
begin
  inherited Create;
  FName := AName;
  FItems := AItems;
  FSubTreeItemList := ASubTreeItemList;
end;

function TVectorItemTree.GetName: string;
begin
  Result := FName;
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
