unit i_VectorItemTree;

interface

uses
  i_Category,
  i_VectorItemSubset;

type
  IVectorItemTree = interface
    function GetName: string;
    property Name: string read GetName;

    function GetSubTreeItemCount: Integer;
    property SubTreeItemCount: Integer read GetSubTreeItemCount;

    function GetSubTreeItem(const AIndex: Integer): IVectorItemTree;

    function GetItems: IVectorItemSubset;
    property Items: IVectorItemSubset read GetItems;
  end;

implementation

end.
