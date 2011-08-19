unit i_StaticTreeItem;

interface

type
  IStaticTreeItem = interface
    ['{2304E423-3893-4869-A140-904D93F2B3EF}']
    function GetData: IInterface;
    property Data: IInterface read GetData;

    function GetName: string;
    property Name: string read GetName;

    function GetDescription: string;
    property Description: string read GetDescription;

    function GetSubItemCount: Integer;
    property SubItemCount: Integer read GetSubItemCount;

    function GetSubItem(AIndex: Integer): IStaticTreeItem;
    property SubItem[AIndex: Integer]: IStaticTreeItem read GetSubItem;
  end;

implementation

end.
