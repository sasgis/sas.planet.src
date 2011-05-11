unit u_VectorDataItemList;

interface

uses
  Classes,
  i_VectorDataItemSimple;

type
  TVectorDataItemList = class(TInterfacedObject, IVectorDataItemList)
  private
    FList: TInterfaceList;
  protected
    function GetCount: Integer;
    function GetItem(AIndex: Integer): IVectorDataItemSimple;
  public
    constructor Create(AList: TInterfaceList);
  end;

implementation

{ TVectorDataItemList }

constructor TVectorDataItemList.Create(AList: TInterfaceList);
begin
  FList := AList;
end;

function TVectorDataItemList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TVectorDataItemList.GetItem(AIndex: Integer): IVectorDataItemSimple;
begin
  Result := IVectorDataItemSimple(FList.Items[AIndex]);
end;

end.
