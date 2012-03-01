unit u_MapVersionListStatic;

interface

uses
  Classes,
  i_MapVersionInfo;

type
  TMapVersionListStatic = class(TInterfacedObject, IMapVersionListStatic)
  private
    FList: IInterfaceList;
  private
    function GetCount: Integer;
    function GetItem(AIndex: Integer): IMapVersionInfo;
  public
    constructor Create(
      AList: IInterfaceList
    );
  end;

implementation

{ TMapVersionListStatic }

constructor TMapVersionListStatic.Create(AList: IInterfaceList);
begin
  FList := AList;
end;

function TMapVersionListStatic.GetCount: Integer;
begin
  if not Assigned(FList) then
    Result := 0
  else
    Result := FList.Count;
end;

function TMapVersionListStatic.GetItem(AIndex: Integer): IMapVersionInfo;
begin
  if not Assigned(FList) then
    Result := nil
  else
    Result := IMapVersionInfo(FList.Items[AIndex]);
end;

end.
