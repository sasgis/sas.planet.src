unit u_MapVersionListStatic;

interface

uses
  Classes,
  i_MapVersionInfo;

type
  TMapVersionListStatic = class(TInterfacedObject, IMapVersionListStatic)
  private
    FCount: Integer;
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
  if FList <>  nil then begin
    FCount := FList.Count;
  end else begin
    FCount := 0;
  end;
end;

function TMapVersionListStatic.GetCount: Integer;
begin
  Result := FCount;
end;

function TMapVersionListStatic.GetItem(AIndex: Integer): IMapVersionInfo;
begin
  if FCount > 0 then begin
    Result := IMapVersionInfo(FList.Items[AIndex]);
  end;
end;

end.
