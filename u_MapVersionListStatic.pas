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
  inherited Create;
  FList := AList;
end;

function TMapVersionListStatic.GetCount: Integer;
begin
  if not Assigned(FList) then begin
    Result := 0;
  end else begin
    Result := FList.Count;
  end;
end;

function TMapVersionListStatic.GetItem(AIndex: Integer): IMapVersionInfo;
begin
  if not Assigned(FList) then begin
    Result := nil;
  end else begin
    Result := IMapVersionInfo(FList.Items[AIndex]);
  end;
end;

end.
