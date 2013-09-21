unit u_MapVersionListStatic;

interface

uses
  i_InterfaceListStatic,
  i_MapVersionInfo,
  u_BaseInterfacedObject;

type
  TMapVersionListStatic = class(TBaseInterfacedObject, IMapVersionListStatic)
  private
    FList: IInterfaceListStatic;
    FSorted: Boolean;
  private
    function GetCount: Integer;
    function GetItem(AIndex: Integer): IMapVersionInfo;
    function GetSorted: Boolean;
  public
    constructor Create(
      const AList: IInterfaceListStatic;
      const ASorted: Boolean = False
    );
  end;

implementation

{ TMapVersionListStatic }

constructor TMapVersionListStatic.Create(
  const AList: IInterfaceListStatic;
  const ASorted: Boolean
);
begin
  inherited Create;
  FList := AList;
  FSorted := ASorted;
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

function TMapVersionListStatic.GetSorted: Boolean;
begin
  Result := FSorted;
end;

end.
