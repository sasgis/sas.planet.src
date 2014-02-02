unit u_VectorItemTreeExporterList;

interface

uses
  i_InterfaceListStatic,
  i_VectorItemTreeExporter,
  i_VectorItemTreeExporterList,
  u_BaseInterfacedObject;

type
  TVectorItemTreeExporterListItem = class(TBaseInterfacedObject, IVectorItemTreeExporterListItem)
  private
    FExporter: IVectorItemTreeExporter;
    FDefaultExt: string;
    FName: string;
  private
    function GetExporter: IVectorItemTreeExporter;
    function GetDefaultExt: string;
    function GetName: string;
  public
    constructor Create(
      const AExporter: IVectorItemTreeExporter;
      const ADefaultExt: string;
      const AName: string
    );
  end;

  TVectorItemTreeExporterListStatic = class(TBaseInterfacedObject, IVectorItemTreeExporterListStatic)
  private
    FList: IInterfaceListStatic;
  private
    function GetCount: Integer;
    function GetItem(const AIndex: Integer): IVectorItemTreeExporterListItem;
  public
    constructor Create(const AList: IInterfaceListStatic);
  end;

implementation

{ TVectorItemTreeExporterListItem }

constructor TVectorItemTreeExporterListItem.Create(
  const AExporter: IVectorItemTreeExporter;
  const ADefaultExt, AName: string
);
begin
  Assert(Assigned(AExporter));
  Assert(ADefaultExt <> '');
  Assert(AName <> '');
  inherited Create;
  FExporter := AExporter;
  FDefaultExt := ADefaultExt;
  FName := AName;
end;

function TVectorItemTreeExporterListItem.GetDefaultExt: string;
begin
  Result := FDefaultExt;
end;

function TVectorItemTreeExporterListItem.GetExporter: IVectorItemTreeExporter;
begin
  Result := FExporter;
end;

function TVectorItemTreeExporterListItem.GetName: string;
begin
  Result := FName;
end;

{ TVectorItemTreeExporterListStatic }

constructor TVectorItemTreeExporterListStatic.Create(
  const AList: IInterfaceListStatic
);
begin
  inherited Create;
  FList := AList;
end;

function TVectorItemTreeExporterListStatic.GetCount: Integer;
begin
  if Assigned(FList) then begin
    Result := FList.Count;
  end else begin
    Result := 0;
  end;
end;

function TVectorItemTreeExporterListStatic.GetItem(
  const AIndex: Integer
): IVectorItemTreeExporterListItem;
begin
  Result := IVectorItemTreeExporterListItem(FList.Items[AIndex]);
end;

end.
