unit u_VectorItemTreeExporterListSimple;

interface

uses
  i_Notifier,
  i_ArchiveReadWriteFactory,
  i_VectorItemTreeExporter,
  i_VectorItemTreeExporterList,
  u_BaseInterfacedObject;

type
  TVectorItemTreeExporterListSimple = class(TBaseInterfacedObject, IVectorItemTreeExporterListChangeable)
  private
    FNotifierFake: INotifier;
    FList: IVectorItemTreeExporterListStatic;
  private
    function GetBeforeChangeNotifier: INotifier;
    function GetChangeNotifier: INotifier;
    function GetAfterChangeNotifier: INotifier;
    function GetStatic: IVectorItemTreeExporterListStatic;
  public
    constructor Create(const AArchiveReadWriteFactory: IArchiveReadWriteFactory);
  end;

implementation

uses
  i_InterfaceListSimple,
  u_Notifier,
  u_InterfaceListSimple,
  u_VectorItemTreeExporterList,
  u_VectorItemTreeExporterKmlKmz;

{ TVectorItemTreeExporterListSimple }

constructor TVectorItemTreeExporterListSimple.Create(
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory
);
var
  VList: IInterfaceListSimple;
  VExporter: IVectorItemTreeExporter;
  VItem: IVectorItemTreeExporterListItem;
begin
  inherited Create;
  FNotifierFake := TNotifierFaked.Create;
  VList := TInterfaceListSimple.Create;
  VExporter := TVectorItemTreeExporterKmlKmz.Create(AArchiveReadWriteFactory);
  VItem :=
    TVectorItemTreeExporterListItem.Create(
      VExporter,
      'kmz',
      'Compressed Keyhole Markup Language (kmz)'
    );
  VList.Add(VItem);
  VItem :=
    TVectorItemTreeExporterListItem.Create(
      VExporter,
      'kml',
      'Keyhole Markup Language (kml)'
    );
  VList.Add(VItem);
  FList := TVectorItemTreeExporterListStatic.Create(VList.MakeStaticAndClear);
end;

function TVectorItemTreeExporterListSimple.GetAfterChangeNotifier: INotifier;
begin
  Result := FNotifierFake;
end;

function TVectorItemTreeExporterListSimple.GetBeforeChangeNotifier: INotifier;
begin
  Result := FNotifierFake;
end;

function TVectorItemTreeExporterListSimple.GetChangeNotifier: INotifier;
begin
  Result := FNotifierFake;
end;

function TVectorItemTreeExporterListSimple.GetStatic: IVectorItemTreeExporterListStatic;
begin
  Result := FList;
end;

end.
