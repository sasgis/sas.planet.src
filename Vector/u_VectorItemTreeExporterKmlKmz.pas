unit u_VectorItemTreeExporterKmlKmz;

interface

uses
  i_VectorItemTree,
  i_VectorItemTreeExporter,
  i_ArchiveReadWriteFactory,
  u_BaseInterfacedObject;

type
  TVectorItemTreeExporterKmlKmz = class(TBaseInterfacedObject, IVectorItemTreeExporter)
  private
    FArchiveReadWriteFactory: IArchiveReadWriteFactory;
  private
    procedure ProcessExport(
      const AFileName: string;
      const ATree: IVectorItemTree
    );
  public
    constructor Create(const AArchiveReadWriteFactory: IArchiveReadWriteFactory);
  end;

implementation

uses
  u_ExportMarks2KML;

{ TVectorItemTreeExporterKmlKmz }

constructor TVectorItemTreeExporterKmlKmz.Create(
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory
);
begin
  inherited Create;
  FArchiveReadWriteFactory := AArchiveReadWriteFactory;
end;

procedure TVectorItemTreeExporterKmlKmz.ProcessExport(
  const AFileName: string;
  const ATree: IVectorItemTree
);
var
  VExport: TExportMarks2KML;
begin
  VExport := TExportMarks2KML.Create(FArchiveReadWriteFactory);
  try
    VExport.ExportTreeToKML(ATree, AFileName);
  finally
    VExport.Free
  end;
end;

end.
