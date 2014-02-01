unit i_VectorItemTreeExporter;

interface

uses
  i_VectorItemTree;

type
  IVectorItemTreeExporter = interface
    ['{B7998E79-6C10-44A4-B17C-E5DF8A3D9525}']
    procedure ProcessExport(
      const AFileName: string;
      const ATree: IVectorItemTree
    );
  end;

implementation

end.
