unit i_VectorItemTreeExporterList;

interface

uses
  i_GUIDListStatic,
  i_Changeable,
  i_VectorItemTreeExporter;

type
  IVectorItemTreeExporterListItem = interface
    ['{96787244-28A5-4D01-A00B-5F7057132BCB}']
    function GetExporter: IVectorItemTreeExporter;
    property Exporter: IVectorItemTreeExporter read GetExporter;

    function GetDefaultExt: string;
    property DefaultExt: string read GetDefaultExt;

    function GetName: string;
    property Name: string read GetName;
  end;

  IVectorItemTreeExporterListStatic = interface
    ['{4D9CF72E-DCD2-48AD-890D-9B9627CF457F}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(const AIndex: Integer): IVectorItemTreeExporterListItem;
    property Items[const AIndex: Integer]: IVectorItemTreeExporterListItem read GetItem;
  end;

  IVectorItemTreeExporterListChangeable = interface(IChangeable)
    ['{6850E0F8-197F-452F-B46D-239DEA3B452C}']
    function GetStatic: IVectorItemTreeExporterListStatic;
  end;

implementation

end.
