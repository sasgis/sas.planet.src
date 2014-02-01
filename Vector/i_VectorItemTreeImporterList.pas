unit i_VectorItemTreeImporterList;

interface

uses
  i_Changeable,
  i_VectorItemTreeImporter;

type
  IVectorItemTreeImporterListItem = interface
    ['{96787244-28A5-4D01-A00B-5F7057132BCB}']
    function GetImporter: IVectorItemTreeImporter;
    property Importer: IVectorItemTreeImporter read GetImporter;

    function GetDefaultExt: string;
    property DefaultExt: string read GetDefaultExt;

    function GetName: string;
    property Name: string read GetName;
  end;

  IVectorItemTreeImporterListStatic = interface
    ['{4D9CF72E-DCD2-48AD-890D-9B9627CF457F}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(const AIndex: Integer): IVectorItemTreeImporterListItem;
    property Items[const AIndex: Integer]: IVectorItemTreeImporterListItem read GetItem;
  end;

  IVectorItemTreeImporterListChangeable = interface(IChangeable)
    ['{6850E0F8-197F-452F-B46D-239DEA3B452C}']
    function GetStatic: IVectorItemTreeImporterListStatic;
  end;

implementation

end.
