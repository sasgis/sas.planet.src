unit i_VectorItemLocal;

interface

uses
  t_GeoTypes,
  i_EnumDoublePoint,
  i_LocalCoordConverter;

type
  ILocalPathLine = interface
    ['{2DB59206-AD9C-47FD-B1CF-329579BEE20B}']
    function GetEnum: IEnumLocalPoint;

    function GetLocalConverter: ILocalCoordConverter;
    property LocalConverter: ILocalCoordConverter read GetLocalConverter;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetPoints: PDoublePointArray;
    property Points: PDoublePointArray read GetPoints;
  end;

  ILocalPolygonLine = interface
    ['{0716D252-1516-440B-AD80-826C60AAC063}']
    function GetEnum: IEnumLocalPoint;

    function GetLocalConverter: ILocalCoordConverter;
    property LocalConverter: ILocalCoordConverter read GetLocalConverter;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetPoints: PDoublePointArray;
    property Points: PDoublePointArray read GetPoints;
  end;

  ILocalPath = interface
    ['{C5B0DB77-DC25-4802-BB90-F0FE90DC1DFC}']
    function GetEnum: IEnumLocalPoint;

    function GetLocalConverter: ILocalCoordConverter;
    property LocalConverter: ILocalCoordConverter read GetLocalConverter;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(AIndex: Integer): ILocalPathLine;
    property Item[AIndex: Integer]: ILocalPathLine read GetItem;
  end;

  ILocalPolygon = interface
    ['{86702869-BE39-41C8-8373-A7C19E20ED7B}']
    function GetEnum: IEnumLocalPoint;

    function GetLocalConverter: ILocalCoordConverter;
    property LocalConverter: ILocalCoordConverter read GetLocalConverter;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(AIndex: Integer): ILocalPolygonLine;
    property Item[AIndex: Integer]: ILocalPolygonLine read GetItem;
  end;

implementation

end.
