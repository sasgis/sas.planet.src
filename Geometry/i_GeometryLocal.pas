unit i_GeometryLocal;

interface

uses
  t_GeoTypes,
  i_EnumDoublePoint,
  i_LocalCoordConverter;

type
  IGeometryLocal = interface
    ['{C9B8B666-3B15-4A97-BA95-172D53916742}']
    function GetLocalConverter: ILocalCoordConverter;
    property LocalConverter: ILocalCoordConverter read GetLocalConverter;
  end;

  IGeometryLocalLine = interface(IGeometryLocal)
    ['{2DB59206-AD9C-47FD-B1CF-329579BEE20B}']
    function GetEnum: IEnumLocalPoint;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetPoints: PDoublePointArray;
    property Points: PDoublePointArray read GetPoints;
  end;

  IGeometryLocalPolygon = interface(IGeometryLocal)
    ['{0716D252-1516-440B-AD80-826C60AAC063}']
    function GetEnum: IEnumLocalPoint;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetPoints: PDoublePointArray;
    property Points: PDoublePointArray read GetPoints;
  end;

  IGeometryLocalMultiLine = interface(IGeometryLocal)
    ['{C5B0DB77-DC25-4802-BB90-F0FE90DC1DFC}']
    function GetEnum: IEnumLocalPoint;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(AIndex: Integer): IGeometryLocalLine;
    property Item[AIndex: Integer]: IGeometryLocalLine read GetItem;
  end;

  IGeometryLocalMultiPolygon = interface(IGeometryLocal)
    ['{86702869-BE39-41C8-8373-A7C19E20ED7B}']
    function GetEnum: IEnumLocalPoint;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(AIndex: Integer): IGeometryLocalPolygon;
    property Item[AIndex: Integer]: IGeometryLocalPolygon read GetItem;
  end;

implementation

end.
