unit i_VectorItems;

interface

uses
  t_GeoTypes;

type
  ILonLatPathLine = interface
    ['{26634DF5-7845-42C1-8B74-C6F4FFA7E27E}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetPoints: PDoublePointArray;
    property Points: PDoublePointArray read GetPoints;
  end;

  ILonLatPolygonLine = interface
    ['{A1F32B46-8C0B-46F1-97E9-D2347CF9FF5B}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetPoints: PDoublePointArray;
    property Points: PDoublePointArray read GetPoints;
  end;


  ILonLatPath = interface
    ['{0E85CB46-D324-4052-BDE3-63F1C4A2665A}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(AIndex: Integer): ILonLatPathLine;
    property Item[AIndex: Integer]: ILonLatPathLine read GetItem;
  end;

  ILonLatPolygon = interface
    ['{04CEBFBE-8FC1-4AB0-8B39-3C283287BF46}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(AIndex: Integer): ILonLatPolygonLine;
    property Item[AIndex: Integer]: ILonLatPolygonLine read GetItem;
  end;

  IProjectedPathLine = interface
    ['{0D9B7321-DBA0-494F-959C-5026DB27C681}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetPoints: PDoublePointArray;
    property Points: PDoublePointArray read GetPoints;
  end;

  IProjectedPolygonLine = interface
    ['{30424113-D148-45EB-A4C8-C0150DB89D22}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetPoints: PDoublePointArray;
    property Points: PDoublePointArray read GetPoints;
  end;

  IProjectedPath = interface
    ['{781FAF61-C109-4CC9-A861-90CBE807D8E1}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(AIndex: Integer): IProjectedPathLine;
    property Item[AIndex: Integer]: IProjectedPathLine read GetItem;
  end;

  IProjectedPolygon = interface
    ['{02C310DE-60C3-4175-8811-367D5C5AC0CE}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(AIndex: Integer): IProjectedPolygonLine;
    property Item[AIndex: Integer]: IProjectedPolygonLine read GetItem;
  end;

implementation

end.
