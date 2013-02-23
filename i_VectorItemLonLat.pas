unit i_VectorItemLonLat;

interface

uses
  t_GeoTypes,
  i_NotifierOperation,
  i_EnumDoublePoint,
  i_LonLatRect,
  i_Datum;

type
  ILonLatPathLine = interface
    ['{26634DF5-7845-42C1-8B74-C6F4FFA7E27E}']
    function GetEnum: IEnumLonLatPoint;
    function IsSame(const ALine: ILonLatPathLine): Boolean;

    function GetBounds: ILonLatRect;
    property Bounds: ILonLatRect read GetBounds;

    function CalcLength(const ADatum: IDatum): Double;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetPoints: PDoublePointArray;
    property Points: PDoublePointArray read GetPoints;
  end;

  ILonLatPolygonLine = interface
    ['{A1F32B46-8C0B-46F1-97E9-D2347CF9FF5B}']
    function GetEnum: IEnumLonLatPoint;
    function IsSame(const ALine: ILonLatPolygonLine): Boolean;

    function GetBounds: ILonLatRect;
    property Bounds: ILonLatRect read GetBounds;

    function CalcPerimeter(const ADatum: IDatum): Double;
    function CalcArea(
      const ADatum: IDatum;
      const ANotifier: INotifierOperation = nil;
      const AOperationID: Integer = 0
    ): Double;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetPoints: PDoublePointArray;
    property Points: PDoublePointArray read GetPoints;
  end;

  ILonLatPath = interface
    ['{0E85CB46-D324-4052-BDE3-63F1C4A2665A}']
    function GetEnum: IEnumLonLatPoint;
    function IsSame(const APath: ILonLatPath): Boolean;

    function GetBounds: ILonLatRect;
    property Bounds: ILonLatRect read GetBounds;

    function CalcLength(const ADatum: IDatum): Double;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(AIndex: Integer): ILonLatPathLine;
    property Item[AIndex: Integer]: ILonLatPathLine read GetItem;
  end;

  ILonLatPolygon = interface
    ['{04CEBFBE-8FC1-4AB0-8B39-3C283287BF46}']
    function GetEnum: IEnumLonLatPoint;
    function IsSame(const APolygon: ILonLatPolygon): Boolean;

    function GetBounds: ILonLatRect;
    property Bounds: ILonLatRect read GetBounds;

    function CalcPerimeter(const ADatum: IDatum): Double;
    function CalcArea(
      const ADatum: IDatum;
      const ANotifier: INotifierOperation = nil;
      const AOperationID: Integer = 0
    ): Double;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(AIndex: Integer): ILonLatPolygonLine;
    property Item[AIndex: Integer]: ILonLatPolygonLine read GetItem;
  end;

implementation

end.
