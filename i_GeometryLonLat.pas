unit i_GeometryLonLat;

interface

uses
  t_GeoTypes,
  t_Hash,
  i_NotifierOperation,
  i_EnumDoublePoint,
  i_LonLatRect,
  i_Datum;

type
  IGeometryLonLat = interface
    ['{E53FCF09-DA26-44B1-854C-CC2A1330A3F0}']
    function GetHash: THashValue;
    property Hash: THashValue read GetHash;

    function GetBounds: ILonLatRect;
    property Bounds: ILonLatRect read GetBounds;

    function IsSameGeometry(const AGeometry: IGeometryLonLat): Boolean;
  end;

  IGeometryLonLatPoint = interface(IGeometryLonLat)
    ['{C52B78AD-2635-48A6-9C8B-E94C4592CFD0}']
    function IsSame(const APoint: IGeometryLonLatPoint): Boolean;

    function GetPoint: TDoublePoint;
    property Point: TDoublePoint read GetPoint;
  end;

  IGeometryLonLatLine = interface(IGeometryLonLat)
    ['{F309D486-2E2A-4526-8BB8-A38A47E3C8FF}']
    function IsSame(const ALine: IGeometryLonLatLine): Boolean;

    function GetEnum: IEnumLonLatPoint;
    function CalcLength(const ADatum: IDatum): Double;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetPoints: PDoublePointArray;
    property Points: PDoublePointArray read GetPoints;
  end;

  IGeometryLonLatPolygon = interface(IGeometryLonLat)
    ['{C9FF5A32-B90D-43D2-9394-9E54A4F29905}']
    function IsSame(const ALine: IGeometryLonLatPolygon): Boolean;

    function GetEnum: IEnumLonLatPoint;
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

  IGeometryLonLatMultiLine = interface(IGeometryLonLat)
    ['{5BB3E4AF-5420-4EDB-9DE0-D44FFA38519E}']
    function IsSame(const ALine: IGeometryLonLatMultiLine): Boolean;

    function GetEnum: IEnumLonLatPoint;

    function CalcLength(const ADatum: IDatum): Double;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(AIndex: Integer): IGeometryLonLatLine;
    property Item[AIndex: Integer]: IGeometryLonLatLine read GetItem;
  end;

  IGeometryLonLatMultiPolygon = interface(IGeometryLonLat)
    ['{E71E059B-8FB3-42AD-97BD-7777AC66C8F2}']
    function IsSame(const ALine: IGeometryLonLatMultiPolygon): Boolean;

    function GetEnum: IEnumLonLatPoint;

    function CalcPerimeter(const ADatum: IDatum): Double;
    function CalcArea(
      const ADatum: IDatum;
      const ANotifier: INotifierOperation = nil;
      const AOperationID: Integer = 0
    ): Double;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(AIndex: Integer): IGeometryLonLatPolygon;
    property Item[AIndex: Integer]: IGeometryLonLatPolygon read GetItem;
  end;

implementation

end.
