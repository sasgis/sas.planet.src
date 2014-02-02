unit i_GeometryProjected;

interface

uses
  t_GeoTypes,
  i_EnumDoublePoint,
  i_ProjectionInfo;

type
  IGeometryProjected = interface
    ['{162D40D7-29D5-44B1-BDB8-7E7616289769}']
    function GetProjection: IProjectionInfo;
    property Projection: IProjectionInfo read GetProjection;

    function GetBounds: TDoubleRect;
    property Bounds: TDoubleRect read GetBounds;
  end;

  IGeometryProjectedLine = interface(IGeometryProjected)
    ['{0D9B7321-DBA0-494F-959C-5026DB27C681}']
    function GetEnum: IEnumProjectedPoint;

    function IsPointOnPath(
      const APoint: TDoublePoint;
      ADist: Double
    ): Boolean;
    function IsRectIntersectPath(const ARect: TDoubleRect): Boolean;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetPoints: PDoublePointArray;
    property Points: PDoublePointArray read GetPoints;
  end;

  IGeometryProjectedPolygon = interface(IGeometryProjected)
    ['{30424113-D148-45EB-A4C8-C0150DB89D22}']
    function GetEnum: IEnumProjectedPoint;

    function IsPointInPolygon(const APoint: TDoublePoint): Boolean;
    function IsPointOnBorder(
      const APoint: TDoublePoint;
      ADist: Double
    ): Boolean;
    function IsRectIntersectPolygon(const ARect: TDoubleRect): Boolean;
    function IsRectIntersectBorder(const ARect: TDoubleRect): Boolean;
    function CalcArea: Double;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetPoints: PDoublePointArray;
    property Points: PDoublePointArray read GetPoints;
  end;

  IGeometryProjectedMultiLine = interface(IGeometryProjected)
    ['{781FAF61-C109-4CC9-A861-90CBE807D8E1}']
    function GetEnum: IEnumProjectedPoint;

    function IsPointOnPath(
      const APoint: TDoublePoint;
      const ADist: Double
    ): Boolean;
    function IsRectIntersectPath(const ARect: TDoubleRect): Boolean;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(AIndex: Integer): IGeometryProjectedLine;
    property Item[AIndex: Integer]: IGeometryProjectedLine read GetItem;
  end;

  IGeometryProjectedMultiPolygon = interface(IGeometryProjected)
    ['{02C310DE-60C3-4175-8811-367D5C5AC0CE}']
    function GetEnum: IEnumProjectedPoint;

    function IsPointInPolygon(const APoint: TDoublePoint): Boolean;
    function IsPointOnBorder(
      const APoint: TDoublePoint;
      const ADist: Double
    ): Boolean;
    function IsRectIntersectPolygon(const ARect: TDoubleRect): Boolean;
    function IsRectIntersectBorder(const ARect: TDoubleRect): Boolean;
    function CalcArea: Double;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(AIndex: Integer): IGeometryProjectedPolygon;
    property Item[AIndex: Integer]: IGeometryProjectedPolygon read GetItem;
  end;

implementation

end.
