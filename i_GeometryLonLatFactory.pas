unit i_GeometryLonLatFactory;

interface

uses
  t_GeoTypes,
  i_ProjectionInfo,
  i_EnumDoublePoint,
  i_DoublePointFilter,
  i_DoublePointsAggregator,
  i_GeometryLonLat;

type
  IGeometryLonLatFactory = interface
    ['{FD69BBD0-2065-43B0-9D7C-900E82C28069}']
    function CreateLonLatPoint(
      const APoint: TDoublePoint
    ): IGeometryLonLatPoint;
    function CreateLonLatMultiLine(
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryLonLatMultiLine;
    function CreateLonLatMultiPolygon(
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryLonLatMultiPolygon;
    function CreateLonLatMultiLineByEnum(
      const AEnum: IEnumLonLatPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryLonLatMultiLine;
    function CreateLonLatMultiPolygonByEnum(
      const AEnum: IEnumLonLatPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryLonLatMultiPolygon;

    function CreateLonLatMultiPolygonByRect(
      const ARect: TDoubleRect
    ): IGeometryLonLatMultiPolygon;
    function CreateLonLatMultiPolygonCircleByPoint(
      const AProjection: IProjectionInfo;
      const APos: TDoublePoint;
      const ARadius: double
    ): IGeometryLonLatMultiPolygon;
    function CreateLonLatMultiPolygonByLonLatPathAndFilter(
      const ASource: IGeometryLonLatMultiLine;
      const AFilter: ILonLatPointFilter
    ): IGeometryLonLatMultiPolygon;
  end;

implementation

end.
