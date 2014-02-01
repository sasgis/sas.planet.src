unit i_VectorGeometryProjectedFactory;

interface

uses
  t_GeoTypes,
  i_ProjectionInfo,
  i_EnumDoublePoint,
  i_DoublePointsAggregator,
  i_DoublePointFilter,
  i_GeometryLonLat,
  i_GeometryProjected;

type
  IGeometryProjectedFactory = interface
    ['{06CC36BA-1833-4AE8-953F-D003B6D81BB7}']
    function CreateProjectedPath(
      const AProjection: IProjectionInfo;
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryProjectedMultiLine;
    function CreateProjectedPolygon(
      const AProjection: IProjectionInfo;
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryProjectedMultiPolygon;

    function CreateProjectedPolygonByRect(
      const AProjection: IProjectionInfo;
      const ARect: TDoubleRect
    ): IGeometryProjectedMultiPolygon;

    function CreateProjectedPathByEnum(
      const AProjection: IProjectionInfo;
      const AEnum: IEnumProjectedPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedMultiLine;
    function CreateProjectedPolygonByEnum(
      const AProjection: IProjectionInfo;
      const AEnum: IEnumProjectedPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedMultiPolygon;
    function CreateProjectedPathByLonLatEnum(
      const AProjection: IProjectionInfo;
      const AEnum: IEnumLonLatPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedMultiLine;
    function CreateProjectedPolygonByLonLatEnum(
      const AProjection: IProjectionInfo;
      const AEnum: IEnumLonLatPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedMultiPolygon;

    function CreateProjectedPathByLonLatPath(
      const AProjection: IProjectionInfo;
      const ASource: IGeometryLonLatMultiLine;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedMultiLine;
    function CreateProjectedPolygonByLonLatPolygon(
      const AProjection: IProjectionInfo;
      const ASource: IGeometryLonLatMultiPolygon;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedMultiPolygon;

    function CreateProjectedPathWithClipByLonLatEnum(
      const AProjection: IProjectionInfo;
      const AEnum: IEnumLonLatPoint;
      const AMapPixelsClipRect: TDoubleRect;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedMultiLine;
    function CreateProjectedPolygonWithClipByLonLatEnum(
      const AProjection: IProjectionInfo;
      const AEnum: IEnumLonLatPoint;
      const AMapPixelsClipRect: TDoubleRect;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedMultiPolygon;

    function CreateProjectedPathWithClipByLonLatPath(
      const AProjection: IProjectionInfo;
      const ASource: IGeometryLonLatMultiLine;
      const AMapPixelsClipRect: TDoubleRect;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedMultiLine;
    function CreateProjectedPolygonWithClipByLonLatPolygon(
      const AProjection: IProjectionInfo;
      const ASource: IGeometryLonLatMultiPolygon;
      const AMapPixelsClipRect: TDoubleRect;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedMultiPolygon;

    function CreateProjectedPathByLonLatPathUseConverter(
      const AProjection: IProjectionInfo;
      const ASource: IGeometryLonLatMultiLine;
      const AConverter: ILonLatPointConverter;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedMultiLine;
    function CreateProjectedPolygonByLonLatPolygonUseConverter(
      const AProjection: IProjectionInfo;
      const ASource: IGeometryLonLatMultiPolygon;
      const AConverter: ILonLatPointConverter;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedMultiPolygon;
  end;

implementation

end.
