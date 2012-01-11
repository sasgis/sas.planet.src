unit i_VectorItmesFactory;

interface

uses
  t_GeoTypes,
  i_ProjectionInfo,
  i_EnumDoublePoint,
  i_DoublePointFilter,
  i_DoublePointsAggregator,
  i_VectorItemLonLat,
  i_VectorItemProjected;

type
  IVectorItmesFactory = interface
    ['{06CC36BA-1833-4AE8-953F-D003B6D81BB7}']
    function CreateLonLatPath(
      APoints: PDoublePointArray;
      ACount: Integer
    ): ILonLatPath;
    function CreateLonLatPolygon(
      APoints: PDoublePointArray;
      ACount: Integer
    ): ILonLatPolygon;
    function CreateProjectedPath(
      AProjection: IProjectionInfo;
      APoints: PDoublePointArray;
      ACount: Integer
    ): IProjectedPath;
    function CreateProjectedPolygon(
      AProjection: IProjectionInfo;
      APoints: PDoublePointArray;
      ACount: Integer
    ): IProjectedPolygon;

    function CreateLonLatPolygonLineByRect(
      ARect: TDoubleRect
    ): ILonLatPolygonLine;
    function CreateLonLatPolygonByRect(
      ARect: TDoubleRect
    ): ILonLatPolygon;
    function CreateProjectedPolygonLineByRect(
      AProjection: IProjectionInfo;
      ARect: TDoubleRect
    ): IProjectedPolygonLine;
    function CreateProjectedPolygonByRect(
      AProjection: IProjectionInfo;
      ARect: TDoubleRect
    ): IProjectedPolygon;

    function CreateProjectedPathByEnum(
      AProjection: IProjectionInfo;
      AEnum: IEnumProjectedPoint;
      ATemp: IDoublePointsAggregator = nil
    ): IProjectedPath;
    function CreateProjectedPolygonByEnum(
      AProjection: IProjectionInfo;
      AEnum: IEnumProjectedPoint;
      ATemp: IDoublePointsAggregator = nil
    ): IProjectedPolygon;

    function CreateProjectedPathByLonLatEnum(
      AProjection: IProjectionInfo;
      AEnum: IEnumLonLatPoint;
      ATemp: IDoublePointsAggregator = nil
    ): IProjectedPath;
    function CreateProjectedPolygonByLonLatEnum(
      AProjection: IProjectionInfo;
      AEnum: IEnumLonLatPoint;
      ATemp: IDoublePointsAggregator = nil
    ): IProjectedPolygon;

    function CreateProjectedPathByLonLatPath(
      AProjection: IProjectionInfo;
      ASource: ILonLatPath;
      ATemp: IDoublePointsAggregator = nil
    ): IProjectedPath;
    function CreateProjectedPolygonByLonLatPolygon(
      AProjection: IProjectionInfo;
      ASource: ILonLatPolygon;
      ATemp: IDoublePointsAggregator = nil
    ): IProjectedPolygon;

    function CreateProjectedPathWithClipByLonLatEnum(
      AProjection: IProjectionInfo;
      AEnum: IEnumLonLatPoint;
      AMapPixelsClipRect: TDoubleRect;
      ATemp: IDoublePointsAggregator = nil
    ): IProjectedPath;
    function CreateProjectedPolygonWithClipByLonLatEnum(
      AProjection: IProjectionInfo;
      AEnum: IEnumLonLatPoint;
      AMapPixelsClipRect: TDoubleRect;
      ATemp: IDoublePointsAggregator = nil
    ): IProjectedPolygon;

    function CreateProjectedPathWithClipByLonLatPath(
      AProjection: IProjectionInfo;
      ASource: ILonLatPath;
      AMapPixelsClipRect: TDoubleRect;
      ATemp: IDoublePointsAggregator = nil
    ): IProjectedPath;
    function CreateProjectedPolygonWithClipByLonLatPolygon(
      AProjection: IProjectionInfo;
      ASource: ILonLatPolygon;
      AMapPixelsClipRect: TDoubleRect;
      ATemp: IDoublePointsAggregator = nil
    ): IProjectedPolygon;

    function CreateProjectedPathByLonLatPathUseFilter(
      AProjection: IProjectionInfo;
      ASource: ILonLatPath;
      AConverter: ILonLatPointConverter;
      ATemp: IDoublePointsAggregator = nil
    ): IProjectedPath;
    function CreateProjectedPolygonByLonLatPolygonUseFilter(
      AProjection: IProjectionInfo;
      ASource: ILonLatPolygon;
      AConverter: ILonLatPointConverter;
      ATemp: IDoublePointsAggregator = nil
    ): IProjectedPolygon;
  end;

implementation

end.
