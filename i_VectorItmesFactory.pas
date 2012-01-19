unit i_VectorItmesFactory;

interface

uses
  t_GeoTypes,
  i_ProjectionInfo,
  i_LocalCoordConverter,
  i_EnumDoublePoint,
  i_DoublePointFilter,
  i_DoublePointsAggregator,
  i_VectorItemLonLat,
  i_VectorItemProjected,
  i_VectorItemLocal;

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
    function CreateLonLatPathByEnum(
      AEnum: IEnumLonLatPoint;
      ATemp: IDoublePointsAggregator = nil
    ): ILonLatPath;
    function CreateLonLatPolygonByEnum(
      AEnum: IEnumLonLatPoint;
      ATemp: IDoublePointsAggregator = nil
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

    function CreateLocalPath(
      ALocalConverter: ILocalCoordConverter;
      APoints: PDoublePointArray;
      ACount: Integer
    ): ILocalPath;
    function CreateLocalPolygon(
      ALocalConverter: ILocalCoordConverter;
      APoints: PDoublePointArray;
      ACount: Integer
    ): ILocalPolygon;

    function CreateLonLatPolygonByRect(
      ARect: TDoubleRect
    ): ILonLatPolygon;
    function CreateProjectedPolygonByRect(
      AProjection: IProjectionInfo;
      ARect: TDoubleRect
    ): IProjectedPolygon;

    function CreateLonLatPolygonByLonLatPathAndFilter(
      ASource: ILonLatPath;
      AFilter: ILonLatPointFilter
    ): ILonLatPolygon;

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
    function CreateLocalPathByEnum(
      ALocalConverter: ILocalCoordConverter;
      AEnum: IEnumLocalPoint;
      ATemp: IDoublePointsAggregator = nil
    ): ILocalPath;
    function CreateLocalPolygonByEnum(
      ALocalConverter: ILocalCoordConverter;
      AEnum: IEnumLocalPoint;
      ATemp: IDoublePointsAggregator = nil
    ): ILocalPolygon;

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

    function CreateProjectedPathByLonLatPathUseConverter(
      AProjection: IProjectionInfo;
      ASource: ILonLatPath;
      AConverter: ILonLatPointConverter;
      ATemp: IDoublePointsAggregator = nil
    ): IProjectedPath;
    function CreateProjectedPolygonByLonLatPolygonUseConverter(
      AProjection: IProjectionInfo;
      ASource: ILonLatPolygon;
      AConverter: ILonLatPointConverter;
      ATemp: IDoublePointsAggregator = nil
    ): IProjectedPolygon;
  end;

implementation

end.
