unit i_VectorItemsFactory;

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
  IVectorGeometryLonLatFactory = interface
    ['{FD69BBD0-2065-43B0-9D7C-900E82C28069}']
    function CreateLonLatPath(
      const APoints: PDoublePointArray;
      ACount: Integer
    ): ILonLatPath;
    function CreateLonLatPolygon(
      const APoints: PDoublePointArray;
      ACount: Integer
    ): ILonLatPolygon;
    function CreateLonLatPathByEnum(
      const AEnum: IEnumLonLatPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): ILonLatPath;
    function CreateLonLatPolygonByEnum(
      const AEnum: IEnumLonLatPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): ILonLatPolygon;

    function CreateLonLatPolygonByRect(
      const ARect: TDoubleRect
    ): ILonLatPolygon;
    function CreateLonLatPolygonCircleByPoint(
      const AProjection: IProjectionInfo;
      const APos: TDoublePoint;
      const ARadius: double
    ): ILonLatPolygon;
    function CreateLonLatPolygonByLonLatPathAndFilter(
      const ASource: ILonLatPath;
      const AFilter: ILonLatPointFilter
    ): ILonLatPolygon;
  end;

  IVectorGeometryProjectedFactory = interface
    ['{06CC36BA-1833-4AE8-953F-D003B6D81BB7}']
    function CreateProjectedPath(
      const AProjection: IProjectionInfo;
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IProjectedPath;
    function CreateProjectedPolygon(
      const AProjection: IProjectionInfo;
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IProjectedPolygon;

    function CreateProjectedPolygonByRect(
      const AProjection: IProjectionInfo;
      const ARect: TDoubleRect
    ): IProjectedPolygon;

    function CreateProjectedPathByEnum(
      const AProjection: IProjectionInfo;
      const AEnum: IEnumProjectedPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IProjectedPath;
    function CreateProjectedPolygonByEnum(
      const AProjection: IProjectionInfo;
      const AEnum: IEnumProjectedPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IProjectedPolygon;
    function CreateProjectedPathByLonLatEnum(
      const AProjection: IProjectionInfo;
      const AEnum: IEnumLonLatPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IProjectedPath;
    function CreateProjectedPolygonByLonLatEnum(
      const AProjection: IProjectionInfo;
      const AEnum: IEnumLonLatPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IProjectedPolygon;

    function CreateProjectedPathByLonLatPath(
      const AProjection: IProjectionInfo;
      const ASource: ILonLatPath;
      const ATemp: IDoublePointsAggregator = nil
    ): IProjectedPath;
    function CreateProjectedPolygonByLonLatPolygon(
      const AProjection: IProjectionInfo;
      const ASource: ILonLatPolygon;
      const ATemp: IDoublePointsAggregator = nil
    ): IProjectedPolygon;

    function CreateProjectedPathWithClipByLonLatEnum(
      const AProjection: IProjectionInfo;
      const AEnum: IEnumLonLatPoint;
      const AMapPixelsClipRect: TDoubleRect;
      const ATemp: IDoublePointsAggregator = nil
    ): IProjectedPath;
    function CreateProjectedPolygonWithClipByLonLatEnum(
      const AProjection: IProjectionInfo;
      const AEnum: IEnumLonLatPoint;
      const AMapPixelsClipRect: TDoubleRect;
      const ATemp: IDoublePointsAggregator = nil
    ): IProjectedPolygon;

    function CreateProjectedPathWithClipByLonLatPath(
      const AProjection: IProjectionInfo;
      const ASource: ILonLatPath;
      const AMapPixelsClipRect: TDoubleRect;
      const ATemp: IDoublePointsAggregator = nil
    ): IProjectedPath;
    function CreateProjectedPolygonWithClipByLonLatPolygon(
      const AProjection: IProjectionInfo;
      const ASource: ILonLatPolygon;
      const AMapPixelsClipRect: TDoubleRect;
      const ATemp: IDoublePointsAggregator = nil
    ): IProjectedPolygon;

    function CreateProjectedPathByLonLatPathUseConverter(
      const AProjection: IProjectionInfo;
      const ASource: ILonLatPath;
      const AConverter: ILonLatPointConverter;
      const ATemp: IDoublePointsAggregator = nil
    ): IProjectedPath;
    function CreateProjectedPolygonByLonLatPolygonUseConverter(
      const AProjection: IProjectionInfo;
      const ASource: ILonLatPolygon;
      const AConverter: ILonLatPointConverter;
      const ATemp: IDoublePointsAggregator = nil
    ): IProjectedPolygon;
  end;

  IVectorGeometryLocalFactory = interface
    ['{E44B8BA5-0443-40CC-8F48-F8B817D0328A}']
    function CreateLocalPath(
      const ALocalConverter: ILocalCoordConverter;
      const APoints: PDoublePointArray;
      ACount: Integer
    ): ILocalPath;
    function CreateLocalPolygon(
      const ALocalConverter: ILocalCoordConverter;
      const APoints: PDoublePointArray;
      ACount: Integer
    ): ILocalPolygon;

    function CreateLocalPathByEnum(
      const ALocalConverter: ILocalCoordConverter;
      const AEnum: IEnumLocalPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): ILocalPath;
    function CreateLocalPolygonByEnum(
      const ALocalConverter: ILocalCoordConverter;
      const AEnum: IEnumLocalPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): ILocalPolygon;
  end;

implementation

end.
