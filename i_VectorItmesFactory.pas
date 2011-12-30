unit i_VectorItmesFactory;

interface

uses
  t_GeoTypes,
  i_ProjectionInfo,
  i_EnumDoublePoint,
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
      AEnum: IEnumDoublePoint;
      ATemp: IDoublePointsAggregator
    ): IProjectedPath;
    function CreateProjectedPolygonByEnum(
      AProjection: IProjectionInfo;
      AEnum: IEnumDoublePoint;
      ATemp: IDoublePointsAggregator
    ): IProjectedPolygon;
  end;

implementation

end.
