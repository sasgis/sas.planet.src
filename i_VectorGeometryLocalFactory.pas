unit i_VectorGeometryLocalFactory;

interface

uses
  t_GeoTypes,
  i_LocalCoordConverter,
  i_EnumDoublePoint,
  i_DoublePointsAggregator,
  i_GeometryLocal;

type
  IGeometryLocalFactory = interface
    ['{E44B8BA5-0443-40CC-8F48-F8B817D0328A}']
    function CreateLocalPath(
      const ALocalConverter: ILocalCoordConverter;
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryLocalMultiLine;
    function CreateLocalPolygon(
      const ALocalConverter: ILocalCoordConverter;
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryLocalMultiPolygon;

    function CreateLocalPathByEnum(
      const ALocalConverter: ILocalCoordConverter;
      const AEnum: IEnumLocalPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryLocalMultiLine;
    function CreateLocalPolygonByEnum(
      const ALocalConverter: ILocalCoordConverter;
      const AEnum: IEnumLocalPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryLocalMultiPolygon;
  end;

implementation

end.
