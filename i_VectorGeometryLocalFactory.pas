unit i_VectorGeometryLocalFactory;

interface

uses
  t_GeoTypes,
  i_LocalCoordConverter,
  i_EnumDoublePoint,
  i_DoublePointsAggregator,
  i_VectorItemLocal;

type
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
