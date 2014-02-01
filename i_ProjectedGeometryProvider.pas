unit i_ProjectedGeometryProvider;

interface

uses
  i_ProjectionInfo,
  i_GeometryLonLat,
  i_GeometryProjected;

type
  IProjectedGeometryProvider = interface
    ['{D16A12D9-29DF-4349-8A3C-05B2BF50BD0D}']
    function GetProjectedPath(
      const AProjectionInfo: IProjectionInfo;
      const ALine: IGeometryLonLatMultiLine
    ): IGeometryProjectedMultiLine;
    function GetProjectedPolygon(
      const AProjectionInfo: IProjectionInfo;
      const ALine: IGeometryLonLatMultiPolygon
    ): IGeometryProjectedMultiPolygon;
  end;
implementation

end.
