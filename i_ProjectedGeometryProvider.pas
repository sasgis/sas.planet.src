unit i_ProjectedGeometryProvider;

interface

uses
  i_ProjectionInfo,
  i_VectorItemLonLat,
  i_VectorItemProjected;

type
  IProjectedGeometryProvider = interface
    ['{D16A12D9-29DF-4349-8A3C-05B2BF50BD0D}']
    function GetProjectedPath(
      const AProjectionInfo: IProjectionInfo;
      const ALine: ILonLatPath
    ): IProjectedPath;
    function GetProjectedPolygon(
      const AProjectionInfo: IProjectionInfo;
      const ALine: ILonLatPolygon
    ): IProjectedPolygon;
  end;
implementation

end.
