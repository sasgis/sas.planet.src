unit i_VectorDataFactory;

interface

uses
  t_GeoTypes,
  i_VectorItemLonLat,
  i_VectorDataItemSimple;

type
  IVectorDataFactory = interface
    ['{F90BAFE2-B3A5-4C6B-9831-3E460F7771F6}']
    function BuildPoint(
      AId: string;
      AName: string;
      ADesc: string;
      APoint: TDoublePoint
    ): IVectorDataItemPoint;
    function BuildPath(
      AId: string;
      AName: string;
      ADesc: string;
      ALine: ILonLatPath
    ): IVectorDataItemLine;
    function BuildPoly(
      AId: string;
      AName: string;
      ADesc: string;
      APoly: ILonLatPolygon
    ): IVectorDataItemPoly;
  end;

implementation

end.
