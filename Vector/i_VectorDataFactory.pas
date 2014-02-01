unit i_VectorDataFactory;

interface

uses
  i_Appearance,
  i_GeometryLonLat,
  i_VectorDataItemSimple;

type
  IVectorDataItemMainInfoFactory = interface
    ['{6A046EF1-D444-4996-AC8F-4645EC01FA68}']
    function BuildMainInfo(
      const AIdData: Pointer;
      const AName: string;
      const ADesc: string
    ): IVectorDataItemMainInfo;
  end;

  IVectorDataFactory = interface
    ['{F90BAFE2-B3A5-4C6B-9831-3E460F7771F6}']
    function BuildItem(
      const AMainInfo: IVectorDataItemMainInfo;
      const AAppearance: IAppearance;
      const AGeometry: IGeometryLonLat
    ): IVectorDataItemSimple;
    function BuildPoint(
      const AMainInfo: IVectorDataItemMainInfo;
      const AAppearance: IAppearance;
      const APoint: IGeometryLonLatPoint
    ): IVectorDataItemPoint;
    function BuildPath(
      const AMainInfo: IVectorDataItemMainInfo;
      const AAppearance: IAppearance;
      const ALine: IGeometryLonLatMultiLine
    ): IVectorDataItemLine;
    function BuildPoly(
      const AMainInfo: IVectorDataItemMainInfo;
      const AAppearance: IAppearance;
      const APoly: IGeometryLonLatMultiPolygon
    ): IVectorDataItemPoly;
  end;

implementation

end.
