unit i_VectorDataFactory;

interface

uses
  t_GeoTypes,
  i_Appearance,
  i_VectorItemLonLat,
  i_VectorDataItemSimple;

type
  IVectorDataFactory = interface
    ['{F90BAFE2-B3A5-4C6B-9831-3E460F7771F6}']
    function BuildPoint(
      const AIdData: Pointer;
      const AAppearance: IAppearance;
      const AName: string;
      const ADesc: string;
      const APoint: TDoublePoint
    ): IVectorDataItemPoint;
    function BuildPath(
      const AIdData: Pointer;
      const AAppearance: IAppearance;
      const AName: string;
      const ADesc: string;
      const ALine: ILonLatPath
    ): IVectorDataItemLine;
    function BuildPoly(
      const AIdData: Pointer;
      const AAppearance: IAppearance;
      const AName: string;
      const ADesc: string;
      const APoly: ILonLatPolygon
    ): IVectorDataItemPoly;
  end;

implementation

end.
