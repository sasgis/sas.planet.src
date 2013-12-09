unit i_RegionProcess;

interface

uses
  i_VectorItemLonLat;

type
  IRegionProcess = interface
    procedure ProcessPolygon(
      const APolygon: IGeometryLonLatMultiPolygon
    );
    procedure ProcessPolygonWithZoom(
      const AZoom: Byte;
      const APolygon: IGeometryLonLatMultiPolygon
    );
  end;

implementation

end.
