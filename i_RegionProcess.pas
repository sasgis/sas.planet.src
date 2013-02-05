unit i_RegionProcess;

interface

uses
  i_VectorItemLonLat;

type
  IRegionProcess = interface
    procedure ProcessPolygon(
      const APolygon: ILonLatPolygon
    );
    procedure ProcessPolygonWithZoom(
      const AZoom: Byte;
      const APolygon: ILonLatPolygon
    );
  end;

implementation

end.
