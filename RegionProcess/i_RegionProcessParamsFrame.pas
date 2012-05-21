unit i_RegionProcessParamsFrame;

interface

uses
  i_VectorItemLonLat;

type
  IRegionProcessParamsFrameBase = interface
    ['{F5346D9B-766C-4B3B-AC4B-9AC71FF62F05}']
    procedure Init(
      const AZoom: byte;
      const APolygon: ILonLatPolygon
    );
  end;

implementation

end.
