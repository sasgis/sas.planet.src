unit i_RegionProcessProgressInfoInternalFactory;

interface

uses
  i_VectorItemLonLat,
  i_RegionProcessProgressInfo;

type
  IRegionProcessProgressInfoInternalFactory = interface
    ['{5D826B77-3BA1-43CF-82D8-E28B02BBBFE8}']
    function Build(
      const APolygon: ILonLatPolygon
    ): IRegionProcessProgressInfoInternal;
  end;

implementation

end.
