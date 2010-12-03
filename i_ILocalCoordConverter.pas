unit i_ILocalCoordConverter;

interface

uses
  Types,
  t_GeoTypes,
  i_ICoordConverter;

type
  ILocalCoordConverter = interface
    ['{48CD8E96-6EB3-4162-B321-B8B64D71B0AB}']
    function GetGeoConverter: ICoordConverter;
    function LocalPixel2MapPixel(APoint: TPoint): TPoint;
    function LocalPixelFloat2MapPixelFloat(APoint: TDoublePoint): TDoublePoint;
    function MapPixel2LocalPixel(APoint: TPoint): TPoint;
    function MapPixelFloat2LocalPixelFloat(APoint: TDoublePoint): TDoublePoint;
  end;

implementation

end.
