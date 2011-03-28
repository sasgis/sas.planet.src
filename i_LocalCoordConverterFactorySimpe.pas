unit i_LocalCoordConverterFactorySimpe;

interface

uses
  Types,
  t_GeoTypes,
  i_CoordConverter,
  i_LocalCoordConverter;

type
  ILocalCoordConverterFactorySimpe = interface
    ['{102D5E00-4F2C-4425-9EB9-ED4DD77141FB}']
    function CreateConverter(
      ALocalRect: TRect;
      AZoom: Byte;
      AGeoConverter: ICoordConverter;
      AMapScale: TDoublePoint;
      ALocalTopLeftAtMap: TDoublePoint
    ): ILocalCoordConverter;
  end;

implementation

end.
