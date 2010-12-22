unit u_LocalCoordConverterFactorySimpe;

interface

uses
  Types,
  t_GeoTypes,
  i_ICoordConverter,
  i_ILocalCoordConverter,
  i_ILocalCoordConverterFactorySimpe;

type
  TLocalCoordConverterFactorySimpe = class(TInterfacedObject, ILocalCoordConverterFactorySimpe)
  protected
    function CreateConverter(
      ALocalRect: TRect;
      AZoom: Byte;
      AGeoConverter: ICoordConverter;
      AMapScale: TDoublePoint;
      ALocalTopLeftAtMap: TDoublePoint
    ): ILocalCoordConverter;
  end;


implementation

uses
  u_LocalCoordConverter;

{ TLocalCoordConverterFactorySimpe }

function TLocalCoordConverterFactorySimpe.CreateConverter(
  ALocalRect: TRect;
  AZoom: Byte;
  AGeoConverter: ICoordConverter;
  AMapScale,
  ALocalTopLeftAtMap: TDoublePoint
): ILocalCoordConverter;
begin
  Result := TLocalCoordConverter.Create(ALocalRect, AZoom, AGeoConverter, AMapScale, ALocalTopLeftAtMap);
end;

end.
