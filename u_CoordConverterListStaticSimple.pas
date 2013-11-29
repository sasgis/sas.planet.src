unit u_CoordConverterListStaticSimple;

interface

uses
  i_CoordConverterFactory,
  u_CoordConverterListStatic;

type
  TCoordConverterListStaticSimple = class(TCoordConverterListStatic)
  public
    constructor Create(const AFactory: ICoordConverterFactory);
  end;

implementation

uses
  gnugettext,
  c_CoordConverter,
  i_CoordConverter;

{ TCoordConverterListStaticSimple }

constructor TCoordConverterListStaticSimple.Create;
var
  VConverter: ICoordConverter;
begin
  inherited Create;
  VConverter := AFactory.GetCoordConverterByCode(CGoogleProjectionEPSG, CTileSplitQuadrate256x256);
  Add(VConverter, gettext_NoOp('Merkator / Google Maps (Sphere Radius 6378137) / EPSG:3785'));

  VConverter := AFactory.GetCoordConverterByCode(CYandexProjectionEPSG, CTileSplitQuadrate256x256);
  Add(VConverter, gettext_NoOp('Merkator / WGS84 / EPSG:3395'));

  VConverter := AFactory.GetCoordConverterByCode(CGELonLatProjectionEPSG, CTileSplitQuadrate256x256);
  Add(VConverter, gettext_NoOp('Geographic (Latitude/Longitude) / WGS84 / EPSG:4326'));
end;

end.



