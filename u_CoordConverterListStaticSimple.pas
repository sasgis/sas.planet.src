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
  Add(VConverter, gettext_NoOp('Google projection'));

  VConverter := AFactory.GetCoordConverterByCode(CYandexProjectionEPSG, CTileSplitQuadrate256x256);
  Add(VConverter, gettext_NoOp('Yandex projection'));

  VConverter := AFactory.GetCoordConverterByCode(CGELonLatProjectionEPSG, CTileSplitQuadrate256x256);
  Add(VConverter, gettext_NoOp('LonLat projection'));
end;

end.
