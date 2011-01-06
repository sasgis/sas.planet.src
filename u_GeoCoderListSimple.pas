unit u_GeoCoderListSimple;

interface

uses
  i_IProxySettings,
  u_GeoCoderListBase;

type
  TGeoCoderListSimple = class(TGeoCoderListBase)
  public
    constructor Create(AProxy: IProxySettings);
  end;

implementation

uses
  SysUtils,
  u_GeoCoderListEntity,
  u_GeoCoderByGoogle,
  u_GeoCoderByYandex;

{ TGeoCoderListSimple }

constructor TGeoCoderListSimple.Create(AProxy: IProxySettings);
begin
  inherited Create;
  Add(
    TGeoCoderListEntity.Create(
      StringToGUID('{012C3CBF-9EDF-44F1-B728-346C9585A95C}'),
      'Google',
      TGeoCoderByGoogle.Create(AProxy)
    )
  );
  Add(
    TGeoCoderListEntity.Create(
      StringToGUID('{67496A88-0531-4C1D-9FF1-81F20683B38B}'),
      'Yandex',
      TGeoCoderByYandex.Create(AProxy)
    )
  );
end;

end.
