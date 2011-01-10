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
  c_GeoCoderGUIDSimple,
  u_GeoCoderListEntity,
  u_GeoCoderByGoogle,
  u_GeoCoderByYandex;

{ TGeoCoderListSimple }

constructor TGeoCoderListSimple.Create(AProxy: IProxySettings);
begin
  inherited Create;
  Add(
    TGeoCoderListEntity.Create(
      CGeoCoderGoogleGUID,
      'Google',
      TGeoCoderByGoogle.Create(AProxy)
    )
  );
  Add(
    TGeoCoderListEntity.Create(
      CGeoCoderYandexGUID,
      'Yandex',
      TGeoCoderByYandex.Create(AProxy)
    )
  );
end;

end.
