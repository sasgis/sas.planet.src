unit u_GeoCoderListSimple;

interface

uses
  i_ProxySettings,
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
  u_GeoCoderByYandex,
  u_GeoCoderBy2GIS;

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
  Add(
    TGeoCoderListEntity.Create(
      CGeoCoder2GISGUID,
      '2GIS',
      TGeoCoderBy2GIS.Create(AProxy)
    )
  );
end;

end.
