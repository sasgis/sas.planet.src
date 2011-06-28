unit u_GeoCoderListSimple;

interface

uses
  i_InetConfig,
  u_GeoCoderListBase;

type
  TGeoCoderListSimple = class(TGeoCoderListBase)
  public
    constructor Create(AInetConfig: IInetConfig);
  end;

implementation

uses
  SysUtils,
  c_GeoCoderGUIDSimple,
  u_GeoCoderListEntity,
  u_GeoCoderByGoogle,
  u_GeoCoderByYandex;

{ TGeoCoderListSimple }

constructor TGeoCoderListSimple.Create(AInetConfig: IInetConfig);
begin
  inherited Create;
  Add(
    TGeoCoderListEntity.Create(
      CGeoCoderGoogleGUID,
      'Google',
      TGeoCoderByGoogle.Create(AInetConfig)
    )
  );
  Add(
    TGeoCoderListEntity.Create(
      CGeoCoderYandexGUID,
      'Yandex',
      TGeoCoderByYandex.Create(AInetConfig)
    )
  );
end;

end.
