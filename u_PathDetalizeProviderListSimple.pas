unit u_PathDetalizeProviderListSimple;

interface

uses
  i_LanguageManager,
  i_KmlInfoSimpleLoader,
  u_PathDetalizeProviderListBase;

type
  TPathDetalizeProviderListSimple = class(TPathDetalizeProviderListBase)
  public
    constructor Create(
      ALanguageManager: ILanguageManager;
      AKmlLoader: IKmlInfoSimpleLoader
    );
  end;

implementation

uses
  i_PathDetalizeProviderList,
  u_PathDetalizeProviderYourNavigation,
  u_PathDetalizeProviderMailRu,
  u_PathDetalizeProviderCloudMade;

{ TPathDetalizeProviderListSimple }

constructor TPathDetalizeProviderListSimple.Create(
  ALanguageManager: ILanguageManager;
  AKmlLoader: IKmlInfoSimpleLoader
);
var
  VEntity: IPathDetalizeProviderListEntity;
begin
  inherited Create;
  VEntity := TPathDetalizeProviderMailRuShortest.Create(ALanguageManager);
  Add(VEntity);
  VEntity := TPathDetalizeProviderMailRuFastest.Create(ALanguageManager);
  Add(VEntity);
  VEntity := TPathDetalizeProviderMailRuFastestWithTraffic.Create(ALanguageManager);
  Add(VEntity);
  VEntity := TPathDetalizeProviderYourNavigationFastestByCar.Create(ALanguageManager, AKmlLoader);
  Add(VEntity);
  VEntity := TPathDetalizeProviderYourNavigationShortestByCar.Create(ALanguageManager, AKmlLoader);
  Add(VEntity);
  VEntity := TPathDetalizeProviderYourNavigationFastestByBicycle.Create(ALanguageManager, AKmlLoader);
  Add(VEntity);
  VEntity := TPathDetalizeProviderYourNavigationShortestByBicycle.Create(ALanguageManager, AKmlLoader);
  Add(VEntity);
  VEntity := TPathDetalizeProviderCloudMadeFastestByCar.Create(ALanguageManager);
  Add(VEntity);
  VEntity := TPathDetalizeProviderCloudMadeFastestByFoot.Create(ALanguageManager);
  Add(VEntity);
  VEntity := TPathDetalizeProviderCloudMadeFastestByBicycle.Create(ALanguageManager);
  Add(VEntity);
  VEntity := TPathDetalizeProviderCloudMadeShortestByCar.Create(ALanguageManager);
  Add(VEntity);
  VEntity := TPathDetalizeProviderCloudMadeShortestByFoot.Create(ALanguageManager);
  Add(VEntity);
  VEntity := TPathDetalizeProviderCloudMadeShortestByBicycle.Create(ALanguageManager);
  Add(VEntity);
end;

end.
