unit u_PathDetalizeProviderYourNavigation;

interface

uses
  t_GeoTypes,
  i_KmlInfoSimpleLoader,
  i_LanguageManager,
  i_ProxySettings,
  u_PathDetalizeProviderListEntity;

type
  TPathDetalizeProviderYourNavigation = class(TPathDetalizeProviderListEntity)
  private
    FBaseUrl: string;
    FProxyConfig: IProxyConfig;
    FKmlLoader: IVectorDataLoader;
  protected { IPathDetalizeProvider }
    function GetPath(ASource: TArrayOfDoublePoint; var AComment: string): TArrayOfDoublePoint; override;
  public
    constructor Create(
      AGUID: TGUID;
      ALanguageManager: ILanguageManager;
      AProxyConfig: IProxyConfig;
      AKmlLoader: IVectorDataLoader;
      ABaseUrl: string
    );
  end;

type
  TPathDetalizeProviderYourNavigationFastestByCar = class(TPathDetalizeProviderYourNavigation)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;
  public
    constructor Create(
      ALanguageManager: ILanguageManager;
      AProxyConfig: IProxyConfig;
      AKmlLoader: IVectorDataLoader
    );
  end;

type
  TPathDetalizeProviderYourNavigationShortestByCar = class(TPathDetalizeProviderYourNavigation)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;
  public
    constructor Create(
      ALanguageManager: ILanguageManager;
      AProxyConfig: IProxyConfig;
      AKmlLoader: IVectorDataLoader
    );
  end;

type
  TPathDetalizeProviderYourNavigationFastestByBicycle = class(TPathDetalizeProviderYourNavigation)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;
  public
    constructor Create(
      ALanguageManager: ILanguageManager;
      AProxyConfig: IProxyConfig;
      AKmlLoader: IVectorDataLoader
    );
  end;

type
  TPathDetalizeProviderYourNavigationShortestByBicycle = class(TPathDetalizeProviderYourNavigation)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;
  public
    constructor Create(
      ALanguageManager: ILanguageManager;
      AProxyConfig: IProxyConfig;
      AKmlLoader: IVectorDataLoader
    );
  end;

implementation

uses
  Classes,
  gnugettext,
  c_PathDetalizeProvidersGUID,
  u_GeoToStr,
  i_VectorDataItemSimple,
  frm_InvisibleBrowser;

{ TPathDetalizeProviderYourNavigation }

constructor TPathDetalizeProviderYourNavigation.Create(
  AGUID: TGUID;
  ALanguageManager: ILanguageManager;
  AProxyConfig: IProxyConfig;
  AKmlLoader: IVectorDataLoader;
  ABaseUrl: string
);
begin
  inherited Create(AGUID, ALanguageManager);
  FBaseUrl := ABaseUrl;
  FProxyConfig := AProxyConfig;
  FKmlLoader := AKmlLoader;
end;

function TPathDetalizeProviderYourNavigation.GetPath(ASource: TArrayOfDoublePoint;
  var AComment: string): TArrayOfDoublePoint;
var
  ms:TMemoryStream;
  url:string;
  i:integer;
  kml:IVectorDataItemList;
  s,l:integer;
  conerr:boolean;
  add_line_arr_b:TArrayOfDoublePoint;
  VItem: IVectorDataItemSimple;
begin
  AComment := '';
  ms:=TMemoryStream.Create;
  try
    url := FBaseUrl;
    conerr:=false;
    for i:= 0 to length(ASource)-2 do begin
      if conerr then Continue;
      url:=url+'&flat='+R2StrPoint(ASource[i].y)+'&flon='+R2StrPoint(ASource[i].x)+
          '&tlat='+R2StrPoint(ASource[i+1].y)+'&tlon='+R2StrPoint(ASource[i+1].x);
      if GetStreamFromURL(ms, url, 'text/xml', FProxyConfig.GetStatic)>0 then begin
        FKmlLoader.LoadFromStream(ms, kml);
        if kml <> nil then begin
          ms.SetSize(0);
          if kml.Count > 0 then begin
            VItem := kml.GetItem(0);
            if Length(VItem.Points)>0 then begin
              s:=length(add_line_arr_b);
              l:=length(VItem.Points);
              SetLength(add_line_arr_b,(s+l));
              Move(VItem.Points[0], add_line_arr_b[s], l*sizeof(TDoublePoint));
            end;
          end;
        end;
      end else begin
        conerr:=true;
      end;
    end;
  finally
    ms.Free;
  end;
  if not conerr then begin
    Result := add_line_arr_b;
  end;
end;

{ TPathDetalizeProviderYourNavigationFastestByCar }

constructor TPathDetalizeProviderYourNavigationFastestByCar.Create(
  ALanguageManager: ILanguageManager;
  AProxyConfig: IProxyConfig;
  AKmlLoader: IVectorDataLoader
);
begin
  inherited Create(
    CPathDetalizeProviderYourNavigationFastestByCar,
    ALanguageManager,
    AProxyConfig,
    AKmlLoader,
    'http://www.yournavigation.org/api/1.0/gosmore.php?format=kml&v=motorcar&fast=1&layer=mapnik'
  );
end;

function TPathDetalizeProviderYourNavigationFastestByCar.GetCaptionTranslated: string;
begin
  Result := _('On car (Fastest) by yournavigation.org');
end;

function TPathDetalizeProviderYourNavigationFastestByCar.GetDescriptionTranslated: string;
begin
  Result := _('Detalize route on car (Fastest) by yournavigation.org');
end;

function TPathDetalizeProviderYourNavigationFastestByCar.GetMenuItemNameTranslated: string;
begin
  Result := _('On car (Fastest)');
end;

{ TPathDetalizeProviderYourNavigationShortestByCar }

constructor TPathDetalizeProviderYourNavigationShortestByCar.Create(
  ALanguageManager: ILanguageManager;
  AProxyConfig: IProxyConfig;
  AKmlLoader: IVectorDataLoader
);
begin
  inherited Create(
    CPathDetalizeProviderYourNavigationShortestByCar,
    ALanguageManager,
    AProxyConfig,
    AKmlLoader,
    'http://www.yournavigation.org/api/1.0/gosmore.php?format=kml&v=motorcar&fast=0&layer=mapnik'
  );
end;

function TPathDetalizeProviderYourNavigationShortestByCar.GetCaptionTranslated: string;
begin
  Result := _('On car (Shortest) by yournavigation.org');
end;

function TPathDetalizeProviderYourNavigationShortestByCar.GetDescriptionTranslated: string;
begin
  Result := _('Detalize route on car (Shortest) by yournavigation.org');
end;

function TPathDetalizeProviderYourNavigationShortestByCar.GetMenuItemNameTranslated: string;
begin
  Result := _('On car (Shortest)');
end;

{ TPathDetalizeProviderYourNavigationFastestByBicycle }

constructor TPathDetalizeProviderYourNavigationFastestByBicycle.Create(
  ALanguageManager: ILanguageManager;
  AProxyConfig: IProxyConfig;
  AKmlLoader: IVectorDataLoader
);
begin
  inherited Create(
    CPathDetalizeProviderYourNavigationFastestByBicycle,
    ALanguageManager,
    AProxyConfig,
    AKmlLoader,
    'http://www.yournavigation.org/api/1.0/gosmore.php?format=kml&v=bicycle&fast=1&layer=mapnik'
  );
end;

function TPathDetalizeProviderYourNavigationFastestByBicycle.GetCaptionTranslated: string;
begin
  Result := _('On bicycle (Fastest) by yournavigation.org');
end;

function TPathDetalizeProviderYourNavigationFastestByBicycle.GetDescriptionTranslated: string;
begin
  Result := _('Detalize route on bicycle (Fastest) by yournavigation.org');
end;

function TPathDetalizeProviderYourNavigationFastestByBicycle.GetMenuItemNameTranslated: string;
begin
  Result := _('On bicycle (Fastest)');
end;

{ TPathDetalizeProviderYourNavigationShortestByBicycle }

constructor TPathDetalizeProviderYourNavigationShortestByBicycle.Create(
  ALanguageManager: ILanguageManager;
  AProxyConfig: IProxyConfig;
  AKmlLoader: IVectorDataLoader
);
begin
  inherited Create(
    CPathDetalizeProviderYourNavigationShortestByBicycle,
    ALanguageManager,
    AProxyConfig,
    AKmlLoader,
    'http://www.yournavigation.org/api/1.0/gosmore.php?format=kml&v=bicycle&fast=0&layer=mapnik'
  );
end;

function TPathDetalizeProviderYourNavigationShortestByBicycle.GetCaptionTranslated: string;
begin
  Result := _('On bicycle (Shortest) by yournavigation.org');
end;

function TPathDetalizeProviderYourNavigationShortestByBicycle.GetDescriptionTranslated: string;
begin
  Result := _('Detalize route on bicycle (Shortest) by yournavigation.org');
end;

function TPathDetalizeProviderYourNavigationShortestByBicycle.GetMenuItemNameTranslated: string;
begin
  Result := _('On bicycle (Shortest)');
end;

end.
