unit u_PathDetalizeProviderYourNavigation;

interface

uses
  t_GeoTypes,
  i_KmlInfoSimpleLoader,
  i_LanguageManager,
  u_UserInterfaceItemBase,
  i_PathDetalizeProvider;

type
  TPathDetalizeProviderYourNavigation = class(TInterfacedObject, IPathDetalizeProvider)
  private
    FBaseUrl: string;
    FKmlLoader: IKmlInfoSimpleLoader;
  protected
    function GetPath(ASource: TArrayOfDoublePoint; var AComment: string): TArrayOfDoublePoint;
    constructor Create(AKmlLoader: IKmlInfoSimpleLoader; ABaseUrl: string);
  end;

type
  TPathDetalizeProviderYourNavigationFastestByCar = class(TPathDetalizeProviderYourNavigation)
  public
    constructor Create(AKmlLoader: IKmlInfoSimpleLoader);
  end;

type
  TPathDetalizeProviderYourNavigationShortestByCar = class(TPathDetalizeProviderYourNavigation)
  public
    constructor Create(AKmlLoader: IKmlInfoSimpleLoader);
  end;

type
  TPathDetalizeProviderYourNavigationFastestByBicycle = class(TPathDetalizeProviderYourNavigation)
  public
    constructor Create(AKmlLoader: IKmlInfoSimpleLoader);
  end;

type
  TPathDetalizeProviderYourNavigationShortestByBicycle = class(TPathDetalizeProviderYourNavigation)
  public
    constructor Create(AKmlLoader: IKmlInfoSimpleLoader);
  end;

implementation

uses
  Classes,
  u_GeoToStr,
  i_VectorDataItemSimple,
  u_GlobalState,
  frm_InvisibleBrowser;

{ TPathDetalizeProviderYourNavigation }

constructor TPathDetalizeProviderYourNavigation.Create(AKmlLoader: IKmlInfoSimpleLoader; ABaseUrl: string);
begin
  FBaseUrl := ABaseUrl;
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
      if GetStreamFromURL(ms, url, 'text/xml')>0 then begin
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

constructor TPathDetalizeProviderYourNavigationFastestByCar.Create(AKmlLoader: IKmlInfoSimpleLoader);
begin
  inherited Create(
    AKmlLoader,
    'http://www.yournavigation.org/api/1.0/gosmore.php?format=kml&v=motorcar&fast=1&layer=mapnik'
  );
end;

{ TPathDetalizeProviderYourNavigationShortestByCar }

constructor TPathDetalizeProviderYourNavigationShortestByCar.Create(AKmlLoader: IKmlInfoSimpleLoader);
begin
  inherited Create(
    AKmlLoader,
    'http://www.yournavigation.org/api/1.0/gosmore.php?format=kml&v=motorcar&fast=0&layer=mapnik'
  );
end;

{ TPathDetalizeProviderYourNavigationFastestByBicycle }

constructor TPathDetalizeProviderYourNavigationFastestByBicycle.Create(AKmlLoader: IKmlInfoSimpleLoader);
begin
  inherited Create(
    AKmlLoader,
    'http://www.yournavigation.org/api/1.0/gosmore.php?format=kml&v=bicycle&fast=1&layer=mapnik'
  );
end;

{ TPathDetalizeProviderYourNavigationShortestByBicycle }

constructor TPathDetalizeProviderYourNavigationShortestByBicycle.Create(AKmlLoader: IKmlInfoSimpleLoader);
begin
  inherited Create(
    AKmlLoader,
    'http://www.yournavigation.org/api/1.0/gosmore.php?format=kml&v=bicycle&fast=0&layer=mapnik'
  );
end;

end.
