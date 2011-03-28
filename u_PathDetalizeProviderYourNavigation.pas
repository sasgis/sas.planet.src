unit u_PathDetalizeProviderYourNavigation;

interface

uses
  t_GeoTypes,
  i_PathDetalizeProvider;

type
  TPathDetalizeProviderYourNavigation = class(TInterfacedObject, IPathDetalizeProvider)
  private
    FBaseUrl: string;
  protected
    function GetPath(ASource: TArrayOfDoublePoint; var AComment: string): TArrayOfDoublePoint;
    constructor Create(ABaseUrl: string);
  end;

type
  TPathDetalizeProviderYourNavigation1 = class(TPathDetalizeProviderYourNavigation)
  public
    constructor Create;
  end;

type
  TPathDetalizeProviderYourNavigation11 = class(TPathDetalizeProviderYourNavigation)
  public
    constructor Create;
  end;

type
  TPathDetalizeProviderYourNavigation2 = class(TPathDetalizeProviderYourNavigation)
  public
    constructor Create;
  end;

type
  TPathDetalizeProviderYourNavigation22 = class(TPathDetalizeProviderYourNavigation)
  public
    constructor Create;
  end;

implementation

uses
  Classes,
  u_GeoToStr,
  u_KmlInfoSimple,
  u_GlobalState,
  frm_InvisibleBrowser;

{ TPathDetalizeProviderYourNavigation }

constructor TPathDetalizeProviderYourNavigation.Create(ABaseUrl: string);
begin
  FBaseUrl := ABaseUrl;
end;

function TPathDetalizeProviderYourNavigation.GetPath(ASource: TArrayOfDoublePoint;
  var AComment: string): TArrayOfDoublePoint;
var
  ms:TMemoryStream;
  url:string;
  i:integer;
  kml:TKmlInfoSimple;
  s,l:integer;
  conerr:boolean;
  add_line_arr_b:TArrayOfDoublePoint;
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
        kml:=TKmlInfoSimple.Create;
        try
          GState.KmlLoader.LoadFromStream(ms, kml);
          ms.SetSize(0);
          if (length(kml.Data)>0)and(length(kml.Data[0].coordinates)>0) then begin
            s:=length(add_line_arr_b);
            l:=length(kml.Data[0].coordinates);
            SetLength(add_line_arr_b,(s+l));
            Move(kml.Data[0].coordinates[0],add_line_arr_b[s],l*sizeof(TDoublePoint));
          end;
        finally
          kml.Free;
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

{ TPathDetalizeProviderYourNavigation1 }

constructor TPathDetalizeProviderYourNavigation1.Create;
begin
  inherited Create(
    'http://www.yournavigation.org/api/1.0/gosmore.php?format=kml&v=motorcar&fast=1&layer=mapnik'
  );
end;

{ TPathDetalizeProviderYourNavigation11 }

constructor TPathDetalizeProviderYourNavigation11.Create;
begin
  inherited Create(
    'http://www.yournavigation.org/api/1.0/gosmore.php?format=kml&v=motorcar&fast=0&layer=mapnik'
  );
end;

{ TPathDetalizeProviderYourNavigation2 }

constructor TPathDetalizeProviderYourNavigation2.Create;
begin
  inherited Create(
    'http://www.yournavigation.org/api/1.0/gosmore.php?format=kml&v=bicycle&fast=1&layer=mapnik'
  );
end;

{ TPathDetalizeProviderYourNavigation22 }

constructor TPathDetalizeProviderYourNavigation22.Create;
begin
  inherited Create(
    'http://www.yournavigation.org/api/1.0/gosmore.php?format=kml&v=bicycle&fast=0&layer=mapnik'
  );
end;

end.
