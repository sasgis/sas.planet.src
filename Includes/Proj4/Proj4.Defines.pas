unit Proj4.Defines;

interface

const
  // EPSG:4326 = WGS 84 - WGS84 - World Geodetic System 1984, used in GPS
  wgs_84 = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs';

  // ESRI:53004 = Sphere Mercator
  esri_53004 = '+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs';

  // EPSG:3785 = Popular Visualisation CRS / Mercator - Spherical Mercator (deprecated EPSG code wrongly defined)
  epsg_3785 = '+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs';

  // EPSG:3857 = WGS 84 / Pseudo-Mercator - Spherical Mercator, Google Maps, OpenStreetMap, Bing, ArcGIS, ESRI
  epsg_3857 = epsg_3785;

  // EPSG:3395 = WGS 84 / World Mercator
  epsg_3395 = '+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs';

  // EPSG:4269 = NAD83
  nad_83 = '+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs';

  // EPSG:2463-2491 = Pulkovo 1995 / Gauss-Kruger CM
  // EPSG:2492-2522 = Pulkovo 1942 / Gauss-Kruger CM
  gauss_kruger_fmt = '+proj=tmerc +lat_0=0 +lon_0=%d +k=1 +x_0=%d +y_0=%d +ellps=krass +units=m +no_defs';

  // GOST P 51794-2008 (http://www.sasgis.org/mantis/view.php?id=3179)
  sk_42 = '+proj=longlat +ellps=krass +towgs84=23.57,-140.95,-79.8,0,0.35,0.79,-0.22 +no_defs';

  // EPSG:32601-32660 = WGS 84 / UTM Zone North
  utm_north_fmt = '+proj=utm +zone=%d +ellps=WGS84 +datum=WGS84 +units=m +no_defs';

  // EPSG:32701-32760 = WGS 84 / UTM Zone South
  utm_south_fmt = '+proj=utm +zone=%d +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs';

function Proj4ArgsByEpsg(const AEPSG: Integer): AnsiString;

implementation

uses
  SysUtils;

function Proj4ArgsByEpsg(const AEPSG: Integer): AnsiString;
var
  I: Integer;
begin
  case AEPSG of
    53004: Result := esri_53004;

    3785: Result := epsg_3785;

    3857: Result := epsg_3857;

    3395: Result := epsg_3395;

    4269: Result := nad_83;

    4326: Result := wgs_84;

    2463..2491: begin // Pulkovo 1995
      I := 21 + (AEPSG - 2463) * 6;
      if I > 180 then begin
        I := I - 360;
      end;
      Result := AnsiString(Format(gauss_kruger_fmt, [I, 500000, 0]));
    end;

    2492..2522: begin // Pulkovo 1942
      I := 9 + (AEPSG - 2492) * 6;
      if I > 180 then begin
        I := I - 360;
      end;
      Result := AnsiString(Format(gauss_kruger_fmt, [I, 500000, 0]));
    end;

    32601..32660: begin
      Result := AnsiString(Format(utm_north_fmt, [AEPSG - 32600]));
    end;

    32701..32760: begin
      Result := AnsiString(Format(utm_south_fmt, [AEPSG - 32700]));
    end;
  else
    Result := '';
  end;
end;

end.
