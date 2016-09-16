unit Proj4Defs;

interface

const
  // WGS 84
  wgs_84 = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs';

  // Sphere Mercator ESRI:53004
  esri_53004 = '+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs';

  // Popular Visualisation CRS / Mercator
  epsg_3785 = '+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs';

  // WGS 84 / World Mercator
  epsg_3395 = '+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs';

  // NAD83
  nad_83 = '+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs';

  // 2463-2491 = Pulkovo 1995 / Gauss-Kruger CM
  // 2492-2522 = Pulkovo 1942 / Gauss-Kruger CM
  gauss_kruger_fmt = '+proj=tmerc +lat_0=0 +lon_0=%d +k=1 +x_0=%d +y_0=%d +ellps=krass +units=m +no_defs';

  sk_42 = '+proj=longlat +ellps=krass +towgs84=23.92,-141.27,-80.9,-0,0.35,0.82,-0.12 +no_defs';

  // 32601-32660 = WGS 84 / UTM zone N
  utm_fmt = '+proj=utm +zone=%d +ellps=WGS84 +datum=WGS84 +units=m +no_defs';


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
      Result := AnsiString(Format(utm_fmt, [AEPSG - 32600]));
    end;

  else
    Result := '';
  end;
end;

end.
