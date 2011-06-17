unit u_PathDetalizeProviderCloudMade;

interface

uses
  StrUtils,
  SysUtils,
  DateUtils,
  t_GeoTypes,
  i_PathDetalizeProvider;

type
  TRouteVehicle = (car,foot,bicycle);
  TRouteCalcType = (fastest,shortest);

type
  TPathDetalizeProviderCloudMade = class(TInterfacedObject, IPathDetalizeProvider)
  public
    constructor Create(Vehicle:TRouteVehicle; RouteCalcType:TRouteCalcType);
  private
    FBaseUrl: string;
    FVehicle: TRouteVehicle;
    FRouteCalcType: TRouteCalcType;
  protected
    function GetPath(ASource: TArrayOfDoublePoint; var AComment: string): TArrayOfDoublePoint;
    function SecondToTime(const Seconds: Cardinal): Double;
  end;


implementation

uses
  Classes,
  u_GeoToStr,
  u_ResStrings,
  i_VectorDataItemSimple,
  u_GlobalState,
  frm_InvisibleBrowser;

{ TPathDetalizeProviderCloudMade }

constructor TPathDetalizeProviderCloudMade.Create(Vehicle:TRouteVehicle; RouteCalcType:TRouteCalcType);
begin
  FBaseUrl := 'http://routes.cloudmade.com/BC9A493B41014CAABB98F0471D759707/api/0.3/';
  FVehicle := Vehicle;
  FRouteCalcType:=RouteCalcType;
end;

function TPathDetalizeProviderCloudMade.GetPath(ASource: TArrayOfDoublePoint;
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

  pathstr,timeT1:string;
  posit,posit2,dd,seconds,meters:integer;
  dateT1:TDateTime;

begin
  AComment := '';
  meters:=0;
  seconds:=0;
  SetLength(add_line_arr_b,0);
  ms:=TMemoryStream.Create;
  try
    conerr:=false;
    for i:= 0 to length(ASource)-2 do begin
      if conerr then Continue;
      url := FBaseUrl;
      url:=url+R2StrPoint(ASource[i].y)+','+R2StrPoint(ASource[i].x)+
          ','+R2StrPoint(ASource[i+1].y)+','+R2StrPoint(ASource[i+1].x);
      case FVehicle of
        car: url:=url+'/car';
        foot: url:=url+'/foot';
        bicycle: url:=url+'/bicycle';
      end;
      case FRouteCalcType of
        fastest: url:=url+'.js?units=km&lang=en&callback=getRoute6&translation=common';
        shortest: url:=url+'/shortest.js?units=km&lang=en&callback=getRoute6&translation=common';
      end;
      ms.Clear;
      if GetStreamFromURL(ms, url, 'application/json;charset=UTF-8')>0 then begin
        ms.Position:=0;
        SetLength(pathstr, ms.Size);
        ms.ReadBuffer(pathstr[1], ms.Size);
        s:=length(add_line_arr_b);
        try
          posit:=PosEx('[',pathstr,1);
          posit:=PosEx('[',pathstr,posit+1);
          if posit>0 then  begin
            While (posit>0) do begin
              SetLength(add_line_arr_b,(s+1));
              s:=length(add_line_arr_b);

              posit2:=PosEx(',',pathstr,posit);
              add_line_arr_b[s-1].Y:=str2r(copy(pathstr,posit+1,posit2-(posit+1)));
              posit:=PosEx(']',pathstr,posit2);
              add_line_arr_b[s-1].X:=str2r(copy(pathstr,posit2+1,posit-(posit2+1)));

              if pathstr[posit+1]=']' then begin
                posit:=-1;
              end else begin
                posit:=PosEx('[',pathstr,posit+1);
              end;
            end;
            posit:=PosEx('"total_distance":',pathstr,1);
            posit2:=PosEx(',',pathstr,posit);
            meters:=meters+strtoint(copy(pathstr,posit+17,posit2-(posit+17)));
            posit:=PosEx('"total_time":',pathstr,1);
            posit2:=PosEx(',',pathstr,posit);
            seconds:=seconds+strtoint(copy(pathstr,posit+13,posit2-(posit+13)));
          end;
        except
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
    if meters>1000 then begin
      AComment:=SAS_STR_MarshLen+' '+RoundEx(meters/1000,2)+' '+SAS_UNITS_km;
    end else begin
      AComment:=SAS_STR_MarshLen+' '+inttostr(meters)+' '+SAS_UNITS_m;
    end;
    DateT1:=SecondToTime(seconds);
    dd:=DaysBetween(0,DateT1);
    timeT1:='';
    if dd>0 then begin
      timeT1:=inttostr(dd)+' дней, ';
    end;
    timeT1:=timeT1+TimeToStr(DateT1);
    AComment:=AComment+#13#10+SAS_STR_Marshtime+timeT1;
  end;
end;

function TPathDetalizeProviderCloudMade.SecondToTime(const Seconds: Cardinal): Double;
const
  SecPerDay = 86400;
  SecPerHour = 3600;
  SecPerMinute = 60;
var
  ms, ss, mm, hh, dd: Cardinal;
begin
  dd := Seconds div SecPerDay;
  hh := (Seconds mod SecPerDay) div SecPerHour;
  mm := ((Seconds mod SecPerDay) mod SecPerHour) div SecPerMinute;
  ss := ((Seconds mod SecPerDay) mod SecPerHour) mod SecPerMinute;
  ms := 0;
  Result := dd + EncodeTime(hh, mm, ss, ms);
end;

end.
