unit u_PathDetalizeProviderMailRu;

interface

uses
  t_GeoTypes,
  i_PathDetalizeProvider;

type
  TPathDetalizeProviderMailRu = class(TInterfacedObject, IPathDetalizeProvider)
  private
    FBaseUrl: string;
    function SecondToTime(const Seconds: Cardinal): Double;
  protected
    function GetPath(ASource: TArrayOfDoublePoint; var AComment: string): TArrayOfDoublePoint;
    constructor Create(ABaseUrl: string);
  end;

type
  TPathDetalizeProviderMailRu1 = class(TPathDetalizeProviderMailRu)
  public
    constructor Create;
  end;

type
  TPathDetalizeProviderMailRu2 = class(TPathDetalizeProviderMailRu)
  public
    constructor Create;
  end;

type
  TPathDetalizeProviderMailRu3 = class(TPathDetalizeProviderMailRu)
  public
    constructor Create;
  end;

implementation

uses
  Classes,
  SysUtils,
  StrUtils,
  DateUtils,
  u_GeoToStr,
  UResStrings,
  frm_InvisibleBrowser;

{ TPathDetalizeProviderMailRu }

constructor TPathDetalizeProviderMailRu.Create(ABaseUrl: string);
begin
  FBaseUrl := ABaseUrl;
end;

function TPathDetalizeProviderMailRu.GetPath(ASource: TArrayOfDoublePoint;
  var AComment: string): TArrayOfDoublePoint;
var ms:TMemoryStream;
    pathstr,timeT1:string;
    url:string;
    i,posit,posit2,endpos,dd,seconds,meters:integer;
    dateT1:TDateTime;
begin
  url := FBaseUrl;
  for i:=0 to length(ASource)-1 do begin
    url:=url+'&x'+inttostr(i)+'='+R2StrPoint(ASource[i].x)+'&y'+inttostr(i)+'='+R2StrPoint(ASource[i].y);
  end;
  ms:=TMemoryStream.Create;
  try
    if GetStreamFromURL(ms,url,'text/javascript; charset=utf-8')>0 then begin
      ms.Position:=0;
      SetLength(pathstr, ms.Size);
      ms.ReadBuffer(pathstr[1], ms.Size);
      SetLength(Result,0);
      meters:=0;
      seconds:=0;

      try
        posit:=PosEx('"totalLength"',pathstr,1);
        While (posit>0) do begin
          try
            posit2:=PosEx('"',pathstr,posit+17);
            meters:=meters+strtoint(copy(pathstr,posit+17,posit2-(posit+17)));
            posit:=PosEx('"totalTime"',pathstr,posit);
            posit2:=PosEx('"',pathstr,posit+15);
            seconds:=seconds+strtoint(copy(pathstr,posit+15,posit2-(posit+15)));
          except
          end;
          posit:=PosEx('"points"',pathstr,posit);
          endpos:=PosEx(']',pathstr,posit);
          while (posit>0)and(posit<endpos) do begin
            try
              SetLength(Result,length(Result)+1);
              posit:=PosEx('"x" : "',pathstr,posit);
              posit2:=PosEx('", "y" : "',pathstr,posit);
              Result[length(Result)-1].X:=str2r(copy(pathstr,posit+7,posit2-(posit+7)));
              posit:=PosEx('"',pathstr,posit2+10);
              Result[length(Result)-1].y:=str2r(copy(pathstr,posit2+10,posit-(posit2+10)));
              posit:=PosEx('{',pathstr,posit);
            except
              SetLength(Result,length(Result)-1);
            end;
          end;
          posit:=PosEx('"totalLength"',pathstr,posit);
        end;
      except
      end;

      if meters>1000 then begin
        AComment:=SAS_STR_MarshLen+RoundEx(meters/1000,2)+' '+SAS_UNITS_km;
      end else begin
        AComment:=SAS_STR_MarshLen+inttostr(meters)+' '+SAS_UNITS_m;
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
  finally
    ms.Free;
  end;
end;

function TPathDetalizeProviderMailRu.SecondToTime(
  const Seconds: Cardinal): Double;
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

{ TPathDetalizeProviderMailRu1 }

constructor TPathDetalizeProviderMailRu1.Create;
begin
  inherited Create(
    'http://maps.mail.ru/stamperx/getPath.aspx?mode=distance'
  );
end;

{ TPathDetalizeProviderMailRu2 }

constructor TPathDetalizeProviderMailRu2.Create;
begin
  inherited Create(
    'http://maps.mail.ru/stamperx/getPath.aspx?mode=time'
  );
end;

{ TPathDetalizeProviderMailRu3 }

constructor TPathDetalizeProviderMailRu3.Create;
begin
  inherited Create(
    'http://maps.mail.ru/stamperx/getPath.aspx?mode=deftime'
  );
end;

end.
