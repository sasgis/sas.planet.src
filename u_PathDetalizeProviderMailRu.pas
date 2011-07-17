unit u_PathDetalizeProviderMailRu;

interface

uses
  t_GeoTypes,
  i_LanguageManager,
  u_PathDetalizeProviderListEntity,
  i_PathDetalizeProvider;

type
  TPathDetalizeProviderMailRu = class(TPathDetalizeProviderListEntity)
  private
    FBaseUrl: string;
    function SecondToTime(const Seconds: Cardinal): Double;
  protected { IPathDetalizeProvider }
    function GetPath(ASource: TArrayOfDoublePoint; var AComment: string): TArrayOfDoublePoint; override;
  public
    constructor Create(
      AGUID: TGUID;
      ALanguageManager: ILanguageManager;
      ABaseUrl: string
    );
  end;

type
  TPathDetalizeProviderMailRuShortest = class(TPathDetalizeProviderMailRu)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;
  public
    constructor Create(
      ALanguageManager: ILanguageManager
    );
  end;

type
  TPathDetalizeProviderMailRuFastest = class(TPathDetalizeProviderMailRu)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;
  public
    constructor Create(
      ALanguageManager: ILanguageManager
    );
  end;

type
  TPathDetalizeProviderMailRuFastestWithTraffic = class(TPathDetalizeProviderMailRu)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;
  public
    constructor Create(
      ALanguageManager: ILanguageManager
    );
  end;

implementation

uses
  Classes,
  SysUtils,
  StrUtils,
  DateUtils,
  gnugettext,
  c_PathDetalizeProvidersGUID,
  u_GeoToStr,
  u_ResStrings,
  frm_InvisibleBrowser;

{ TPathDetalizeProviderMailRu }

constructor TPathDetalizeProviderMailRu.Create(
  AGUID: TGUID;
  ALanguageManager: ILanguageManager;
  ABaseUrl: string
);
begin
  inherited Create(AGUID, ALanguageManager);
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

{ TPathDetalizeProviderMailRuShortest }

constructor TPathDetalizeProviderMailRuShortest.Create(
      ALanguageManager: ILanguageManager
);
begin
  inherited Create(
    CPathDetalizeProviderMailRuShortest,
    ALanguageManager,
    'http://maps.mail.ru/stamperx/getPath.aspx?mode=distance'
  );
end;

function TPathDetalizeProviderMailRuShortest.GetCaptionTranslated: string;
begin
  Result := _('On car (Shortest) by Maps@mail.ru');
end;

function TPathDetalizeProviderMailRuShortest.GetDescriptionTranslated: string;
begin
  Result := _('Detalize route on car (Shortest) by Maps@mail.ru');
end;

function TPathDetalizeProviderMailRuShortest.GetMenuItemNameTranslated: string;
begin
  Result := _('On car (Shortest)');
end;

{ TPathDetalizeProviderMailRuFastest }

constructor TPathDetalizeProviderMailRuFastest.Create(
      ALanguageManager: ILanguageManager
);
begin
  inherited Create(
    CPathDetalizeProviderMailRuFastest,
    ALanguageManager,
    'http://maps.mail.ru/stamperx/getPath.aspx?mode=time'
  );
end;

function TPathDetalizeProviderMailRuFastest.GetCaptionTranslated: string;
begin
  Result := _('On car (Fastest) by Maps@mail.ru');
end;

function TPathDetalizeProviderMailRuFastest.GetDescriptionTranslated: string;
begin
  Result := _('Detalize route on car (Fastest) by Maps@mail.ru');
end;

function TPathDetalizeProviderMailRuFastest.GetMenuItemNameTranslated: string;
begin
  Result := _('On car (Fastest)');
end;

{ TPathDetalizeProviderMailRuFastestWithTraffic }

constructor TPathDetalizeProviderMailRuFastestWithTraffic.Create(
      ALanguageManager: ILanguageManager
);
begin
  inherited Create(
    CPathDetalizeProviderMailRuFastestWithTraffic,
    ALanguageManager,
    'http://maps.mail.ru/stamperx/getPath.aspx?mode=deftime'
  );
end;

function TPathDetalizeProviderMailRuFastestWithTraffic.GetCaptionTranslated: string;
begin
  Result := _('On car (Fastest with traffic) by Maps@mail.ru');
end;

function TPathDetalizeProviderMailRuFastestWithTraffic.GetDescriptionTranslated: string;
begin
  Result := _('Detalize route on car (Fastest with traffic) by Maps@mail.ru');
end;

function TPathDetalizeProviderMailRuFastestWithTraffic.GetMenuItemNameTranslated: string;
begin
  Result := _('On car (Fastest with traffic)');
end;

end.
