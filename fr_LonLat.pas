unit fr_LonLat;

interface

uses
  Windows,
  Messages,
  SysUtils,
  StrUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  Mask,
  u_CommonFormAndFrameParents,
  t_GeoTypes;

type
  TfrLonLat = class(TFrame)
    lblLat: TLabel;
    lblLon: TLabel;
    flwpnlLat: TFlowPanel;
    flwpnlLon: TFlowPanel;
    grdpnlMain: TGridPanel;
    EditLat: TEdit;
    EditLon: TEdit;
  private
    function GetLonLat: TDoublePoint;
    procedure SetLonLat(const Value: TDoublePoint);
    function Edit2Digit(Atext:string; lat:boolean; var res:Double): boolean;
  public
    property LonLat: TDoublePoint read GetLonLat write SetLonLat;
  end;

implementation

uses
  Ugeofun,
  u_GeoToStr;

{$R *.dfm}

{ TfrLonLat }
function TfrLonLat.Edit2Digit(Atext:string; lat:boolean; var res:Double): boolean;
var i,delitel:integer;
    gms:double;
    text:string;
begin
  result:=true;
  res:=0;
  text:=Atext;

  text:=StringReplace(text,'S','-',[rfReplaceAll]);
  text:=StringReplace(text,'W','-',[rfReplaceAll]);
  text:=StringReplace(text,'N','+',[rfReplaceAll]);
  text:=StringReplace(text,'E','+',[rfReplaceAll]);
  text:=StringReplace(text,'Ю','-',[rfReplaceAll]);
  text:=StringReplace(text,'З','-',[rfReplaceAll]);
  text:=StringReplace(text,'В','+',[rfReplaceAll]);
  text:=StringReplace(text,'С','+',[rfReplaceAll]);

  i:=1;
  while i<=length(text) do begin
    if (not(text[i] in ['0'..'9','-','+','.',',',' ']))or
       ((i=1)and(text[i]=' '))or
       ((i<length(text)-1)and(text[i]=' ')and(text[i+1]=' '))or
       ((i>1) and (text[i]=' ') and (not(text[i-1] in ['0'..'9'])))or
       ((i<length(text)-1)and(text[i]=',')and(text[i+1]=' ')) then begin
      Delete(text,i,1);
      dec(i);
    end;
    inc(i);
  end;

  try
    res:=0;
    delitel:=1;
    repeat
     i:=posEx(' ',text,1);
     if i=0 then begin
       gms:=str2r(text);
     end else begin
       gms:=str2r(copy(text,1,i-1));
       Delete(text,1,i);
     end;
     if ((delitel>1)and(abs(gms)>60))or
        ((delitel=1)and(lat)and(abs(gms)>90))or
        ((delitel=1)and(not lat)and(abs(gms)>180)) then begin
       Result:=false;
     end;
     res:=res+gms/delitel;
     delitel:=delitel*60;
    until (i=0)or(delitel>3600)or(result=false);
  except
    result:=false;
  end;
end;

function TfrLonLat.GetLonLat: TDoublePoint;
begin
  if not(Edit2Digit(EditLat.Text,true,Result.y)) then begin
    ShowMessage('Неверный формат ввода широты!');
  end;
  if not(Edit2Digit(EditLon.Text,false,Result.x)) then begin
    ShowMessage('Неверный формат ввода широты!');
  end;
end;

procedure TfrLonLat.SetLonLat(const Value: TDoublePoint);
begin
  EditLat.Text:=RoundEx(Value.y,6);
  EditLon.Text:=RoundEx(Value.x,6);
end;

end.
