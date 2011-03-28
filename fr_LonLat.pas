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
  u_CommonFormAndFrameParents,
  i_LocalCoordConverter,
  t_GeoTypes;

type
  TfrLonLat = class(TFrame)
    Panel1: TPanel;
    ComboBoxCoordType: TComboBox;
    grdpnlFull: TGridPanel;
    Panel2: TPanel;
    grdpnlMain: TGridPanel;
    lblLat: TLabel;
    lblLon: TLabel;
    EditLat: TEdit;
    EditLon: TEdit;
    pnlZoom: TPanel;
    lblZoom: TLabel;
    cbbZoom: TComboBox;
    procedure ComboBoxCoordTypeSelect(Sender: TObject);
  private
    FCoordinates: TDoublePoint;
    function GetLonLat: TDoublePoint;
    procedure SetLonLat(const Value: TDoublePoint);
    function Edit2Digit(Atext:string; lat:boolean; var res:Double): boolean;
  public
    property LonLat: TDoublePoint read GetLonLat write SetLonLat;
  end;

implementation

uses
  i_ValueToStringConverter,
  u_GeoToStr,
  u_GlobalState,
  UResStrings;

{$R *.dfm}

{ TfrLonLat }
procedure TfrLonLat.ComboBoxCoordTypeSelect(Sender: TObject);
begin
  SetLonLat(FCoordinates);
  case ComboBoxCoordType.ItemIndex of
   0:   begin
          lblLat.Caption:=SAS_STR_Lat+':';
          lblLon.Caption:=SAS_STR_Lon+':';
          pnlZoom.Visible:=false;
        end;
   1,2: begin
          lblLat.Caption:=SAS_STR_OnHorizontal+':';
          lblLon.Caption:=SAS_STR_OnVertical+':';
          pnlZoom.Visible:=true;
        end;
  end;
  grdpnlMain.Realign;
end;

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
  text:=StringReplace(text,'Þ','-',[rfReplaceAll]);
  text:=StringReplace(text,'Ç','-',[rfReplaceAll]);
  text:=StringReplace(text,'Â','+',[rfReplaceAll]);
  text:=StringReplace(text,'Ñ','+',[rfReplaceAll]);

  i:=1;
  while i<=length(text) do begin
    if (not(text[i] in ['0'..'9','-','+','.',',',' '])) then begin
      text[i]:=' ';
      dec(i);
    end;

    if ((i=1)and(text[i]=' '))or
       ((i=length(text))and(text[i]=' '))or
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
     if res<0 then begin
       res:=res-gms/delitel;
     end else begin
       res:=res+gms/delitel;
     end;
     delitel:=delitel*60;
    until (i=0)or(delitel>3600)or(result=false);
  except
    result:=false;
  end;
end;

function TfrLonLat.GetLonLat: TDoublePoint;
var  VLocalConverter: ILocalCoordConverter;
     XYPoint:TPoint;
     XYRect:TRect;
begin
  case ComboBoxCoordType.ItemIndex of
   0: begin
        if not(Edit2Digit(EditLat.Text,true,Result.y))or
           not(Edit2Digit(EditLon.Text,false,Result.x)) then begin
          ShowMessage(SAS_ERR_CoordinatesInput);
        end;
      end;
   1: begin
        try
          XYPoint.X:=strtoint(EditLon.Text);
          XYPoint.Y:=strtoint(EditLat.Text);
        except
          ShowMessage(SAS_ERR_CoordinatesInput);
        end;
        VLocalConverter :=  GState.MainFormConfig.ViewPortState.GetVisualCoordConverter;
        Result:=VLocalConverter.GetGeoConverter.PixelPos2LonLat(XYPoint,cbbZoom.ItemIndex);
      end;
   2: begin
        try
          XYPoint.X:=strtoint(EditLon.Text);
          XYPoint.Y:=strtoint(EditLat.Text);
        except
          ShowMessage(SAS_ERR_CoordinatesInput);
        end;
        VLocalConverter :=  GState.MainFormConfig.ViewPortState.GetVisualCoordConverter;
        XYRect:=VLocalConverter.GetGeoConverter.TilePos2PixelRect(XYPoint,cbbZoom.ItemIndex);
        XYPoint:=Point(XYRect.Right-(XYRect.Right-XYRect.Left)div 2,
                       XYRect.Bottom-(XYRect.Bottom-XYRect.top)div 2);
        Result:=VLocalConverter.GetGeoConverter.PixelPos2LonLat(XYPoint,cbbZoom.ItemIndex);
      end;
  end;
end;

procedure TfrLonLat.SetLonLat(const Value: TDoublePoint);
var
  VValueConverter: IValueToStringConverter;
  XYPoint:TPoint;
  CurrZoom:integer;
  VLocalConverter: ILocalCoordConverter;
begin
  FCoordinates:=Value;
  VValueConverter := GState.ValueToStringConverterConfig.GetStaticConverter;
  CurrZoom:=GState.MainFormConfig.ViewPortState.GetCurrentZoom;
  cbbZoom.ItemIndex:=CurrZoom;
  if ComboBoxCoordType.ItemIndex=-1 then begin
    ComboBoxCoordType.ItemIndex:=0;
  end;

  case ComboBoxCoordType.ItemIndex of
   0: begin
        EditLon.Text:=VValueConverter.LonConvert(Value.x);
        EditLat.Text:=VValueConverter.LatConvert(Value.y);
      end;
   1: begin
        VLocalConverter :=  GState.MainFormConfig.ViewPortState.GetVisualCoordConverter;
        XYPoint:=VLocalConverter.GetGeoConverter.LonLat2PixelPos(Value,CurrZoom);
        EditLon.Text:=inttostr(XYPoint.x);
        EditLat.Text:=inttostr(XYPoint.y);
      end;
   2: begin
        VLocalConverter :=  GState.MainFormConfig.ViewPortState.GetVisualCoordConverter;
        XYPoint:=VLocalConverter.GetGeoConverter.LonLat2TilePos(Value,CurrZoom);
        EditLon.Text:=inttostr(XYPoint.x);
        EditLat.Text:=inttostr(XYPoint.y);
      end;
  end;
end;

end.
