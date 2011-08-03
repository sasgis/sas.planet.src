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
  t_GeoTypes,
  i_ValueToStringConverter,
  i_ViewPortState;

type
  TfrLonLat = class(TFrame)
    pnlTop: TPanel;
    cbbCoordType: TComboBox;
    pnlXY: TPanel;
    grdpnlLonLat: TGridPanel;
    lblLat: TLabel;
    lblLon: TLabel;
    edtLat: TEdit;
    edtLon: TEdit;
    lblZoom: TLabel;
    cbbZoom: TComboBox;
    grdpnlXY: TGridPanel;
    lblY: TLabel;
    edtX: TEdit;
    edtY: TEdit;
    lblX: TLabel;
    grdpnlZoom: TGridPanel;
    procedure cbbCoordTypeSelect(Sender: TObject);
  private
    FCoordinates: TDoublePoint;
    FViewPortState: IViewPortState;
    FValueToStringConverterConfig: IValueToStringConverterConfig;
    function GetLonLat: TDoublePoint;
    procedure SetLonLat(const Value: TDoublePoint);
    function Edit2Digit(Atext:string; lat:boolean; var res:Double): boolean;
  public
    constructor Create(
      AOwner: TComponent;
      AViewPortState: IViewPortState;
      AValueToStringConverterConfig: IValueToStringConverterConfig
    ); reintroduce;
    property LonLat: TDoublePoint read GetLonLat write SetLonLat;
  end;

implementation

uses
  u_GeoToStr,
  u_ResStrings;

{$R *.dfm}

{ TfrLonLat }
procedure TfrLonLat.cbbCoordTypeSelect(Sender: TObject);
begin
  SetLonLat(FCoordinates);
  case cbbCoordType.ItemIndex of
   0:   begin
          pnlXY.Visible := False;
          grdpnlLonLat.Visible := True;
          grdpnlLonLat.Realign;
        end;
   1,2: begin
          pnlXY.Visible := True;
          grdpnlLonLat.Visible := False;
          grdpnlXY.Realign;
        end;
  end;
end;

constructor TfrLonLat.Create(AOwner: TComponent; AViewPortState: IViewPortState;
  AValueToStringConverterConfig: IValueToStringConverterConfig);
begin
  inherited Create(AOwner);
  FViewPortState := AViewPortState;
  FValueToStringConverterConfig := AValueToStringConverterConfig;
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
  case cbbCoordType.ItemIndex of
   0: begin
        if not(Edit2Digit(edtLat.Text,true,Result.y))or
           not(Edit2Digit(edtLon.Text,false,Result.x)) then begin
          ShowMessage(SAS_ERR_CoordinatesInput);
        end;
      end;
   1: begin
        try
          XYPoint.X:=strtoint(edtX.Text);
          XYPoint.Y:=strtoint(edtY.Text);
        except
          ShowMessage(SAS_ERR_CoordinatesInput);
        end;
        VLocalConverter :=  FViewPortState.GetVisualCoordConverter;
        Result:=VLocalConverter.GetGeoConverter.PixelPos2LonLat(XYPoint,cbbZoom.ItemIndex);
      end;
   2: begin
        try
          XYPoint.X:=strtoint(edtX.Text);
          XYPoint.Y:=strtoint(edtY.Text);
        except
          ShowMessage(SAS_ERR_CoordinatesInput);
        end;
        VLocalConverter :=  FViewPortState.GetVisualCoordConverter;
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
  VValueConverter := FValueToStringConverterConfig.GetStaticConverter;
  CurrZoom:=FViewPortState.GetCurrentZoom;
  cbbZoom.ItemIndex:=CurrZoom;
  if cbbCoordType.ItemIndex=-1 then begin
    cbbCoordType.ItemIndex:=0;
  end;

  case cbbCoordType.ItemIndex of
   0: begin
        edtLon.Text:=VValueConverter.LonConvert(Value.x);
        edtLat.Text:=VValueConverter.LatConvert(Value.y);
      end;
   1: begin
        VLocalConverter :=  FViewPortState.GetVisualCoordConverter;
        XYPoint:=VLocalConverter.GetGeoConverter.LonLat2PixelPos(Value,CurrZoom);
        edtX.Text:=inttostr(XYPoint.x);
        edtY.Text:=inttostr(XYPoint.y);
      end;
   2: begin
        VLocalConverter :=  FViewPortState.GetVisualCoordConverter;
        XYPoint:=VLocalConverter.GetGeoConverter.LonLat2TilePos(Value,CurrZoom);
        edtX.Text:=inttostr(XYPoint.x);
        edtY.Text:=inttostr(XYPoint.y);
      end;
  end;
end;

end.
