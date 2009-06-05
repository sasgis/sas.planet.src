unit UaddPoly;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Spin, StdCtrls, ExtCtrls, Buttons, UGeoFun, GR32, DB, Unit1, UResStrings;

type
  TFAddPoly = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    EditName: TEdit;
    EditComment: TMemo;
    Badd: TButton;
    Button2: TButton;
    CheckBox2: TCheckBox;
    OpenDialog1: TOpenDialog;
    Label3: TLabel;
    Label5: TLabel;
    ColorBox1: TColorBox;
    SpinEdit1: TSpinEdit;
    SEtransp: TSpinEdit;
    Label4: TLabel;
    SpeedButton1: TSpeedButton;
    Label6: TLabel;
    ColorBox2: TColorBox;
    SEtransp2: TSpinEdit;
    Label8: TLabel;
    SpeedButton2: TSpeedButton;
    Label9: TLabel;
    Label10: TLabel;
    ColorDialog1: TColorDialog;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BaddClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    { Private declarations }
  public
   procedure show_(Sender:TObject;aLL:array of TExtendedPoint;new:boolean);
  end;

var
  FAddPoly: TFAddPoly;
  arrLL:PArrLL;
  lenarr:integer;
  new_:boolean;
  Sender_: TObject;

implementation
{$R *.dfm}

procedure TFAddPoly.show_(Sender:TObject;aLL:array of TExtendedPoint;new:boolean);
var DMS:TDMS;
    i:integer;
begin
 lenarr:=length(aLL);
 if (aLL[0].x<>aLL[length(all)-1].x)or(aLL[0].y<>aLL[length(all)-1].y)
  then begin
        getmem(arrLL,(length(all)+1)*SizeOf(TExtendedPoint));
        for i:=0 to lenarr-1 do arrLL^[i]:=aLL[i];
        arrLL^[lenarr]:=aLL[0];
        inc(lenarr);
       end
  else begin
        getmem(arrLL,length(all)*SizeOf(TExtendedPoint));
        for i:=0 to lenarr-1 do arrLL^[i]:=aLL[i];
       end;
 Sender_:=Sender;
 new_:=new;
 EditComment.Text:='';
 EditName.Text:=SAS_STR_NewPoly;
 TForm(sender).Enabled := false;
 FaddPoly.Visible:=true;
 if new then begin
              faddPoly.Caption:=SAS_STR_AddNewPoly;
              Badd.Caption:=SAS_STR_Add;
              CheckBox2.Checked:=true;
             end
        else begin
              faddPoly.Caption:=SAS_STR_EditPoly;
              Badd.Caption:=SAS_STR_Edit;
              EditName.Text:=Fmain.CDSmarks.FieldByName('name').AsString;
              EditComment.Text:=Fmain.CDSmarks.FieldByName('descr').AsString;
              SEtransp.Value:=100-round(AlphaComponent(TColor32(Fmain.CDSmarks.FieldByName('Color1').AsInteger))/255*100);
              SEtransp2.Value:=100-round(AlphaComponent(TColor32(Fmain.CDSmarks.FieldByName('Color2').AsInteger))/255*100);
              SpinEdit1.Value:=Fmain.CDSmarks.FieldByName('Scale1').AsInteger;
              ColorBox1.Selected:=WinColor(TColor32(Fmain.CDSmarks.FieldByName('Color1').AsInteger));
              ColorBox2.Selected:=WinColor(TColor32(Fmain.CDSmarks.FieldByName('Color2').AsInteger));
              CheckBox2.Checked:=Fmain.CDSmarks.FieldByName('Visible').AsBoolean;
             end;
end;

procedure TFAddPoly.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 FreeMem(ArrLL);
 TForm(sender_).Enabled := true;
end;

procedure TFAddPoly.BaddClick(Sender: TObject);
var i:integer;
    ms:TMemoryStream;
    alltl,allbr:TExtendedPoint;
begin
 if new_ then Fmain.CDSmarks.Insert
         else Fmain.CDSmarks.Edit;
 alltl:=arrLL^[0];
 allbr:=arrLL^[0];

 for i:=1 to lenarr-1 do
  begin
   if alltl.x>arrLL^[i].x then alltl.x:=arrLL^[i].x;
   if alltl.y<arrLL^[i].y then alltl.y:=arrLL^[i].y;
   if allbr.x<arrLL^[i].x then allbr.x:=arrLL^[i].x;
   if allbr.y>arrLL^[i].y then allbr.y:=arrLL^[i].y;
  end;
 Fmain.CDSmarks.FieldByName('name').AsString:=EditName.Text;
 Fmain.CDSmarks.FieldByName('descr').AsString:=EditComment.Text;
 ms:=TMemoryStream.Create;
 ms.WriteBuffer(arrLL^,lenarr*24);
 TBlobField(Fmain.CDSmarks.FieldByName('LonLatArr')).LoadFromStream(ms);
 ms.free;
 Fmain.CDSmarks.FieldByName('Scale1').AsInteger:=SpinEdit1.Value;

 Fmain.CDSmarks.FieldByName('Color1').AsFloat:=SetAlpha(Color32(ColorBox1.Selected),round(((100-SEtransp.Value)/100)*256));
 Fmain.CDSmarks.FieldByName('Color2').AsFloat:=SetAlpha(Color32(ColorBox2.Selected),round(((100-SEtransp2.Value)/100)*256));
 Fmain.CDSmarks.FieldByName('Visible').AsBoolean:=CheckBox2.Checked;
 Fmain.CDSmarks.FieldByName('LonL').AsFloat:=alltl.x;
 Fmain.CDSmarks.FieldByName('LatT').AsFloat:=alltl.y;
 Fmain.CDSmarks.FieldByName('LonR').AsFloat:=allbr.x;
 Fmain.CDSmarks.FieldByName('LatB').AsFloat:=allbr.y;
 Fmain.CDSmarks.ApplyRange;
 Fmain.CDSmarks.MergeChangeLog;
 Fmain.CDSmarks.SaveToFile(extractfilepath(paramstr(0))+'marks.xml');
 close;
 if aoper=add_Poly then Fmain.setalloperationfalse(movemap);
 Fmain.generate_im(nilLastLoad,'');
end;

procedure TFAddPoly.Button2Click(Sender: TObject);
begin
 Fmain.generate_im(nilLastLoad,'');
 close;
end;

procedure TFAddPoly.SpeedButton1Click(Sender: TObject);
begin
 if ColorDialog1.Execute then ColorBox1.Selected:=ColorDialog1.Color;
end;

procedure TFAddPoly.SpeedButton2Click(Sender: TObject);
begin
 if ColorDialog1.Execute then ColorBox2.Selected:=ColorDialog1.Color;
end;

end.
