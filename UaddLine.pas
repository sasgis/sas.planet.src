unit UaddLine;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Buttons,
  Classes,
  Controls,
  Forms,
  Dialogs,
  DBClient,
  DB,
  Spin,
  StdCtrls,
  ExtCtrls,
  GR32,
  UGeofun,
  Unit1,
  UResStrings,
  UMarksExplorer,
  t_GeoTypes;

type
  TFaddLine = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Label3: TLabel;
    Label5: TLabel;
    EditName: TEdit;
    EditComment: TMemo;
    Badd: TButton;
    Button2: TButton;
    CheckBox2: TCheckBox;
    ColorBox1: TColorBox;
    SpinEdit1: TSpinEdit;
    OpenDialog1: TOpenDialog;
    SEtransp: TSpinEdit;
    Label4: TLabel;
    ColorDialog1: TColorDialog;
    SpeedButton1: TSpeedButton;
    Label7: TLabel;
    CBKateg: TComboBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BaddClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    function show_(aLL:array of TExtendedPoint;new:boolean):boolean;
  end;

var
  FaddLine: TFaddLine;
  arrLL:PArrLL;
  lenarr:integer;
  new_:boolean;

implementation


{$R *.dfm}
function TFaddLine.show_(aLL:array of TExtendedPoint;new:boolean):boolean;
var
    i:integer;
    namecatbuf:string;
begin
 getmem(arrLL,length(all)*SizeOf(TExtendedPoint));
 lenarr:=length(aLL);
 for i:=0 to lenarr-1 do arrLL^[i]:=aLL[i];
 new_:=new;
 EditComment.Text:='';
 EditName.Text:=SAS_STR_NewPath;
 namecatbuf:=CBKateg.Text;
 CBKateg.Clear;
 Kategory2Strings(CBKateg.Items);
 CBKateg.Text:=namecatbuf;
 if new then begin
              faddLine.Caption:=SAS_STR_AddNewPath;
              Badd.Caption:=SAS_STR_Add;
              CheckBox2.Checked:=true;
              if marshrutcomment<>'' then faddLine.EditComment.Text:=marshrutcomment;
             end
        else begin
              faddLine.Caption:=SAS_STR_EditPath;
              Badd.Caption:=SAS_STR_Edit;
              EditName.Text:=Fmain.CDSmarks.FieldByName('name').AsString;
              EditComment.Text:=Fmain.CDSmarks.FieldByName('descr').AsString;
              SEtransp.Value:=100-round(AlphaComponent(TColor32(Fmain.CDSmarks.FieldByName('Color1').AsInteger))/255*100);
              SpinEdit1.Value:=Fmain.CDSmarks.FieldByName('Scale1').AsInteger;
              ColorBox1.Selected:=WinColor(TColor32(Fmain.CDSmarks.FieldByName('Color1').AsInteger));
              CheckBox2.Checked:=Fmain.CDSmarks.FieldByName('Visible').AsBoolean;
              Fmain.CDSKategory.Locate('id',Fmain.CDSmarkscategoryid.AsInteger,[]);
              CBKateg.Text:=Fmain.CDSKategory.fieldbyname('name').AsString;
             end;
 FaddLine.ShowModal;
 result:=ModalResult=mrOk;
end;

procedure TFaddLine.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 FreeMem(ArrLL);
end;

procedure TFaddLine.BaddClick(Sender: TObject);
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
 Fmain.CDSmarks.FieldByName('Visible').AsBoolean:=CheckBox2.Checked;
 Fmain.CDSmarks.FieldByName('LonL').AsFloat:=alltl.x;
 Fmain.CDSmarks.FieldByName('LatT').AsFloat:=alltl.y;
 Fmain.CDSmarks.FieldByName('LonR').AsFloat:=allbr.x;
 Fmain.CDSmarks.FieldByName('LatB').AsFloat:=allbr.y;
 if not(Fmain.CDSKategory.Locate('name',CBKateg.Text,[]))
  then AddKategory(CBKateg.Text);
 Fmain.CDSmarks.FieldByName('categoryid').AsFloat:=Fmain.CDSKategory.FieldByName('id').AsInteger;
 Fmain.CDSmarks.ApplyRange;
 Fmain.CDSmarks.MergeChangeLog;
 Fmain.CDSmarks.SaveToFile(extractfilepath(paramstr(0))+'marks.sml',dfXMLUTF8);
 close;
 ModalResult:=mrOk;
end;

procedure TFaddLine.Button2Click(Sender: TObject);
begin
 close;
end;

procedure TFaddLine.SpeedButton1Click(Sender: TObject);
begin
 if ColorDialog1.Execute then ColorBox1.Selected:=ColorDialog1.Color;
end;

end.
