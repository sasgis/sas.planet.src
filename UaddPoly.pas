unit UaddPoly;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Spin,
  StdCtrls,
  ExtCtrls,
  Buttons,
  DB,
  GR32,
  TB2Item,
  TBX,
  TB2Dock,
  TB2Toolbar,
  Unit1,
  UResStrings,
  UMarksExplorer,
  t_GeoTypes;

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
    Label7: TLabel;
    CBKateg: TComboBox;
    TBXToolbar1: TTBXToolbar;
    TBXItem3: TTBXItem;
    TBXItem2: TTBXItem;
    TBXItem1: TTBXItem;
    TBXSeparatorItem1: TTBXSeparatorItem;
    TBXItem4: TTBXItem;
    TBXItem5: TTBXItem;
    TBXItem6: TTBXItem;
    TBXSeparatorItem2: TTBXSeparatorItem;
    TBXItem7: TTBXItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BaddClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure TBXItem3Click(Sender: TObject);
    procedure EditCommentKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FarrLL:TExtendedPointArray;
    new_:boolean;
  public
    destructor Destroy; override;
   function show_(aLL:TExtendedPointArray;new:boolean):boolean;
  end;

  TEditBtn = (ebB,ebI,ebU,ebLeft,ebCenter,ebRight,ebImg);

var
  FAddPoly: TFAddPoly;

implementation

{$R *.dfm}

function TFAddPoly.show_(aLL:TExtendedPointArray;new:boolean):boolean;
var
    namecatbuf:string;
  i: Integer;
  VCategory: TCategoryId;
  VId: integer;
begin
 if new  then Fmain.CDSmarks.Insert
         else Fmain.CDSmarks.Edit;
 FarrLL := Copy(aLL);
 new_:=new;
 EditComment.Text:='';
 EditName.Text:=SAS_STR_NewPoly;
 namecatbuf:=CBKateg.Text;
 Kategory2StringsWithObjects(CBKateg.Items);
 CBKateg.Sorted:=true;
 CBKateg.Text:=namecatbuf;
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
              VId := Fmain.CDSmarkscategoryid.AsInteger;
              for i := 0 to CBKateg.Items.Count - 1 do begin
                VCategory := TCategoryId(CBKateg.Items.Objects[i]);
                if VCategory <> nil then begin
                  if VCategory.id = VId then begin
                    CBKateg.ItemIndex := i;
                    Break;
                  end;
                end;
              end;
             end;
  FaddPoly.ShowModal;
  result:=ModalResult=mrOk;
end;

procedure TFAddPoly.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Fmain.CDSmarks.Cancel;
 FarrLL := nil;
end;

procedure TFAddPoly.BaddClick(Sender: TObject);
var i:integer;
    alltl,allbr:TExtendedPoint;
    VPointCount: integer;
    VCategory: TCategoryId;
begin
 alltl:=FarrLL[0];
 allbr:=FarrLL[0];
 VPointCount := Length(FarrLL);
 for i:=1 to VPointCount-1 do
  begin
   if alltl.x>FarrLL[i].x then alltl.x:=FarrLL[i].x;
   if alltl.y<FarrLL[i].y then alltl.y:=FarrLL[i].y;
   if allbr.x<FarrLL[i].x then allbr.x:=FarrLL[i].x;
   if allbr.y>FarrLL[i].y then allbr.y:=FarrLL[i].y;
  end;
 Fmain.CDSmarks.FieldByName('name').AsString:=EditName.Text;
 Fmain.CDSmarks.FieldByName('descr').AsString:=EditComment.Text;
 BlobFromExtArr(FarrLL, Fmain.CDSmarks.FieldByName('LonLatArr'));
 Fmain.CDSmarks.FieldByName('Scale1').AsInteger:=SpinEdit1.Value;

 Fmain.CDSmarks.FieldByName('Color1').AsFloat:=SetAlpha(Color32(ColorBox1.Selected),round(((100-SEtransp.Value)/100)*256));
 Fmain.CDSmarks.FieldByName('Color2').AsFloat:=SetAlpha(Color32(ColorBox2.Selected),round(((100-SEtransp2.Value)/100)*256));
 Fmain.CDSmarks.FieldByName('Visible').AsBoolean:=CheckBox2.Checked;
 Fmain.CDSmarks.FieldByName('LonL').AsFloat:=alltl.x;
 Fmain.CDSmarks.FieldByName('LatT').AsFloat:=alltl.y;
 Fmain.CDSmarks.FieldByName('LonR').AsFloat:=allbr.x;
 Fmain.CDSmarks.FieldByName('LatB').AsFloat:=allbr.y;
 if CBKateg.ItemIndex < 0 then begin
  Fmain.CDSmarks.FieldByName('categoryid').AsInteger := AddKategory(CBKateg.Text);
 end else begin
  VCategory := TCategoryId(CBKateg.Items.Objects[CBKateg.ItemIndex]);
  if VCategory <> nil then begin
    Fmain.CDSmarks.FieldByName('categoryid').AsInteger := VCategory.id;
  end else begin
    Fmain.CDSmarks.FieldByName('categoryid').AsFloat:=AddKategory(CBKateg.Text);
  end;
 end;
 Fmain.CDSmarks.Post;
 SaveMarks2File;
 close;
 ModalResult:=mrOk;
end;

procedure TFAddPoly.Button2Click(Sender: TObject);
begin
 close;
end;

destructor TFAddPoly.Destroy;
var
  i: Integer;
begin
  for i := 0 to CBKateg.Items.Count - 1 do begin
    CBKateg.Items.Objects[i].Free;
  end;
  CBKateg.Items.Clear;
  inherited;
end;

procedure TFAddPoly.SpeedButton1Click(Sender: TObject);
begin
 if ColorDialog1.Execute then ColorBox1.Selected:=ColorDialog1.Color;
end;

procedure TFAddPoly.SpeedButton2Click(Sender: TObject);
begin
 if ColorDialog1.Execute then ColorBox2.Selected:=ColorDialog1.Color;
end;

procedure TFAddPoly.TBXItem3Click(Sender: TObject);
var s:string;
    seli:integer;
begin
 s:=EditComment.Text;
 seli:=EditComment.SelStart;
 case TEditBtn(TTBXItem(sender).Tag) of
  ebB: begin
        Insert('<b>',s,EditComment.SelStart+1);
        Insert('</b>',s,EditComment.SelStart+EditComment.SelLength+3+1);
       end;
  ebI: begin
        Insert('<i>',s,EditComment.SelStart+1);
        Insert('</i>',s,EditComment.SelStart+EditComment.SelLength+3+1);
       end;
  ebU: begin
        Insert('<u>',s,EditComment.SelStart+1);
        Insert('</u>',s,EditComment.SelStart+EditComment.SelLength+3+1);
       end;
  ebImg:
       begin
        if (FMain.OpenPictureDialog.Execute)and(FMain.OpenPictureDialog.FileName<>'') then begin
         Insert('<img src="'+FMain.OpenPictureDialog.FileName+'"/>',s,EditComment.SelStart+1);
        end;
       end;
  ebCenter:
       begin
        Insert('<CENTER>',s,EditComment.SelStart+1);
        Insert('</CENTER>',s,EditComment.SelStart+EditComment.SelLength+8+1);
       end;
  ebLeft:
       begin
        Insert('<div ALIGN=LEFT>',s,EditComment.SelStart+1);
        Insert('</div>',s,EditComment.SelStart+EditComment.SelLength+16+1);
       end;
  ebRight:
       begin
        Insert('<div ALIGN=RIGHT>',s,EditComment.SelStart+1);
        Insert('</div>',s,EditComment.SelStart+EditComment.SelLength+17+1);
       end;
 end;
 EditComment.Text:=s;
 EditComment.SelStart:=seli;
end;

procedure TFAddPoly.EditCommentKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var s:string;
    seli:integer;
begin
 if Key=13 then begin
   Key:=0;
   s:=EditComment.Text;
   seli:=EditComment.SelStart;
   Insert('<BR>',s,EditComment.SelStart+1);
   EditComment.Text:=s;
   EditComment.SelStart:=seli+4;
 end;
end;

end.
