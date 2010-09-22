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
  Spin,
  StdCtrls,
  ExtCtrls,
  TB2Item,
  TBX,
  TB2Dock,
  TB2Toolbar,
  GR32,
  u_CommonFormAndFrameParents,
  Unit1,
  UResStrings,
  UMarksExplorer,
  u_MarksSimple,
  t_GeoTypes;

type
  TFaddLine = class(TCommonFormParent)
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
    procedure TBXItem3Click(Sender: TObject);
    procedure EditCommentKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FMark: TMarkFull;
  public
    destructor Destroy; override;
    function EditMark(AMark: TMarkFull):boolean;
  end;

  TEditBtn = (ebB,ebI,ebU,ebLeft,ebCenter,ebRight,ebImg);

var
  FaddLine: TFaddLine;

implementation

uses
  u_MarksReadWriteSimple;
  
{$R *.dfm}

function TFaddLine.EditMark(AMark: TMarkFull): boolean;
var
  namecatbuf:string;
  i: Integer;
  VCategory: TCategoryId;
  VId: integer;
begin
  FMark := AMark;
  EditComment.Text:='';
  EditName.Text:=SAS_STR_NewPath;
  namecatbuf:=CBKateg.Text;
  Kategory2StringsWithObjects(CBKateg.Items);
  CBKateg.Sorted:=true;
  CBKateg.Text:=namecatbuf;
  if FMark.id < 0 then begin
    Caption:=SAS_STR_AddNewPath;
    Badd.Caption:=SAS_STR_Add;
    CheckBox2.Checked:=true;
    if FMark.Desc<>'' then faddLine.EditComment.Text:=FMark.Desc;
  end else begin
    Caption:=SAS_STR_EditPath;
    Badd.Caption:=SAS_STR_Edit;
    EditName.Text:=FMark.name;
    EditComment.Text:=FMark.Desc;
    SEtransp.Value:=100-round(AlphaComponent(FMark.Color1)/255*100);
    SpinEdit1.Value:=FMark.Scale1;
    ColorBox1.Selected:=WinColor(FMark.Color1);
    CheckBox2.Checked:=FMark.visible;
    VId := FMark.CategoryId;
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
  result:= ShowModal=mrOk;
end;

procedure TFaddLine.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FMark := nil;
end;

procedure TFaddLine.BaddClick(Sender: TObject);
var i:integer;
    alltl,allbr:TExtendedPoint;
    VPointCount: integer;
    VCategory: TCategoryId;
    VIndex: Integer;
    VId: Integer;
begin
 alltl:=FMark.Points[0];
 allbr:=FMark.Points[0];
 VPointCount := Length(FMark.Points);
 for i:=1 to VPointCount-1 do
  begin
   if alltl.x>FMark.Points[i].x then alltl.x:=FMark.Points[i].x;
   if alltl.y<FMark.Points[i].y then alltl.y:=FMark.Points[i].y;
   if allbr.x<FMark.Points[i].x then allbr.x:=FMark.Points[i].x;
   if allbr.y>FMark.Points[i].y then allbr.y:=FMark.Points[i].y;
  end;
  FMark.name:=EditName.Text;
  FMark.Desc:=EditComment.Text;
  FMark.Scale1:=SpinEdit1.Value;
  FMark.Color1:=SetAlpha(Color32(ColorBox1.Selected),round(((100-SEtransp.Value)/100)*256));
  FMark.visible:=CheckBox2.Checked;
  FMark.LLRect.TopLeft := alltl;
  FMark.LLRect.BottomRight := allbr;
  VIndex := CBKateg.ItemIndex;
  if VIndex < 0 then begin
    VIndex:= CBKateg.Items.IndexOf(CBKateg.Text);
  end;
  if VIndex < 0 then begin
    VId := AddKategory(CBKateg.Text);
  end else begin
    VCategory := TCategoryId(CBKateg.Items.Objects[VIndex]);
    if VCategory <> nil then begin
      VId := VCategory.id;
    end else begin
      VId := AddKategory(CBKateg.Text);
    end;
  end;
  FMark.CategoryId := VId;
  ModalResult:=mrOk;
end;

procedure TFaddLine.Button2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

destructor TFaddLine.Destroy;
var
  i: Integer;
begin
  for i := 0 to CBKateg.Items.Count - 1 do begin
    CBKateg.Items.Objects[i].Free;
  end;
  CBKateg.Items.Clear;
  inherited;
end;

procedure TFaddLine.SpeedButton1Click(Sender: TObject);
begin
 if ColorDialog1.Execute then ColorBox1.Selected:=ColorDialog1.Color;
end;

procedure TFaddLine.TBXItem3Click(Sender: TObject);
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
       if (FMain.OpenPictureDialog.Execute)and(FMain.OpenPictureDialog.FileName<>'') then begin
         Insert('<img src="'+FMain.OpenPictureDialog.FileName+'"/>',s,EditComment.SelStart+1);
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

procedure TFaddLine.EditCommentKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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
