unit UaddPoly;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  Spin,
  StdCtrls,
  ExtCtrls,
  Buttons,
  GR32,
  u_CommonFormAndFrameParents,
  UResStrings,
  UMarksExplorer,
  u_MarksSimple,
  fr_MarkDescription,
  t_GeoTypes;

type
  TFAddPoly = class(TCommonFormParent)
    Label1: TLabel;
    EditName: TEdit;
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
    pnlBottomButtons: TPanel;
    flwpnlFill: TFlowPanel;
    pnlFill: TPanel;
    pnlLine: TPanel;
    flwpnlLine: TFlowPanel;
    pnlDescription: TPanel;
    pnlCategory: TPanel;
    pnlName: TPanel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BaddClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FMark: TMarkFull;
    frMarkDescription: TfrMarkDescription;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function EditMark(AMark: TMarkFull):boolean;
    procedure RefreshTranslation; override;
  end;

var
  FAddPoly: TFAddPoly;

implementation

uses
  u_MarksReadWriteSimple;

{$R *.dfm}

function TFAddPoly.EditMark(AMark: TMarkFull): boolean;
var
  namecatbuf:string;
  i: Integer;
  VCategory: TCategoryId;
  VId: integer;
begin
  FMark := AMark;
  frMarkDescription.Description:='';
  EditName.Text:=SAS_STR_NewPoly;
  namecatbuf:=CBKateg.Text;
  Kategory2StringsWithObjects(CBKateg.Items);
  CBKateg.Sorted:=true;
  CBKateg.Text:=namecatbuf;
  if FMark.id < 0 then begin
    Caption:=SAS_STR_AddNewPoly;
    Badd.Caption:=SAS_STR_Add;
    CheckBox2.Checked:=true;
  end else begin
    Caption:=SAS_STR_EditPoly;
    Badd.Caption:=SAS_STR_Edit;
    EditName.Text:=FMark.name;
    frMarkDescription.Description:=FMark.Desc;
    SEtransp.Value:=100-round(AlphaComponent(FMark.Color1)/255*100);
    SEtransp2.Value:=100-round(AlphaComponent(FMark.Color2)/255*100);
    SpinEdit1.Value:=FMark.Scale1;
    ColorBox1.Selected:=WinColor(FMark.Color1);
    ColorBox2.Selected:=WinColor(FMark.Color2);
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

procedure TFAddPoly.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FMark := nil;
end;

procedure TFAddPoly.FormShow(Sender: TObject);
begin
  frMarkDescription.Parent := pnlDescription;
  EditName.SetFocus;
end;

procedure TFAddPoly.RefreshTranslation;
begin
  inherited;
  frMarkDescription.RefreshTranslation;
end;

procedure TFAddPoly.BaddClick(Sender: TObject);
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
  for i:=1 to VPointCount-1 do begin
    if alltl.x>FMark.Points[i].x then alltl.x:=FMark.Points[i].x;
    if alltl.y<FMark.Points[i].y then alltl.y:=FMark.Points[i].y;
    if allbr.x<FMark.Points[i].x then allbr.x:=FMark.Points[i].x;
    if allbr.y>FMark.Points[i].y then allbr.y:=FMark.Points[i].y;
  end;
  FMark.name:=EditName.Text;
  FMark.Desc:=frMarkDescription.Description;
  FMark.Scale1:=SpinEdit1.Value;

  FMark.Color1:=SetAlpha(Color32(ColorBox1.Selected),round(((100-SEtransp.Value)/100)*256));
  FMark.Color2:=SetAlpha(Color32(ColorBox2.Selected),round(((100-SEtransp2.Value)/100)*256));
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

constructor TFAddPoly.Create(AOwner: TComponent);
begin
  inherited;
  frMarkDescription := TfrMarkDescription.Create(nil);
end;

destructor TFAddPoly.Destroy;
var
  i: Integer;
begin
  for i := 0 to CBKateg.Items.Count - 1 do begin
    CBKateg.Items.Objects[i].Free;
  end;
  CBKateg.Items.Clear;
  FreeAndNil(frMarkDescription);
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

end.
