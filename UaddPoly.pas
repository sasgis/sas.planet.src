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
  i_MarksSimple,
  u_MarksSimple,
  u_MarksDbGUIHelper,
  fr_MarkDescription,
  t_GeoTypes;

type
  TFAddPoly = class(TCommonFormParent)
    lblName: TLabel;
    edtName: TEdit;
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
    procedure BaddClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    frMarkDescription: TfrMarkDescription;
    FMarkDBGUI: TMarksDbGUIHelper;
    FCategoryList: TList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function EditMark(AMark: IMarkFull; AMarkDBGUI: TMarksDbGUIHelper): IMarkFull;
    procedure RefreshTranslation; override;
  end;

var
  FAddPoly: TFAddPoly;

implementation

uses
  u_GlobalState,
  u_MarksReadWriteSimple;

{$R *.dfm}

function TFAddPoly.EditMark(AMark: IMarkFull; AMarkDBGUI: TMarksDbGUIHelper): IMarkFull;
var
  VLastUsedCategoryName: string;
  i: Integer;
  VCategory: TCategoryId;
  VId: integer;
  VIndex: Integer;
begin
  FMarkDBGUI := AMarkDBGUI;
  VLastUsedCategoryName:=CBKateg.Text;
  FCategoryList := FMarkDBGUI.MarksDB.CategoryDB.GetCategoriesList;
  try
    FMarkDBGUI.CategoryListToStrings(FCategoryList, CBKateg.Items);
    CBKateg.Sorted:=true;
    CBKateg.Text:=VLastUsedCategoryName;
    edtName.Text:=AMark.name;
    frMarkDescription.Description:=AMark.Desc;
    SEtransp.Value:=100-round(AlphaComponent(AMark.Color1)/255*100);
    SEtransp2.Value:=100-round(AlphaComponent(AMark.Color2)/255*100);
    SpinEdit1.Value:=AMark.Scale1;
    ColorBox1.Selected:=WinColor(AMark.Color1);
    ColorBox2.Selected:=WinColor(AMark.Color2);
    CheckBox2.Checked:=(AMark as IMarkVisible).visible;
    VId := AMark.CategoryId;
    for i := 0 to CBKateg.Items.Count - 1 do begin
      VCategory := TCategoryId(CBKateg.Items.Objects[i]);
      if VCategory <> nil then begin
        if VCategory.id = VId then begin
          CBKateg.ItemIndex := i;
          Break;
        end;
      end;
    end;
    if AMark.id < 0 then begin
      Caption:=SAS_STR_AddNewPoly;
      Badd.Caption:=SAS_STR_Add;
      CheckBox2.Checked:=true;
    end else begin
      Caption:=SAS_STR_EditPoly;
      Badd.Caption:=SAS_STR_Edit;
    end;
    if ShowModal=mrOk then begin
      VCategory := nil;
      VIndex := CBKateg.ItemIndex;
      if VIndex < 0 then begin
        VIndex:= CBKateg.Items.IndexOf(CBKateg.Text);
      end;
      if VIndex >= 0 then begin
        VCategory := TCategoryId(CBKateg.Items.Objects[VIndex]);
      end;
      if VCategory <> nil then begin
        VId := VCategory.id;
      end else begin
        VId := -1;
      end;
      Result := AMarkDBGUI.MarksDB.MarksDb.Factory.CreatePoly(
        edtName.Text,
        CheckBox2.Checked,
        VId,
        frMarkDescription.Description,
        AMark.Points,
        SetAlpha(Color32(ColorBox1.Selected),round(((100-SEtransp.Value)/100)*256)),
        SetAlpha(Color32(ColorBox2.Selected),round(((100-SEtransp2.Value)/100)*256)),
        SpinEdit1.Value,
        AMark
      )
    end else begin
      Result := nil;
    end;
  finally
    FreeAndNil(FCategoryList);
  end;
end;

procedure TFAddPoly.FormShow(Sender: TObject);
begin
  frMarkDescription.Parent := pnlDescription;
  edtName.SetFocus;
end;

procedure TFAddPoly.RefreshTranslation;
begin
  inherited;
  frMarkDescription.RefreshTranslation;
end;

procedure TFAddPoly.BaddClick(Sender: TObject);
var i:integer;
    alltl,allbr:TDoublePoint;
    VPointCount: integer;
    VCategory: TCategoryId;
    VIndex: Integer;
    VId: Integer;
begin
  VCategory := nil;
  VIndex := CBKateg.ItemIndex;
  if VIndex < 0 then begin
    VIndex:= CBKateg.Items.IndexOf(CBKateg.Text);
  end;
  if VIndex >= 0 then begin
    VCategory := TCategoryId(CBKateg.Items.Objects[VIndex]);
  end;
  if VCategory = nil then begin
    VCategory := FMarkDBGUI.AddKategory(CBKateg.Text);
    if VCategory <> nil then begin
      FCategoryList.Add(VCategory);
      FMarkDBGUI.CategoryListToStrings(FCategoryList, CBKateg.Items);
    end;
  end;
  ModalResult:=mrOk;
end;

constructor TFAddPoly.Create(AOwner: TComponent);
begin
  inherited;
  frMarkDescription := TfrMarkDescription.Create(nil);
end;

destructor TFAddPoly.Destroy;
begin
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
