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
    btnOk: TButton;
    btnCancel: TButton;
    chkVisible: TCheckBox;
    OpenDialog1: TOpenDialog;
    lblLineColor: TLabel;
    lblLineWidth: TLabel;
    clrbxLineColor: TColorBox;
    seLineWidth: TSpinEdit;
    seLineTransp: TSpinEdit;
    lblLineTransp: TLabel;
    btnLineColor: TSpeedButton;
    lblFillColor: TLabel;
    clrbxFillColor: TColorBox;
    seFillTransp: TSpinEdit;
    lblFillTransp: TLabel;
    btnFillColor: TSpeedButton;
    lblLine: TLabel;
    lblFill: TLabel;
    ColorDialog1: TColorDialog;
    lblCategory: TLabel;
    CBKateg: TComboBox;
    pnlBottomButtons: TPanel;
    flwpnlFill: TFlowPanel;
    pnlFill: TPanel;
    pnlLine: TPanel;
    flwpnlLine: TFlowPanel;
    pnlDescription: TPanel;
    pnlCategory: TPanel;
    pnlName: TPanel;
    procedure btnOkClick(Sender: TObject);
    procedure btnLineColorClick(Sender: TObject);
    procedure btnFillColorClick(Sender: TObject);
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
    seLineTransp.Value:=100-round(AlphaComponent(AMark.Color1)/255*100);
    seFillTransp.Value:=100-round(AlphaComponent(AMark.Color2)/255*100);
    seLineWidth.Value:=AMark.Scale1;
    clrbxLineColor.Selected:=WinColor(AMark.Color1);
    clrbxFillColor.Selected:=WinColor(AMark.Color2);
    chkVisible.Checked:= FMarkDBGUI.MarksDB.MarksDb.GetMarkVisible(AMark);
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
      btnOk.Caption:=SAS_STR_Add;
    end else begin
      Caption:=SAS_STR_EditPoly;
      btnOk.Caption:=SAS_STR_Edit;
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
        chkVisible.Checked,
        VId,
        frMarkDescription.Description,
        AMark.Points,
        SetAlpha(Color32(clrbxLineColor.Selected),round(((100-seLineTransp.Value)/100)*256)),
        SetAlpha(Color32(clrbxFillColor.Selected),round(((100-seFillTransp.Value)/100)*256)),
        seLineWidth.Value,
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

procedure TFAddPoly.btnOkClick(Sender: TObject);
var
  VCategory: TCategoryId;
  VIndex: Integer;
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

procedure TFAddPoly.btnLineColorClick(Sender: TObject);
begin
 if ColorDialog1.Execute then clrbxLineColor.Selected:=ColorDialog1.Color;
end;

procedure TFAddPoly.btnFillColorClick(Sender: TObject);
begin
 if ColorDialog1.Execute then clrbxFillColor.Selected:=ColorDialog1.Color;
end;

end.
