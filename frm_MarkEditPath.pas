unit frm_MarkEditPath;

interface

uses
  Windows,
  SysUtils,
  Buttons,
  Classes,
  Controls,
  Forms,
  Dialogs,
  Spin,
  StdCtrls,
  ExtCtrls,
  GR32,
  u_CommonFormAndFrameParents,
  u_ResStrings,
  i_MarksSimple,
  i_MarkCategory,
  u_MarksDbGUIHelper,
  fr_MarkDescription,
  t_GeoTypes;

type
  TfrmMarkEditPath = class(TCommonFormParent)
    lblName: TLabel;
    lblLineColor: TLabel;
    lblWidth: TLabel;
    edtName: TEdit;
    btnOk: TButton;
    btnCancel: TButton;
    chkVisible: TCheckBox;
    clrbxLineColor: TColorBox;
    seWidth: TSpinEdit;
    SEtransp: TSpinEdit;
    lblTransp: TLabel;
    ColorDialog1: TColorDialog;
    btnLineColor: TSpeedButton;
    lblCategory: TLabel;
    CBKateg: TComboBox;
    pnlCategory: TPanel;
    pnlName: TPanel;
    pnlDescription: TPanel;
    flwpnlStyle: TFlowPanel;
    pnlBottomButtons: TPanel;
    procedure btnOkClick(Sender: TObject);
    procedure btnLineColorClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    frMarkDescription: TfrMarkDescription;
    FMarkDBGUI: TMarksDbGUIHelper;
    FCategoryList: IInterfaceList;
    FCategory: IMarkCategory;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function EditMark(AMark: IMarkLine; AMarkDBGUI: TMarksDbGUIHelper): IMarkLine;
    procedure RefreshTranslation; override;
  end;

var
  frmMarkEditPath: TfrmMarkEditPath;

implementation

{$R *.dfm}

function TfrmMarkEditPath.EditMark(AMark: IMarkLine; AMarkDBGUI: TMarksDbGUIHelper): IMarkLine;
var
  VLastUsedCategoryName: string;
  i: Integer;
  VCategory: IMarkCategory;
begin
  FMarkDBGUI := AMarkDBGUI;
  VLastUsedCategoryName:=CBKateg.Text;
  FCategoryList := FMarkDBGUI.MarksDB.CategoryDB.GetCategoriesList;
  try
    FMarkDBGUI.CategoryListToStrings(FCategoryList, CBKateg.Items);
    CBKateg.Sorted:=true;
    CBKateg.Text:=VLastUsedCategoryName;
    edtName.Text:=AMark.name;
    frMarkDescription.Description := AMark.Desc;
    SEtransp.Value:=100-round(AlphaComponent(AMark.LineColor)/255*100);
    seWidth.Value:=AMark.Scale1;
    clrbxLineColor.Selected:=WinColor(AMark.LineColor);
    chkVisible.Checked:= FMarkDBGUI.MarksDB.MarksDb.GetMarkVisible(AMark);
    FCategory := AMark.Category;
    if FCategory <> nil then begin
      for i := 0 to CBKateg.Items.Count - 1 do begin
        VCategory := IMarkCategory(Pointer(CBKateg.Items.Objects[i]));
        if VCategory <> nil then begin
          if VCategory.IsSame(FCategory) then begin
            CBKateg.ItemIndex := i;
            Break;
          end;
        end;
      end;
    end else begin
      CBKateg.ItemIndex := -1;
    end;
    if AMark.IsNew then begin
      Caption:=SAS_STR_AddNewPath;
    end else begin
      Caption:=SAS_STR_EditPath;
    end;
    if ShowModal=mrOk then begin
      Result := AMarkDBGUI.MarksDB.MarksDb.Factory.ModifyLine(
        AMark,
        edtName.Text,
        chkVisible.Checked,
        FCategory,
        frMarkDescription.Description,
        AMark.Points,
        SetAlpha(Color32(clrbxLineColor.Selected),round(((100-SEtransp.Value)/100)*256)),
        seWidth.Value
      );
    end else begin
      Result := nil;
    end;
  finally
    FCategoryList := nil;
  end;
end;

procedure TfrmMarkEditPath.FormShow(Sender: TObject);
begin
  frMarkDescription.Parent := pnlDescription;
  edtName.SetFocus;
end;

procedure TfrmMarkEditPath.RefreshTranslation;
begin
  inherited;
  frMarkDescription.RefreshTranslation;
end;

procedure TfrmMarkEditPath.btnOkClick(Sender: TObject);
var
  VIndex: Integer;
  VCategoryText: string;
begin
  FCategory := nil;
  VCategoryText := CBKateg.Text;
  VIndex := CBKateg.ItemIndex;
  if VIndex < 0 then begin
    VIndex:= CBKateg.Items.IndexOf(VCategoryText);
  end;
  if VIndex >= 0 then begin
    FCategory := IMarkCategory(Pointer(CBKateg.Items.Objects[VIndex]));
  end;
  if FCategory = nil then begin
    FCategory := FMarkDBGUI.AddKategory(VCategoryText);
  end;
  ModalResult := mrOk;
end;

constructor TfrmMarkEditPath.Create(AOwner: TComponent);
begin
  inherited;
  frMarkDescription := TfrMarkDescription.Create(nil);
end;

destructor TfrmMarkEditPath.Destroy;
begin
  FreeAndNil(frMarkDescription);
  inherited;
end;

procedure TfrmMarkEditPath.btnLineColorClick(Sender: TObject);
begin
 if ColorDialog1.Execute then clrbxLineColor.Selected:=ColorDialog1.Color;
end;

end.
