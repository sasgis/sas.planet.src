{******************************************************************************}
{* SAS.Планета (SAS.Planet)                                                   *}
{* Copyright (C) 2007-2011, авторы программы SAS.Планета (SAS.Planet).        *}
{* Это программа является свободным программным обеспечением. Вы можете       *}
{* распространять и/или модифицировать её согласно условиям Стандартной       *}
{* Общественной Лицензии GNU, опубликованной Фондом Свободного Программного   *}
{* Обеспечения, версии 3. Эта программа распространяется в надежде, что она   *}
{* будет полезной, но БЕЗ ВСЯКИХ ГАРАНТИЙ, в том числе подразумеваемых        *}
{* гарантий ТОВАРНОГО СОСТОЯНИЯ ПРИ ПРОДАЖЕ и ГОДНОСТИ ДЛЯ ОПРЕДЕЛЁННОГО      *}
{* ПРИМЕНЕНИЯ. Смотрите Стандартную Общественную Лицензию GNU версии 3, для   *}
{* получения дополнительной информации. Вы должны были получить копию         *}
{* Стандартной Общественной Лицензии GNU вместе с программой. В случае её     *}
{* отсутствия, посмотрите http://www.gnu.org/licenses/.                       *}
{*                                                                            *}
{* http://sasgis.ru/sasplanet                                                 *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit frm_MarkEditPoly;

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
  u_ResStrings,
  i_MarksSimple,
  i_MarkCategory,
  u_MarksDbGUIHelper,
  fr_MarkDescription,
  t_GeoTypes;

type
  TfrmMarkEditPoly = class(TCommonFormParent)
    lblName: TLabel;
    edtName: TEdit;
    btnOk: TButton;
    btnCancel: TButton;
    chkVisible: TCheckBox;
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
    FCategoryList: IInterfaceList;
    FCategory: ICategory;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function EditMark(AMark: IMarkPoly; AMarkDBGUI: TMarksDbGUIHelper): IMarkPoly;
    procedure RefreshTranslation; override;
  end;

var
  frmMarkEditPoly: TfrmMarkEditPoly;

implementation

{$R *.dfm}

function TfrmMarkEditPoly.EditMark(AMark: IMarkPoly; AMarkDBGUI: TMarksDbGUIHelper): IMarkPoly;
var
  VLastUsedCategoryName: string;
  i: Integer;
  VCategory: ICategory;
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
    seLineTransp.Value:=100-round(AlphaComponent(AMark.BorderColor)/255*100);
    seFillTransp.Value:=100-round(AlphaComponent(AMark.FillColor)/255*100);
    seLineWidth.Value:=AMark.LineWidth;
    clrbxLineColor.Selected:=WinColor(AMark.BorderColor);
    clrbxFillColor.Selected:=WinColor(AMark.FillColor);
    chkVisible.Checked:= FMarkDBGUI.MarksDB.MarksDb.GetMarkVisible(AMark);
    FCategory := AMark.Category;
    if FCategory <> nil then begin
      for i := 0 to CBKateg.Items.Count - 1 do begin
        VCategory := ICategory(Pointer(CBKateg.Items.Objects[i]));
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
      Caption:=SAS_STR_AddNewPoly;
    end else begin
      Caption:=SAS_STR_EditPoly;
    end;
    if ShowModal=mrOk then begin
      Result := AMarkDBGUI.MarksDB.MarksDb.Factory.ModifyPoly(
        AMark,
        edtName.Text,
        chkVisible.Checked,
        FCategory,
        frMarkDescription.Description,
        AMark.Points,
        SetAlpha(Color32(clrbxLineColor.Selected),round(((100-seLineTransp.Value)/100)*256)),
        SetAlpha(Color32(clrbxFillColor.Selected),round(((100-seFillTransp.Value)/100)*256)),
        seLineWidth.Value
      )
    end else begin
      Result := nil;
    end;
  finally
    FCategoryList := nil;
  end;
end;

procedure TfrmMarkEditPoly.FormShow(Sender: TObject);
begin
  frMarkDescription.Parent := pnlDescription;
  edtName.SetFocus;
end;

procedure TfrmMarkEditPoly.RefreshTranslation;
begin
  inherited;
  frMarkDescription.RefreshTranslation;
end;

procedure TfrmMarkEditPoly.btnOkClick(Sender: TObject);
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
    FCategory := ICategory(Pointer(CBKateg.Items.Objects[VIndex]));
  end;
  if FCategory = nil then begin
    FCategory := FMarkDBGUI.AddKategory(VCategoryText);
  end;
  ModalResult := mrOk;
end;

constructor TfrmMarkEditPoly.Create(AOwner: TComponent);
begin
  inherited;
  frMarkDescription := TfrMarkDescription.Create(nil);
end;

destructor TfrmMarkEditPoly.Destroy;
begin
  FreeAndNil(frMarkDescription);
  inherited;
end;

procedure TfrmMarkEditPoly.btnLineColorClick(Sender: TObject);
begin
 if ColorDialog1.Execute then clrbxLineColor.Selected:=ColorDialog1.Color;
end;

procedure TfrmMarkEditPoly.btnFillColorClick(Sender: TObject);
begin
 if ColorDialog1.Execute then clrbxFillColor.Selected:=ColorDialog1.Color;
end;

end.
