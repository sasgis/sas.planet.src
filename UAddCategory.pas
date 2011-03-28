unit UAddCategory;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  ExtCtrls,
  Spin,
  i_MarkCategory,
  u_CommonFormAndFrameParents,
  u_MarksDbGUIHelper,
  UResStrings;

type
  TFAddCategory = class(TCommonFormParent)
    Label1: TLabel;
    EditName: TEdit;
    CBShow: TCheckBox;
    EditS1: TSpinEdit;
    Label2: TLabel;
    Label3: TLabel;
    EditS2: TSpinEdit;
    Label4: TLabel;
    Bevel5: TBevel;
    btnOk: TButton;
    btnCancel: TButton;
    pnlBottomButtons: TPanel;
    flwpnlZooms: TFlowPanel;
    pnlName: TPanel;
  private
    FMarkDBGUI: TMarksDbGUIHelper;
  public
   function EditCategory(ACategory: IMarkCategory; AMarkDBGUI: TMarksDbGUIHelper): IMarkCategory;
  end;

var
  FAddCategory: TFAddCategory;

implementation

{$R *.dfm}

function TFAddCategory.EditCategory(ACategory: IMarkCategory; AMarkDBGUI: TMarksDbGUIHelper): IMarkCategory;
begin
  FMarkDBGUI := AMarkDBGUI;
  EditName.Text:=SAS_STR_NewPoly;
  if ACategory.IsNew then begin
    FaddCategory.Caption:=SAS_STR_AddNewCategory;
    btnOk.Caption:=SAS_STR_Add;
  end else begin
    FaddCategory.Caption:=SAS_STR_EditCategory;
    btnOk.Caption:=SAS_STR_Edit;
  end;
  EditName.Text:=ACategory.name;
  EditS1.Value:=ACategory.AfterScale;
  EditS2.Value:=ACategory.BeforeScale;
  CBShow.Checked:=ACategory.visible;
  if ShowModal = mrOk then begin
    Result := FMarkDBGUI.MarksDB.CategoryDB.Factory.Modify(
        ACategory,
        EditName.Text,
        CBShow.Checked,
        EditS1.Value,
        EditS2.Value
      );
  end else begin
    Result := nil;
  end;
end;

end.
