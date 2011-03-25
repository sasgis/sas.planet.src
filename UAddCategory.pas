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
  i_IMarkCategory,
  u_CommonFormAndFrameParents,
  u_MarksSimple,
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
    Badd: TButton;
    Button2: TButton;
    pnlBottomButtons: TPanel;
    flwpnlZooms: TFlowPanel;
    pnlName: TPanel;
    procedure BaddClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
  public
   function EditCategory(ACategory: IMarkCategory): IMarkCategory;
  end;

var
  FAddCategory: TFAddCategory;

implementation

uses
  u_MarkCategory;

{$R *.dfm}

function TFAddCategory.EditCategory(ACategory: IMarkCategory): IMarkCategory;
begin
  EditName.Text:=SAS_STR_NewPoly;
  if ACategory.id < 0 then begin
    EditName.Text:=SAS_STR_NewCategory;
    FaddCategory.Caption:=SAS_STR_AddNewCategory;
    Badd.Caption:=SAS_STR_Add;
    CBShow.Checked:=true;
  end else begin
    FaddCategory.Caption:=SAS_STR_EditCategory;
    Badd.Caption:=SAS_STR_Edit;
    EditName.Text:=ACategory.name;
    EditS1.Value:=ACategory.AfterScale;
    EditS2.Value:=ACategory.BeforeScale;
    CBShow.Checked:=ACategory.visible;
  end;
  if ShowModal = mrOk then begin
    Result :=
      TMarkCategory.Create(
        ACategory.Id,
        ExcludeTrailingBackslash(EditName.Text),
        CBShow.Checked,
        EditS1.Value,
        EditS2.Value
      );
  end else begin
    Result := nil;
  end;
end;

procedure TFAddCategory.BaddClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFAddCategory.Button2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
