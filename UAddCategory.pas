unit UAddCategory;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  Spin,
  u_MarksSimple,
  UResStrings;

type
  TFAddCategory = class(TForm)
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
    procedure BaddClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FCategory:TCategoryId;
  public
   function EditCategory(ACategory:TCategoryId): Boolean;
  end;

var
  FAddCategory: TFAddCategory;

implementation

{$R *.dfm}
function TFAddCategory.EditCategory(ACategory:TCategoryId): Boolean;
begin
  FCategory := ACategory;
  EditName.Text:=SAS_STR_NewPoly;
  if FCategory.id < 0 then begin
    EditName.Text:=SAS_STR_NewCategory;
    FaddCategory.Caption:=SAS_STR_AddNewCategory;
    Badd.Caption:=SAS_STR_Add;
    CBShow.Checked:=true;
  end else begin
    FaddCategory.Caption:=SAS_STR_EditCategory;
    Badd.Caption:=SAS_STR_Edit;
    EditName.Text:=FCategory.name;
    EditS1.Value:=FCategory.AfterScale;
    EditS2.Value:=FCategory.BeforeScale;
    CBShow.Checked:=FCategory.visible;
  end;
  Result := ShowModal = mrOk;
end;

procedure TFAddCategory.BaddClick(Sender: TObject);
begin
  FCategory.name:=EditName.Text;
  FCategory.AfterScale:=EditS1.Value;
  FCategory.BeforeScale:=EditS2.Value;
  FCategory.visible:=CBShow.Checked;
  ModalResult := mrOk;
end;

procedure TFAddCategory.Button2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
