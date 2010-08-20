unit UAddCategory;

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
  StdCtrls,
  ExtCtrls,
  Spin,
  UMarksExplorer,
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
  private
    new_:boolean;
    FCategory:TCategoryId;
  public
   function show_(ACategory:TCategoryId): Boolean;
  end;

var
  FAddCategory: TFAddCategory;

implementation

{$R *.dfm}
function TFAddCategory.show_(ACategory:TCategoryId): Boolean;
begin
  Result := False;
  FCategory := ACategory;
 new_:=FCategory.id < 0;
 EditName.Text:=SAS_STR_NewPoly;
 if new_ then begin
              EditName.Text:=SAS_STR_NewCategory;
              FaddCategory.Caption:=SAS_STR_AddNewCategory;
              Badd.Caption:=SAS_STR_Add;
              CBShow.Checked:=true;
             end
        else begin
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
end;

end.
