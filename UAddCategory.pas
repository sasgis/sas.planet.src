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
  DBClient,
  UResStrings,
  UGeoFun;

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
    { Private declarations }
  public
   procedure show_(new:boolean);
  end;

var
  FAddCategory: TFAddCategory;
  new_:boolean;

implementation

uses
  Unit1,
  u_GlobalState;

{$R *.dfm}
procedure TFAddCategory.show_(new:boolean);
begin
 new_:=new;
 EditName.Text:=SAS_STR_NewPoly;
 if new then begin
              EditName.Text:=SAS_STR_NewCategory;
              FaddCategory.Caption:=SAS_STR_AddNewCategory;
              Badd.Caption:=SAS_STR_Add;
              CBShow.Checked:=true;
             end
        else begin
              FaddCategory.Caption:=SAS_STR_EditCategory;
              Badd.Caption:=SAS_STR_Edit;
              EditName.Text:=Fmain.CDSKategory.FieldByName('name').AsString;
              EditS1.Value:=Fmain.CDSKategory.FieldByName('AfterScale').AsInteger;
              EditS2.Value:=Fmain.CDSKategory.FieldByName('BeforeScale').AsInteger;
              CBShow.Checked:=Fmain.CDSKategory.FieldByName('visible').AsBoolean;
             end;
end;
procedure TFAddCategory.BaddClick(Sender: TObject);
begin
 if new_ then Fmain.CDSKategory.Insert
         else Fmain.CDSKategory.Edit;
 Fmain.CDSKategory.FieldByName('name').AsString:=EditName.Text;
 Fmain.CDSKategory.FieldByName('AfterScale').AsInteger:=EditS1.Value;
 Fmain.CDSKategory.FieldByName('BeforeScale').AsInteger:=EditS2.Value;
 Fmain.CDSKategory.FieldByName('visible').AsBoolean:=CBShow.Checked;
 Fmain.CDSKategory.ApplyRange;
 Fmain.CDSKategory.MergeChangeLog;
 Fmain.CDSKategory.SaveToFile(GState.MarksCategoryFileName,dfXMLUTF8);
end;

end.
