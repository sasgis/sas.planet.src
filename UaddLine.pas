unit UaddLine;

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
  UResStrings,
  UMarksExplorer,
  i_MarksSimple,
  u_MarksSimple,
  u_MarksDbGUIHelper,
  fr_MarkDescription,
  t_GeoTypes;

type
  TFaddLine = class(TCommonFormParent)
    Label1: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    EditName: TEdit;
    Badd: TButton;
    Button2: TButton;
    CheckBox2: TCheckBox;
    ColorBox1: TColorBox;
    SpinEdit1: TSpinEdit;
    OpenDialog1: TOpenDialog;
    SEtransp: TSpinEdit;
    Label4: TLabel;
    ColorDialog1: TColorDialog;
    SpeedButton1: TSpeedButton;
    Label7: TLabel;
    CBKateg: TComboBox;
    pnlCategory: TPanel;
    pnlName: TPanel;
    pnlDescription: TPanel;
    flwpnlStyle: TFlowPanel;
    pnlBottomButtons: TPanel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BaddClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FMark: IMarkFull;
    frMarkDescription: TfrMarkDescription;
    FMarkDBGUI: TMarksDbGUIHelper;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function EditMark(AMark: IMarkFull; AMarkDBGUI: TMarksDbGUIHelper): IMarkFull;
    procedure RefreshTranslation; override;
  end;

var
  FaddLine: TFaddLine;

implementation

uses
  u_GlobalState,
  u_MarksReadWriteSimple;

{$R *.dfm}

function TFaddLine.EditMark(AMark: IMarkFull; AMarkDBGUI: TMarksDbGUIHelper): IMarkFull;
var
  namecatbuf:string;
  i: Integer;
  VCategory: TCategoryId;
  VId: integer;
  VCategoryList: TList;
  VIndex: Integer;
begin
  FMark := AMark;
  FMarkDBGUI := AMarkDBGUI;
  frMarkDescription.Description := '';
  EditName.Text:=SAS_STR_NewPath;
  namecatbuf:=CBKateg.Text;
  VCategoryList := FMarkDBGUI.MarksDB.CategoryDB.GetCategoriesList;
  try
    FMarkDBGUI.CategoryListToStrings(VCategoryList, CBKateg.Items);
    CBKateg.Sorted:=true;
    CBKateg.Text:=namecatbuf;
    EditName.Text:=FMark.name;
    frMarkDescription.Description := FMark.Desc;
    SEtransp.Value:=100-round(AlphaComponent(FMark.Color1)/255*100);
    SpinEdit1.Value:=FMark.Scale1;
    ColorBox1.Selected:=WinColor(FMark.Color1);
    CheckBox2.Checked:=(FMark as IMarkVisible).visible;
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
    if FMark.id < 0 then begin
      Caption:=SAS_STR_AddNewPath;
      Badd.Caption:=SAS_STR_Add;
    end else begin
      Caption:=SAS_STR_EditPath;
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
      Result := AMarkDBGUI.MarksDB.MarksDb.Factory.CreateLine(
        EditName.Text,
        CheckBox2.Checked,
        VId,
        frMarkDescription.Description,
        FMark.Points,
        SetAlpha(Color32(ColorBox1.Selected),round(((100-SEtransp.Value)/100)*256)),
        SpinEdit1.Value,
        FMark
      );
    end else begin
      Result := nil;
    end;
  finally
    FreeAndNil(VCategoryList);
  end;
end;

procedure TFaddLine.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FMark := nil;
end;

procedure TFaddLine.FormShow(Sender: TObject);
begin
  frMarkDescription.Parent := pnlDescription;
  EditName.SetFocus;
end;

procedure TFaddLine.RefreshTranslation;
begin
  inherited;
  frMarkDescription.RefreshTranslation;
end;

procedure TFaddLine.BaddClick(Sender: TObject);
var
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
  if VCategory <> nil then begin
    VId := VCategory.id;
  end else begin
    VId := FMarkDBGUI.AddKategory(CBKateg.Text);
  end;
  ModalResult:=mrOk;
end;

constructor TFaddLine.Create(AOwner: TComponent);
begin
  inherited;
  frMarkDescription := TfrMarkDescription.Create(nil);
end;

destructor TFaddLine.Destroy;
begin
  FreeAndNil(frMarkDescription);
  inherited;
end;

procedure TFaddLine.SpeedButton1Click(Sender: TObject);
begin
 if ColorDialog1.Execute then ColorBox1.Selected:=ColorDialog1.Color;
end;

end.
