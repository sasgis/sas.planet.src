unit UaddPoint;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  graphics,
  ExtCtrls,
  StdCtrls,
  Grids,
  Buttons,
  Spin,
  GR32,
  GR32_Resamplers,
  u_CommonFormAndFrameParents,
  UResStrings,
  UMarksExplorer,
  i_IMarkPicture,
  i_MarksSimple,
  u_MarksSimple,
  u_MarksDbGUIHelper,
  fr_MarkDescription,
  fr_LonLat,
  t_GeoTypes;

type
  TFaddPoint = class(TCommonFormParent)
    EditName: TEdit;
    Label1: TLabel;
    Badd: TButton;
    Button2: TButton;
    Bevel1: TBevel;
    CheckBox2: TCheckBox;
    ColorBox1: TColorBox;
    Label3: TLabel;
    Label4: TLabel;
    SpinEdit1: TSpinEdit;
    Label5: TLabel;
    ColorBox2: TColorBox;
    Label6: TLabel;
    SpinEdit2: TSpinEdit;
    SEtransp: TSpinEdit;
    Label7: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    ColorDialog1: TColorDialog;
    Label8: TLabel;
    CBKateg: TComboBox;
    DrawGrid1: TDrawGrid;
    Image1: TImage;
    pnlBottomButtons: TPanel;
    flwpnlTrahsparent: TFlowPanel;
    flwpnlTextColor: TFlowPanel;
    flwpnlShadowColor: TFlowPanel;
    flwpnlFontSize: TFlowPanel;
    flwpnlIconSize: TFlowPanel;
    grdpnlStyleRows: TGridPanel;
    grdpnlLine1: TGridPanel;
    grdpnlLine2: TGridPanel;
    pnlDescription: TPanel;
    pnlLonLat: TPanel;
    pnlTop: TPanel;
    pnlImage: TPanel;
    pnlTopMain: TPanel;
    pnlCategory: TPanel;
    pnlName: TPanel;
    procedure BaddClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawGrid1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
  private
    FMark: IMarkFull;
    FPicName: string;
    FPic: IMarkPicture;
    frMarkDescription: TfrMarkDescription;
    frLonLatPoint: TfrLonLat;
    FMarkDBGUI: TMarksDbGUIHelper;
    procedure DrawFromMarkIcons(canvas:TCanvas; APic: IMarkPicture; bound:TRect);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function EditMark(AMark: IMarkFull; AMarkDBGUI: TMarksDbGUIHelper): IMarkFull;
    procedure RefreshTranslation; override;
  end;

var
  FaddPoint: TFaddPoint;

implementation

uses
  Math,
  u_GlobalState,
  u_MarksReadWriteSimple;

{$R *.dfm}

function TFaddPoint.EditMark(AMark: IMarkFull; AMarkDBGUI: TMarksDbGUIHelper): IMarkFull;
var
  VLastUsedCategoryName:string;
  i: Integer;
  VCategory: TCategoryId;
  VId: integer;
  VPicCount: Integer;
  VColCount: Integer;
  VRowCount: Integer;
  VPictureList: IMarkPictureList;
  VCategoryList: TList;
  VIndex: Integer;
  VLonLat:TDoublePoint;
begin
  FMark := AMark;
  FMarkDBGUI := AMarkDBGUI;
  frMarkDescription.Description:='';
  EditName.Text:=SAS_STR_NewMark;
  VLastUsedCategoryName:=CBKateg.Text;
  VCategoryList := FMarkDBGUI.MarksDB.CategoryDB.GetCategoriesList;
  try
    FMarkDBGUI.CategoryListToStrings(VCategoryList, CBKateg.Items);
    CBKateg.Sorted:=true;
    CBKateg.Text:=VLastUsedCategoryName;
    VPictureList := FMarkDBGUI.MarksDB.MarksDb.MarkPictureList;
    VPicCount := VPictureList.Count;
    VColCount := DrawGrid1.ColCount;
    VRowCount := VPicCount div VColCount;
    if (VPicCount mod VColCount) > 0 then begin
      Inc(VRowCount);
    end;
    DrawGrid1.RowCount := VRowCount;
    DrawGrid1.Repaint;
    FPicName := FMark.PicName;
    FPic := FMark.Pic;
    EditName.Text:=FMark.name;
    frMarkDescription.Description:=FMark.Desc;
    SpinEdit1.Value:=FMark.Scale1;
    SpinEdit2.Value:=FMark.Scale2;
    SEtransp.Value:=100-round(AlphaComponent(FMark.Color1)/255*100);
    ColorBox1.Selected:=WinColor(FMark.Color1);
    ColorBox2.Selected:=WinColor(FMark.Color2);
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
      if FPic = nil then begin
        if VPicCount > 0 then begin
          FPic := VPictureList.Get(0);
          FPicName := VPictureList.GetName(0);
        end;
      end;
      Caption:=SAS_STR_AddNewMark;
      Badd.Caption:=SAS_STR_Add;
    end else begin
      Caption:=SAS_STR_EditMark;
      Badd.Caption:=SAS_STR_Edit;
    end;
    DrawFromMarkIcons(Image1.canvas, FMark.Pic, bounds(4,4,36,36));
    frLonLatPoint.LonLat := FMark.Points[0];
    if ShowModal=mrOk then begin
      VLonLat := frLonLatPoint.LonLat;
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
      Result := AMarkDBGUI.MarksDB.MarksDb.Factory.CreatePoint(
        EditName.Text,
        CheckBox2.Checked,
        FPicName,
        FPic,
        VId,
        frMarkDescription.Description,
        VLonLat,
        SetAlpha(Color32(ColorBox1.Selected),round(((100-SEtransp.Value)/100)*256)),
        SetAlpha(Color32(ColorBox2.Selected),round(((100-SEtransp.Value)/100)*256)),
        SpinEdit1.Value,
        SpinEdit2.Value,
        FMark
      );
    end else begin
      Result := nil;
    end;
  finally
    FreeAndNil(VCategoryList);
  end;
end;
procedure TFaddPoint.BaddClick(Sender: TObject);
var
  VLonLat:TDoublePoint;
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
  ModalResult := mrOk;
end;

procedure TFaddPoint.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FMark := nil;
end;

//procedure TFaddPoint.EditCommentKeyPress(Sender: TObject; var Key: Char);
//begin
// if key='$' then
//  begin
//   if (sender is TEdit) then key:=' ';
//   if (sender is TMemo) then key:=' ';
//  end;
//end;
//
//procedure TFaddPoint.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
//begin
// if Key=VK_ESCAPE then close;
// if Key=VK_RETURN then BaddClick(Sender);
//end;

procedure TFaddPoint.FormShow(Sender: TObject);
begin
  frLonLatPoint.Parent := pnlLonLat;
  frMarkDescription.Parent := pnlDescription;
  EditName.SetFocus;
  DrawGrid1.Visible:=false;
end;

procedure TFaddPoint.SpeedButton1Click(Sender: TObject);
begin
 if ColorDialog1.Execute then ColorBox1.Selected:=ColorDialog1.Color;
end;

procedure TFaddPoint.SpeedButton2Click(Sender: TObject);
begin
 if ColorDialog1.Execute then ColorBox2.Selected:=ColorDialog1.Color;
end;

constructor TFaddPoint.Create(AOwner: TComponent);
begin
  inherited;
  frMarkDescription := TfrMarkDescription.Create(nil);
  frLonLatPoint := TfrLonLat.Create(nil);
end;

destructor TFaddPoint.Destroy;
begin
  FreeAndNil(frMarkDescription);
  FreeAndNil(frLonLatPoint);
  inherited;
end;

procedure TFaddPoint.DrawFromMarkIcons(canvas:TCanvas; APic: IMarkPicture; bound:TRect);
var
  Bitmap: TCustomBitmap32;
  Bitmap2: TBitmap32;
  wdth:integer;
begin
  canvas.FillRect(bound);
  if APic <> nil then begin
    wdth:=min(bound.Right-bound.Left,bound.Bottom-bound.Top);
    Bitmap:=TCustomBitmap32.Create;
    try
      APic.LoadBitmap(Bitmap);
      Bitmap.DrawMode:=dmBlend;
      Bitmap.Resampler:=TKernelResampler.Create;
      TKernelResampler(Bitmap.Resampler).Kernel:=TLinearKernel.Create;

      Bitmap2:=TBitmap32.Create;
      try
        Bitmap2.SetSize(wdth,wdth);
        Bitmap2.Clear(clWhite32);
        Bitmap2.Draw(Bounds(0, 0, wdth,wdth), Bounds(0, 0, Bitmap.Width,Bitmap.Height),Bitmap);
        Bitmap2.DrawTo(canvas.Handle, bound, Bounds(0, 0, Bitmap2.Width,Bitmap2.Height));
      finally
        Bitmap2.Free;
      end;
    finally
      Bitmap.Free;
    end;
  end;
end;

procedure TFaddPoint.DrawGrid1DrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  i:Integer;
  VPictureList: IMarkPictureList;
begin
   i:=(Arow*DrawGrid1.ColCount)+ACol;
   VPictureList := FMarkDBGUI.MarksDB.MarksDb.MarkPictureList;
   if i < VPictureList.Count then
    DrawFromMarkIcons(DrawGrid1.Canvas, VPictureList.Get(i), DrawGrid1.CellRect(ACol,ARow));
end;

procedure TFaddPoint.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 DrawGrid1.Visible:=not(DrawGrid1.Visible);
 if DrawGrid1.Visible then DrawGrid1.SetFocus;
end;

procedure TFaddPoint.RefreshTranslation;
begin
  inherited;
  frLonLatPoint.RefreshTranslation;
  frMarkDescription.RefreshTranslation;
end;

procedure TFaddPoint.DrawGrid1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i:integer;
  ACol,ARow: Integer;
  VPictureList: IMarkPictureList;
begin
 DrawGrid1.MouseToCell(X,Y,ACol,ARow);
 i:=(ARow*DrawGrid1.ColCount)+ACol;
 VPictureList := FMarkDBGUI.MarksDB.MarksDb.MarkPictureList;
 if (ARow>-1)and(ACol>-1) and (i < VPictureList.Count) then begin
   FPic := VPictureList.Get(i);
   FPicName := VPictureList.GetName(i);
   image1.Canvas.FillRect(image1.Canvas.ClipRect);
   DrawFromMarkIcons(image1.Canvas, FPic, bounds(5,5,36,36));
   DrawGrid1.Visible:=false;
 end;
end;

end.
