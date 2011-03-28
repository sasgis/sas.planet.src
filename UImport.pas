unit UImport;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Spin,
  ExtCtrls,
  Buttons,
  GR32,
  GR32_Resamplers,
  u_CommonFormAndFrameParents,
  i_MarksSimple,
  i_IMarkCategory,
  i_ImportConfig,
  u_MarksDbGUIHelper;

type
  TFImport = class(TCommonFormParent)
    lblCategory: TLabel;
    CBKateg: TComboBox;
    grpPoint: TGroupBox;
    lblPointTextColor: TLabel;
    lblPointShadowColor: TLabel;
    lblPointFontSize: TLabel;
    lblPointIconSize: TLabel;
    lblPointTextTransp: TLabel;
    btnPointTextColor: TSpeedButton;
    btnPointShadowColor: TSpeedButton;
    clrbxPointTextColor: TColorBox;
    sePointFontSize: TSpinEdit;
    clrbxPointShadowColor: TColorBox;
    sePointIconSize: TSpinEdit;
    sePointTextTransp: TSpinEdit;
    cbbPointIcon: TComboBox;
    grpLine: TGroupBox;
    grpPoly: TGroupBox;
    ColorDialog1: TColorDialog;
    lblLineColor: TLabel;
    lblLineWidth: TLabel;
    lblLineTransp: TLabel;
    btnLineColor: TSpeedButton;
    clrbxLineColor: TColorBox;
    seLineWidth: TSpinEdit;
    seLineTransp: TSpinEdit;
    lblPolyLineColor: TLabel;
    lblPolyLineWidth: TLabel;
    lblPolyLineTransp: TLabel;
    btnPolyLineColor: TSpeedButton;
    lblPolyFillColor: TLabel;
    lblPolyFillTransp: TLabel;
    btnPolyFillColor: TSpeedButton;
    lblPolyLine: TLabel;
    lblPolyFill: TLabel;
    clrbxPolyLineColor: TColorBox;
    sePolyLineWidth: TSpinEdit;
    sePolyLineTransp: TSpinEdit;
    clrbxPolyFillColor: TColorBox;
    sePolyFillTransp: TSpinEdit;
    btnOk: TButton;
    btnCancel: TButton;
    lblPointIcon: TLabel;
    chkPointIgnore: TCheckBox;
    chkLineIgnore: TCheckBox;
    chkPolyIgnore: TCheckBox;
    procedure btnPointTextColorClick(Sender: TObject);
    procedure btnPointShadowColorClick(Sender: TObject);
    procedure cbbPointIconDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure btnLineColorClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnPolyLineColorClick(Sender: TObject);
    procedure btnPolyFillColorClick(Sender: TObject);
  private
    { Private declarations }
    FMarkDBGUI: TMarksDbGUIHelper;
    FCategoryList: IInterfaceList;
    FCategory: IMarkCategory;
  public
    function GetImportConfig(AMarkDBGUI: TMarksDbGUIHelper): IImportConfig;
  end;

var
  FImport: TFImport;

implementation

uses
  i_IMarkPicture,
  u_MarksSimple,
  u_ImportConfig;

{$R *.dfm}

procedure TFImport.btnPointTextColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then clrbxPointTextColor.Selected:=ColorDialog1.Color;
end;

procedure TFImport.btnPointShadowColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then clrbxPointShadowColor.Selected:=ColorDialog1.Color;
end;

procedure TFImport.btnLineColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then clrbxLineColor.Selected:=ColorDialog1.Color;
end;

procedure TFImport.btnPolyLineColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then clrbxPolyLineColor.Selected:=ColorDialog1.Color;
end;

procedure TFImport.btnPolyFillColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then clrbxPolyFillColor.Selected:=ColorDialog1.Color;
end;

procedure TFImport.cbbPointIconDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  Bitmap: TCustomBitmap32;
  Bitmap2: TBitmap32;
  VPic: IMarkPicture;
begin
  cbbPointIcon.Canvas.FillRect(Rect);

  Bitmap:=TCustomBitmap32.Create;
  try
    VPic := IMarkPicture(Pointer(cbbPointIcon.Items.Objects[Index]));
    VPic.LoadBitmap(Bitmap);
    Bitmap.DrawMode:=dmBlend;
    Bitmap.Resampler:=TKernelResampler.Create;
    TKernelResampler(Bitmap.Resampler).Kernel:=TCubicKernel.Create;

    Bitmap2:=TBitmap32.Create;
    try
      Bitmap2.SetSize(31,31);
      Bitmap2.Clear(clWhite32);
      Bitmap2.Draw(Bounds(0, 0, 31,31), Bounds(0, 0, Bitmap.Width,Bitmap.Height),Bitmap);
      Bitmap2.DrawTo(
        cbbPointIcon.Canvas.Handle,
        Bounds(Rect.Left + 2, Rect.Top + 2, 31,31),
        Bounds(0, 0, Bitmap2.Width,Bitmap2.Height)
      );
    finally
      Bitmap2.Free;
    end;
  finally
    Bitmap.Free;
  end;
end;

function TFImport.GetImportConfig(AMarkDBGUI: TMarksDbGUIHelper): IImportConfig;
var
  VIndex: Integer;
  VPic: IMarkPicture;
  VPicName: string;
  VMarkTemplatePoint: IMarkTemplatePoint;
  VMarkTemplateLine: IMarkTemplateLine;
  VMarkTemplatePoly: IMarkTemplatePoly;
  VPictureList: IMarkPictureList;
  i: Integer;
begin
  FMarkDBGUI := AMarkDBGUI;
  VPictureList := FMarkDBGUI.MarkPictureList;
  cbbPointIcon.Items.Clear;
  for i := 0 to VPictureList.Count - 1 do begin
    cbbPointIcon.Items.AddObject(VPictureList.GetName(i), Pointer(VPictureList.Get(i)));
  end;
  try
    cbbPointIcon.Repaint;
    cbbPointIcon.ItemIndex:=0;

    FCategoryList := FMarkDBGUI.MarksDB.CategoryDB.GetCategoriesList;
    try
      FMarkDBGUI.CategoryListToStrings(FCategoryList, CBKateg.Items);
      if ShowModal = mrOk then begin
        if not chkPointIgnore.Checked then begin
          VIndex := cbbPointIcon.ItemIndex;
          if VIndex < 0 then begin
            VPic := nil;
            VPicName := '';
          end else begin
            VPic := IMarkPicture(Pointer(cbbPointIcon.Items.Objects[VIndex]));
            VPicName := cbbPointIcon.Items.Strings[VIndex];
          end;
          VMarkTemplatePoint :=
            FMarkDBGUI.MarksDB.MarksDb.Factory.Config.PointTemplateConfig.CreateTemplate(
              VPicName,
              VPic,
              FCategory,
              SetAlpha(Color32(clrbxPointTextColor.Selected),round(((100-sePointTextTransp.Value)/100)*256)),
              SetAlpha(Color32(clrbxPointShadowColor.Selected),round(((100-sePointTextTransp.Value)/100)*256)),
              sePointFontSize.Value,
              sePointIconSize.Value
            );
        end;
        VMarkTemplateLine := nil;
        if not chkLineIgnore.Checked then begin
          VMarkTemplateLine :=
            FMarkDBGUI.MarksDB.MarksDb.Factory.Config.LineTemplateConfig.CreateTemplate(
              FCategory,
              SetAlpha(Color32(clrbxLineColor.Selected),round(((100-seLineTransp.Value)/100)*256)),
              seLineWidth.Value
            );
        end;
        VMarkTemplatePoly := nil;
        if not chkPolyIgnore.Checked then begin
          VMarkTemplatePoly :=
            FMarkDBGUI.MarksDB.MarksDb.Factory.Config.PolyTemplateConfig.CreateTemplate(
              FCategory,
              SetAlpha(Color32(clrbxPolyLineColor.Selected),round(((100-sePolyLineTransp.Value)/100)*256)),
              SetAlpha(Color32(clrbxPolyFillColor.Selected),round(((100-sePolyFillTransp.Value)/100)*256)),
              sePolyLineWidth.Value
            );
        end;
        Result :=
          TImportConfig.Create(
            FMarkDBGUI.MarksDB.MarksDb,
            VMarkTemplatePoint,
            VMarkTemplateLine,
            VMarkTemplatePoly
          );
      end else begin
        Result := nil;
      end;
    finally
      FCategoryList := nil;
    end;
  finally
    cbbPointIcon.Items.Clear;
  end;
end;

procedure TFImport.btnOkClick(Sender: TObject);
var
  VCategoryText: string;
  VIndex: Integer;
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

end.
