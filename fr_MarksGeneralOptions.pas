unit fr_MarksGeneralOptions;

interface

uses
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
  i_MarkCategory,
  i_ImportConfig,
  i_MarksDB,
  i_MarkCategoryDB,
  i_LanguageManager,
  u_CommonFormAndFrameParents,
  fr_MarkCategorySelectOrAdd;

type
  TfrMarksGeneralOptions = class(TFrame)
    grpPoint: TGroupBox;
    lblPointTextColor: TLabel;
    lblPointShadowColor: TLabel;
    lblPointFontSize: TLabel;
    lblPointIconSize: TLabel;
    lblPointTextTransp: TLabel;
    btnPointTextColor: TSpeedButton;
    btnPointShadowColor: TSpeedButton;
    lblPointIcon: TLabel;
    clrbxPointTextColor: TColorBox;
    sePointFontSize: TSpinEdit;
    clrbxPointShadowColor: TColorBox;
    sePointIconSize: TSpinEdit;
    sePointTextTransp: TSpinEdit;
    cbbPointIcon: TComboBox;
    grpLine: TGroupBox;
    lblLineColor: TLabel;
    lblLineWidth: TLabel;
    lblLineTransp: TLabel;
    btnLineColor: TSpeedButton;
    clrbxLineColor: TColorBox;
    seLineWidth: TSpinEdit;
    seLineTransp: TSpinEdit;
    grpPoly: TGroupBox;
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
    chkPointIgnore: TCheckBox;
    chkLineIgnore: TCheckBox;
    chkPolyIgnore: TCheckBox;
    ColorDialog1: TColorDialog;
    pnlCategory: TPanel;
    procedure btnPointTextColorClick(Sender: TObject);
    procedure btnPointShadowColorClick(Sender: TObject);
    procedure btnLineColorClick(Sender: TObject);
    procedure btnPolyLineColorClick(Sender: TObject);
    procedure btnPolyFillColorClick(Sender: TObject);
    procedure cbbPointIconDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  private
    frMarkCategory: TfrMarkCategorySelectOrAdd;
    FMarksDb: IMarksDb;
  public
    procedure SetIgnore(AValue: Boolean);
    procedure Init(const ACategory: ICategory);
    function GetImportConfig: IImportConfig;
    procedure Clear;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const ACategoryDB: IMarkCategoryDB;
      const AMarksDb: IMarksDb
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  i_MarkPicture,
  i_MarkTemplate,
  i_MarkFactory,
  i_MarksFactoryConfig,
  i_BitmapMarker,
  u_ImportConfig;

{$R *.dfm}

constructor TfrMarksGeneralOptions.Create(
  const ALanguageManager: ILanguageManager;
  const ACategoryDB: IMarkCategoryDB;
  const AMarksDb: IMarksDb
);
begin
  inherited Create(ALanguageManager);
  FMarksDb := AMarksDb;

  frMarkCategory :=
    TfrMarkCategorySelectOrAdd.Create(
      ALanguageManager,
      ACategoryDB
    );
end;

destructor TfrMarksGeneralOptions.Destroy;
begin
  FreeAndNil(frMarkCategory);
  inherited;
end;

procedure TfrMarksGeneralOptions.Init(const ACategory: ICategory);
var
  VPictureList: IMarkPictureList;
  i: Integer;
  VFactory: IMarkFactory;
  VConfig: IMarksFactoryConfig;
  VPointTemplate: IMarkTemplatePoint;
  VPic: IMarkPicture;
  VPathTemplate: IMarkTemplateLine;
  VPolyTemplate: IMarkTemplatePoly;
begin
  frMarkCategory.Parent := pnlCategory;
  frMarkCategory.Init(ACategory);

  VFactory := FMarksDb.Factory;
  VConfig := VFactory.Config;
  VPictureList := VFactory.MarkPictureList;
  VPointTemplate := VConfig.PointTemplateConfig.DefaultTemplate;

  clrbxPointTextColor.Selected := WinColor(VPointTemplate.TextColor);
  clrbxPointShadowColor.Selected := WinColor(VPointTemplate.TextBgColor);
  sePointTextTransp.Value := 100-round(AlphaComponent(VPointTemplate.TextColor)/255*100);
  sePointFontSize.Value := VPointTemplate.FontSize;
  sePointIconSize.Value := VPointTemplate.MarkerSize;

  cbbPointIcon.Items.BeginUpdate;
  try
    cbbPointIcon.Items.Clear;
    for i := 0 to VPictureList.Count - 1 do begin
      VPic := VPictureList.Get(i);
      cbbPointIcon.Items.AddObject(VPictureList.GetName(i), Pointer(VPic));
      if VPic = VPointTemplate.Pic then begin
        cbbPointIcon.ItemIndex := i;
      end;
    end;
    if cbbPointIcon.ItemIndex < 0 then begin
      cbbPointIcon.ItemIndex := 0;
    end;
  finally
    cbbPointIcon.Items.EndUpdate;
  end;
  cbbPointIcon.Repaint;
  VPathTemplate := VConfig.LineTemplateConfig.DefaultTemplate;
  clrbxLineColor.Selected := WinColor(VPathTemplate.LineColor);
  seLineTransp.Value := 100-round(AlphaComponent(VPathTemplate.LineColor)/255*100);
  seLineWidth.Value := VPathTemplate.LineWidth;

  VPolyTemplate := VConfig.PolyTemplateConfig.DefaultTemplate;

  clrbxPolyLineColor.Selected := WinColor(VPolyTemplate.BorderColor);
  sePolyLineTransp.Value := 100-round(AlphaComponent(VPolyTemplate.BorderColor)/255*100);
  clrbxPolyFillColor.Selected := WinColor(VPolyTemplate.FillColor);
  sePolyFillTransp.Value := 100-round(AlphaComponent(VPolyTemplate.FillColor)/255*100);
  sePolyLineWidth.Value := VPolyTemplate.LineWidth;
end;

procedure TfrMarksGeneralOptions.SetIgnore(AValue: Boolean);
begin
  chkPointIgnore.Checked := AValue;
  chkLineIgnore.Checked := AValue;
  chkPolyIgnore.Checked := AValue;
end;

procedure TfrMarksGeneralOptions.Clear;
begin
  cbbPointIcon.Items.Clear;
  frMarkCategory.Clear;
end;

function TfrMarksGeneralOptions.GetImportConfig: IImportConfig;
var
  VIndex: Integer;
  VPic: IMarkPicture;
  VMarkTemplatePoint: IMarkTemplatePoint;
  VMarkTemplateLine: IMarkTemplateLine;
  VMarkTemplatePoly: IMarkTemplatePoly;
  VCategory: ICategory;
begin
  if not chkPointIgnore.Checked then begin
    VCategory := frMarkCategory.GetCategory;
    VIndex := cbbPointIcon.ItemIndex;
    if VIndex < 0 then begin
      VPic := nil;
    end else begin
      VPic := IMarkPicture(Pointer(cbbPointIcon.Items.Objects[VIndex]));
    end;
    VMarkTemplatePoint :=
      FMarksDb.Factory.Config.PointTemplateConfig.CreateTemplate(
        VPic,
        VCategory,
        SetAlpha(Color32(clrbxPointTextColor.Selected),round(((100-sePointTextTransp.Value)/100)*256)),
        SetAlpha(Color32(clrbxPointShadowColor.Selected),round(((100-sePointTextTransp.Value)/100)*256)),
        sePointFontSize.Value,
        sePointIconSize.Value
      );
  end;
  VMarkTemplateLine := nil;
  if not chkLineIgnore.Checked then begin
    VCategory := frMarkCategory.GetCategory;
    VMarkTemplateLine :=
      FMarksDb.Factory.Config.LineTemplateConfig.CreateTemplate(
        VCategory,
        SetAlpha(Color32(clrbxLineColor.Selected),round(((100-seLineTransp.Value)/100)*256)),
        seLineWidth.Value
      );
  end;
  VMarkTemplatePoly := nil;
  if not chkPolyIgnore.Checked then begin
    VCategory := frMarkCategory.GetCategory;
    VMarkTemplatePoly :=
      FMarksDb.Factory.Config.PolyTemplateConfig.CreateTemplate(
        VCategory,
        SetAlpha(Color32(clrbxPolyLineColor.Selected),round(((100-sePolyLineTransp.Value)/100)*256)),
        SetAlpha(Color32(clrbxPolyFillColor.Selected),round(((100-sePolyFillTransp.Value)/100)*256)),
        sePolyLineWidth.Value
      );
  end;
  Result :=
    TImportConfig.Create(
      FMarksDb,
      VMarkTemplatePoint,
      VMarkTemplateLine,
      VMarkTemplatePoly
    );
end;

procedure TfrMarksGeneralOptions.btnLineColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then clrbxLineColor.Selected:=ColorDialog1.Color;
end;

procedure TfrMarksGeneralOptions.btnPointShadowColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then clrbxPointShadowColor.Selected:=ColorDialog1.Color;
end;

procedure TfrMarksGeneralOptions.btnPointTextColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then clrbxPointTextColor.Selected:=ColorDialog1.Color;
end;

procedure TfrMarksGeneralOptions.btnPolyFillColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then clrbxPolyFillColor.Selected:=ColorDialog1.Color;
end;

procedure TfrMarksGeneralOptions.btnPolyLineColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then clrbxPolyLineColor.Selected:=ColorDialog1.Color;
end;

procedure TfrMarksGeneralOptions.cbbPointIconDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  VBitmap: TBitmap32;
  VPic: IMarkPicture;
  VMarker: IBitmapMarker;
  VResampler: TCustomResampler;
begin
  cbbPointIcon.Canvas.FillRect(Rect);

  VPic := IMarkPicture(Pointer(cbbPointIcon.Items.Objects[Index]));
  if (VPic <> nil) then begin
    VMarker := VPic.GetMarker;
    VBitmap:=TBitmap32.Create;
    try
      VBitmap.SetSize(31,31);
      VBitmap.Clear(clWhite32);
      VResampler := TKernelResampler.Create;
      try
        TKernelResampler(VResampler).Kernel := TLinearKernel.Create;
        StretchTransfer(
          VBitmap,
          VBitmap.BoundsRect,
          VBitmap.ClipRect,
          VMarker.Bitmap,
          VMarker.Bitmap.BoundsRect,
          VResampler,
          dmBlend,
          cmBlend
        );
      finally
        VResampler.Free;
      end;
      VBitmap.DrawTo(
        cbbPointIcon.Canvas.Handle,
        Bounds(Rect.Left + 2, Rect.Top + 2, 31,31),
        VBitmap.BoundsRect
      );
    finally
      VBitmap.Free;
    end;
  end;
end;

end.
