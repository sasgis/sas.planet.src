unit fr_MarksGeneralOptions;

interface

uses
  Types,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Spin,
  ExtCtrls,
  Buttons,
  GR32,
  i_MarkCategory,
  i_ImportConfig,
  i_MarksDb,
  i_MarkCategoryDB,
  i_LanguageManager,
  u_CommonFormAndFrameParents,
  fr_PictureSelectFromList,
  fr_SelectedPicture,
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
    pnlImage: TPanel;
    sePointShadowAlfa: TSpinEdit;
    lblPointShadowAlfa: TLabel;
    flwpnlPointText: TFlowPanel;
    pnlPointParams: TPanel;
    flwpnlPointShadowParams: TFlowPanel;
    flwpnlPath: TFlowPanel;
    flwpnlPlygonLine: TFlowPanel;
    flwpnlPolygonFill: TFlowPanel;
    procedure btnPointTextColorClick(Sender: TObject);
    procedure btnPointShadowColorClick(Sender: TObject);
    procedure btnLineColorClick(Sender: TObject);
    procedure btnPolyLineColorClick(Sender: TObject);
    procedure btnPolyFillColorClick(Sender: TObject);
    procedure imgIconMouseDown(Sender: TObject);
    procedure pnlImageResize(Sender: TObject);
  private
    frMarkCategory: TfrMarkCategorySelectOrAdd;
    frSelectPicture: TfrPictureSelectFromList;
    frSelectedPicture: TfrSelectedPicture;
    FMarksDb: IMarksDb;
    procedure SelectImageFromList(Sender: TObject);
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
  i_MarkTemplate,
  i_MarkFactory,
  i_MarksFactoryConfig,
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

  frSelectPicture :=
    TfrPictureSelectFromList.Create(
      ALanguageManager,
      FMarksDb.Factory.MarkPictureList,
      Self.SelectImageFromList
    );
  frSelectedPicture := TfrSelectedPicture.Create(ALanguageManager, Self.imgIconMouseDown);
end;

destructor TfrMarksGeneralOptions.Destroy;
begin
  FreeAndNil(frMarkCategory);
  FreeAndNil(frSelectPicture);
  FreeAndNil(frSelectedPicture);
  inherited;
end;

procedure TfrMarksGeneralOptions.Init(const ACategory: ICategory);
var
  VFactory: IMarkFactory;
  VConfig: IMarksFactoryConfig;
  VPointTemplate: IMarkTemplatePoint;
  VPathTemplate: IMarkTemplateLine;
  VPolyTemplate: IMarkTemplatePoly;
begin
  frMarkCategory.Parent := pnlCategory;
  frMarkCategory.Init(ACategory);
  frSelectPicture.Visible := False;
  frSelectPicture.Parent := Self;

  frSelectedPicture.Parent := pnlImage;

  VFactory := FMarksDb.Factory;
  VConfig := VFactory.Config;

  VPointTemplate := VConfig.PointTemplateConfig.DefaultTemplate;
  frSelectPicture.Picture := VPointTemplate.Pic;
  frSelectedPicture.Picture := frSelectPicture.Picture;

  clrbxPointTextColor.Selected := WinColor(VPointTemplate.TextColor);
  clrbxPointShadowColor.Selected := WinColor(VPointTemplate.TextBgColor);
  sePointTextTransp.Value := 100-round(AlphaComponent(VPointTemplate.TextColor)/255*100);
  sePointShadowAlfa.Value := 100-round(AlphaComponent(VPointTemplate.TextBgColor)/255*100);
  sePointFontSize.Value := VPointTemplate.FontSize;
  sePointIconSize.Value := VPointTemplate.MarkerSize;


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

procedure TfrMarksGeneralOptions.SelectImageFromList(Sender: TObject);
begin
  frSelectPicture.Visible := False;
  frSelectedPicture.Picture := frSelectPicture.Picture;
end;

procedure TfrMarksGeneralOptions.SetIgnore(AValue: Boolean);
begin
  chkPointIgnore.Checked := AValue;
  chkLineIgnore.Checked := AValue;
  chkPolyIgnore.Checked := AValue;
end;

procedure TfrMarksGeneralOptions.Clear;
begin
  frMarkCategory.Clear;
end;

function TfrMarksGeneralOptions.GetImportConfig: IImportConfig;
var
  VMarkTemplatePoint: IMarkTemplatePoint;
  VMarkTemplateLine: IMarkTemplateLine;
  VMarkTemplatePoly: IMarkTemplatePoly;
  VCategory: ICategory;
begin
  if not chkPointIgnore.Checked then begin
    VCategory := frMarkCategory.GetCategory;
    VMarkTemplatePoint :=
      FMarksDb.Factory.Config.PointTemplateConfig.CreateTemplate(
        frSelectPicture.Picture,
        VCategory,
        SetAlpha(Color32(clrbxPointTextColor.Selected),round(((100-sePointTextTransp.Value)/100)*256)),
        SetAlpha(Color32(clrbxPointShadowColor.Selected),round(((100-sePointShadowAlfa.Value)/100)*256)),
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

procedure TfrMarksGeneralOptions.imgIconMouseDown(Sender: TObject);
var
  VPnlPos: TPoint;
begin
  if frSelectPicture.Visible then begin
    frSelectPicture.Visible := False;
  end else begin
    VPnlPos := pnlImage.ClientToParent(Point(0, 0), Self);
    frSelectPicture.Left := 5;
    frSelectPicture.Width := Self.ClientWidth - frSelectPicture.Left - 5;
    frSelectPicture.Top := VPnlPos.Y + pnlImage.Height;
    frSelectPicture.Height := Self.ClientHeight - frSelectPicture.Top - 5;
    frSelectPicture.Visible := True;
    frSelectPicture.SetFocus;
  end;
end;

procedure TfrMarksGeneralOptions.pnlImageResize(Sender: TObject);
begin
  pnlImage.Width := pnlImage.Height;
end;

end.
