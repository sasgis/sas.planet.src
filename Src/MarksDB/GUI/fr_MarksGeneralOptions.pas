{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

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
  i_Category,
  i_ImportConfig,
  i_AppearanceOfMarkFactory,
  i_MarkFactory,
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
    FAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
    FMarkFactory: IMarkFactory;
    FIsIgnoreByDefault: Boolean;
    FIsForceSet: Boolean;
    procedure SelectImageFromList(Sender: TObject);
  public
    procedure Init(const ACategory: ICategory);
    function GetImportConfig: IImportConfig;
    procedure Clear;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
      const AMarkFactory: IMarkFactory;
      const ACategoryDB: IMarkCategoryDB;
      const AIsIgnoreByDefault: Boolean;
      const AIsForceSet: Boolean
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  GR32,
  i_Appearance,
  i_MarkTemplate,
  i_MarkPicture,
  i_MarkFactoryConfig,
  u_ImportConfig;

{$R *.dfm}

constructor TfrMarksGeneralOptions.Create(
  const ALanguageManager: ILanguageManager;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
  const AMarkFactory: IMarkFactory;
  const ACategoryDB: IMarkCategoryDB;
  const AIsIgnoreByDefault: Boolean;
  const AIsForceSet: Boolean
);
begin
  inherited Create(ALanguageManager);
  FAppearanceOfMarkFactory := AAppearanceOfMarkFactory;
  FMarkFactory := AMarkFactory;
  FIsIgnoreByDefault := AIsIgnoreByDefault;
  FIsForceSet := AIsForceSet;

  frMarkCategory :=
    TfrMarkCategorySelectOrAdd.Create(
      ALanguageManager,
      ACategoryDB
    );

  frSelectPicture :=
    TfrPictureSelectFromList.Create(
      ALanguageManager,
      AMarkFactory.MarkPictureList,
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
  VConfig: IMarkFactoryConfig;
  VPointTemplate: IMarkTemplatePoint;
  VPathTemplate: IMarkTemplateLine;
  VPolyTemplate: IMarkTemplatePoly;
  VPicName: string;
  VPic: IMarkPicture;
begin
  frMarkCategory.Parent := pnlCategory;
  frMarkCategory.Init(ACategory);
  frSelectPicture.Visible := False;
  frSelectPicture.Parent := Self;

  frSelectedPicture.Parent := pnlImage;

  VConfig := FMarkFactory.Config;

  VPointTemplate := VConfig.PointTemplateConfig.DefaultTemplate;

  VPicName := VPointTemplate.IconAppearance.PicName;
  VPic := FMarkFactory.MarkPictureList.FindByNameOrDefault(VPicName);

  frSelectPicture.Picture := VPic;
  frSelectedPicture.Picture := frSelectPicture.Picture;

  clrbxPointTextColor.Selected := WinColor(VPointTemplate.CaptionAppearance.TextColor);
  clrbxPointShadowColor.Selected := WinColor(VPointTemplate.CaptionAppearance.TextBgColor);
  sePointTextTransp.Value := 100 - round(AlphaComponent(VPointTemplate.CaptionAppearance.TextColor) / 255 * 100);
  sePointShadowAlfa.Value := 100 - round(AlphaComponent(VPointTemplate.CaptionAppearance.TextBgColor) / 255 * 100);
  sePointFontSize.Value := VPointTemplate.CaptionAppearance.FontSize;
  sePointIconSize.Value := VPointTemplate.IconAppearance.MarkerSize;


  VPathTemplate := VConfig.LineTemplateConfig.DefaultTemplate;
  clrbxLineColor.Selected := WinColor(VPathTemplate.LineAppearance.LineColor);
  seLineTransp.Value := 100 - round(AlphaComponent(VPathTemplate.LineAppearance.LineColor) / 255 * 100);
  seLineWidth.Value := VPathTemplate.LineAppearance.LineWidth;

  VPolyTemplate := VConfig.PolyTemplateConfig.DefaultTemplate;

  clrbxPolyLineColor.Selected := WinColor(VPolyTemplate.BorderAppearance.LineColor);
  sePolyLineTransp.Value := 100 - round(AlphaComponent(VPolyTemplate.BorderAppearance.LineColor) / 255 * 100);
  clrbxPolyFillColor.Selected := WinColor(VPolyTemplate.FillAppearance.FillColor);
  sePolyFillTransp.Value := 100 - round(AlphaComponent(VPolyTemplate.FillAppearance.FillColor) / 255 * 100);
  sePolyLineWidth.Value := VPolyTemplate.BorderAppearance.LineWidth;

  chkPointIgnore.Checked := FIsIgnoreByDefault;
  chkLineIgnore.Checked := FIsIgnoreByDefault;
  chkPolyIgnore.Checked := FIsIgnoreByDefault;
end;

procedure TfrMarksGeneralOptions.SelectImageFromList(Sender: TObject);
begin
  frSelectPicture.Visible := False;
  frSelectedPicture.Picture := frSelectPicture.Picture;
end;

procedure TfrMarksGeneralOptions.Clear;
begin
  frMarkCategory.Clear;
end;

function TfrMarksGeneralOptions.GetImportConfig: IImportConfig;
var
  VPointParams: IImportPointParams;
  VLineParams: IImportLineParams;
  VPolyParams: IImportPolyParams;
  VCategory: ICategory;
  VPic: IMarkPicture;
  VPicName: string;
  VAppearance: IAppearance;
begin
  VPointParams := nil;
  VLineParams := nil;
  VPolyParams := nil;
  VCategory := frMarkCategory.GetCategory;
  if not chkPointIgnore.Checked then begin
    VPicName := '';
    VPic := frSelectPicture.Picture;
    if VPic <> nil then begin
      VPicName := VPic.GetName;
    end;
    VAppearance :=
      FAppearanceOfMarkFactory.CreatePointAppearance(
        SetAlpha(Color32(clrbxPointTextColor.Selected), round(((100 - sePointTextTransp.Value) / 100) * 256)),
        SetAlpha(Color32(clrbxPointShadowColor.Selected), round(((100 - sePointShadowAlfa.Value) / 100) * 256)),
        sePointFontSize.Value,
        VPicName,
        VPic,
        sePointIconSize.Value
      );
    VPointParams := TImportPointParams.Create(VAppearance, FIsForceSet, FIsForceSet, FIsForceSet, FIsForceSet, FIsForceSet);
  end;
  if not chkLineIgnore.Checked then begin
    VAppearance :=
      FAppearanceOfMarkFactory.CreateLineAppearance(
        SetAlpha(Color32(clrbxLineColor.Selected), round(((100 - seLineTransp.Value) / 100) * 256)),
        seLineWidth.Value
      );
    VLineParams := TImportLineParams.Create(VAppearance, FIsForceSet, FIsForceSet);
  end;
  if not chkPolyIgnore.Checked then begin
    VAppearance :=
      FAppearanceOfMarkFactory.CreatePolygonAppearance(
        SetAlpha(Color32(clrbxPolyLineColor.Selected), round(((100 - sePolyLineTransp.Value) / 100) * 256)),
        sePolyLineWidth.Value,
        SetAlpha(Color32(clrbxPolyFillColor.Selected), round(((100 - sePolyFillTransp.Value) / 100) * 256))
      );
    VPolyParams := TImportPolyParams.Create(VAppearance, FIsForceSet, FIsForceSet, FIsForceSet);
  end;
  Result :=
    TImportConfig.Create(
      VCategory,
      TImportCategoryParams.Create(True, False, False, False),
      VPointParams,
      VLineParams,
      VPolyParams
    );
end;

procedure TfrMarksGeneralOptions.btnLineColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then begin
    clrbxLineColor.Selected := ColorDialog1.Color;
  end;
end;

procedure TfrMarksGeneralOptions.btnPointShadowColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then begin
    clrbxPointShadowColor.Selected := ColorDialog1.Color;
  end;
end;

procedure TfrMarksGeneralOptions.btnPointTextColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then begin
    clrbxPointTextColor.Selected := ColorDialog1.Color;
  end;
end;

procedure TfrMarksGeneralOptions.btnPolyFillColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then begin
    clrbxPolyFillColor.Selected := ColorDialog1.Color;
  end;
end;

procedure TfrMarksGeneralOptions.btnPolyLineColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then begin
    clrbxPolyLineColor.Selected := ColorDialog1.Color;
  end;
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
