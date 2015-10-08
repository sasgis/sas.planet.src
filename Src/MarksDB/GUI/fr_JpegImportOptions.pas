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

unit fr_JpegImportOptions;

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
  i_JpegWithExifImportConfig,
  i_AppearanceOfMarkFactory,
  i_MarkFactory,
  i_MarkCategoryDB,
  i_LanguageManager,
  u_CommonFormAndFrameParents,
  fr_PictureSelectFromList,
  fr_SelectedPicture,
  fr_MarkCategorySelectOrAdd;

type
  TfrJpegImportOptions = class(TFrame)
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
    ColorDialog1: TColorDialog;
    pnlCategory: TPanel;
    pnlImage: TPanel;
    sePointShadowAlfa: TSpinEdit;
    lblPointShadowAlfa: TLabel;
    flwpnlPointText: TFlowPanel;
    pnlPointParams: TPanel;
    flwpnlPointShadowParams: TFlowPanel;
    grp1: TGroupBox;
    chk1: TCheckBox;
    procedure btnPointTextColorClick(Sender: TObject);
    procedure btnPointShadowColorClick(Sender: TObject);
    procedure imgIconMouseDown(Sender: TObject);
    procedure pnlImageResize(Sender: TObject);
  private
    frMarkCategory: TfrMarkCategorySelectOrAdd;
    frSelectPicture: TfrPictureSelectFromList;
    frSelectedPicture: TfrSelectedPicture;
    FAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
    FMarkFactory: IMarkFactory;
    procedure SelectImageFromList(Sender: TObject);
  public
    procedure Init(const ACategory: ICategory);
    function GetConfig: IJpegWithExifImportConfig;
    procedure Clear;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
      const AMarkFactory: IMarkFactory;
      const ACategoryDB: IMarkCategoryDB
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
  u_ImportConfig,
  u_JpegWithExifImportConfig;

{$R *.dfm}

constructor TfrJpegImportOptions.Create(
  const ALanguageManager: ILanguageManager;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
  const AMarkFactory: IMarkFactory;
  const ACategoryDB: IMarkCategoryDB
);
begin
  inherited Create(ALanguageManager);
  FAppearanceOfMarkFactory := AAppearanceOfMarkFactory;
  FMarkFactory := AMarkFactory;

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

destructor TfrJpegImportOptions.Destroy;
begin
  FreeAndNil(frMarkCategory);
  FreeAndNil(frSelectPicture);
  FreeAndNil(frSelectedPicture);
  inherited;
end;

procedure TfrJpegImportOptions.Init(const ACategory: ICategory);
var
  VConfig: IMarkFactoryConfig;
  VPointTemplate: IMarkTemplatePoint;
  VPicName: string;
  VPic: IMarkPicture;
begin
  // ToDo: create custom point template

  frMarkCategory.Parent := pnlCategory;
  frMarkCategory.Init(ACategory);
  frSelectPicture.Visible := False;
  frSelectPicture.Parent := Self;

  frSelectedPicture.Parent := pnlImage;

  VConfig := FMarkFactory.Config;

  VPointTemplate := VConfig.PointTemplateConfig.DefaultTemplate;

  VPicName := 'photo.png'; // ToDo: use template
  VPic := FMarkFactory.MarkPictureList.FindByNameOrDefault(VPicName);

  if not Assigned(VPic) then begin
    VPicName := VPointTemplate.IconAppearance.PicName;
    VPic := FMarkFactory.MarkPictureList.FindByNameOrDefault(VPicName);
  end;

  frSelectPicture.Picture := VPic;
  frSelectedPicture.Picture := frSelectPicture.Picture;

  clrbxPointTextColor.Selected := WinColor(VPointTemplate.CaptionAppearance.TextColor);
  clrbxPointShadowColor.Selected := WinColor(VPointTemplate.CaptionAppearance.TextBgColor);
  sePointTextTransp.Value := 100; // ToDo: use template
  sePointShadowAlfa.Value := 100; // ToDo: use template
  sePointFontSize.Value := VPointTemplate.CaptionAppearance.FontSize;
  sePointIconSize.Value := 64; // ToDo: use template
end;

procedure TfrJpegImportOptions.SelectImageFromList(Sender: TObject);
begin
  frSelectPicture.Visible := False;
  frSelectedPicture.Picture := frSelectPicture.Picture;
end;

procedure TfrJpegImportOptions.Clear;
begin
  frMarkCategory.Clear;
end;

function TfrJpegImportOptions.GetConfig: IJpegWithExifImportConfig;
var
  VPointParams: IImportPointParams;
  VCategory: ICategory;
  VPic: IMarkPicture;
  VPicName: string;
  VAppearance: IAppearance;
begin
  VCategory := frMarkCategory.GetCategory;

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

  VPointParams := TImportPointParams.Create(VAppearance, False, False, False, False, False);

  Result :=
    TJpegWithExifImportConfig.Create(
      VCategory,
      TImportCategoryParams.Create(True, False, False, False),
      VPointParams,
      chk1.Checked,
      600 // ToDo
    );
end;

procedure TfrJpegImportOptions.btnPointShadowColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then begin
    clrbxPointShadowColor.Selected := ColorDialog1.Color;
  end;
end;

procedure TfrJpegImportOptions.btnPointTextColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then begin
    clrbxPointTextColor.Selected := ColorDialog1.Color;
  end;
end;

procedure TfrJpegImportOptions.imgIconMouseDown(Sender: TObject);
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

procedure TfrJpegImportOptions.pnlImageResize(Sender: TObject);
begin
  pnlImage.Width := pnlImage.Height;
end;

end.
