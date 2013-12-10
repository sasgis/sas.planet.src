{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit frm_MarkEditPoly;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  Spin,
  StdCtrls,
  ExtCtrls,
  Buttons,
  GR32,
  u_CommonFormAndFrameParents,
  i_Appearance,
  i_AppearanceOfMarkFactory,
  i_PathConfig,
  i_LanguageManager,
  i_Mark,
  i_VectorDataItemSimple,
  i_MarkFactory,
  i_MarkCategoryDB,
  fr_MarkDescription,
  fr_MarkCategorySelectOrAdd;

type
  TfrmMarkEditPoly = class(TFormWitghLanguageManager)
    lblName: TLabel;
    edtName: TEdit;
    btnOk: TButton;
    btnCancel: TButton;
    chkVisible: TCheckBox;
    lblLineColor: TLabel;
    lblLineWidth: TLabel;
    clrbxLineColor: TColorBox;
    seLineWidth: TSpinEdit;
    seLineTransp: TSpinEdit;
    lblLineTransp: TLabel;
    btnLineColor: TSpeedButton;
    lblFillColor: TLabel;
    clrbxFillColor: TColorBox;
    seFillTransp: TSpinEdit;
    lblFillTransp: TLabel;
    btnFillColor: TSpeedButton;
    lblLine: TLabel;
    lblFill: TLabel;
    ColorDialog1: TColorDialog;
    lblCategory: TLabel;
    CBKateg: TComboBox;
    pnlBottomButtons: TPanel;
    flwpnlFill: TFlowPanel;
    pnlFill: TPanel;
    pnlLine: TPanel;
    flwpnlLine: TFlowPanel;
    pnlDescription: TPanel;
    pnlCategory: TPanel;
    pnlName: TPanel;
    btnSetAsTemplate: TButton;
    procedure btnOkClick(Sender: TObject);
    procedure btnLineColorClick(Sender: TObject);
    procedure btnFillColorClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnSetAsTemplateClick(Sender: TObject);
  private
    FMarkFactory: IMarkFactory;
    FCategoryDB: IMarkCategoryDB;
    FAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
    frMarkDescription: TfrMarkDescription;
    frMarkCategory: TfrMarkCategorySelectOrAdd;
    function MakeAppearance: IAppearance;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMediaPath: IPathConfig;
      const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
      const AMarkFactory: IMarkFactory;
      const ACategoryDB: IMarkCategoryDB
    ); reintroduce;
    destructor Destroy; override;
    function EditMark(
      const AMark: IVectorDataItemPoly;
      const AIsNewMark: Boolean;
      var AVisible: Boolean
    ): IVectorDataItemPoly;
  end;

implementation

uses
  i_AppearanceOfVectorItem,
  i_Category,
  i_MarkTemplate,
  i_MarkFactoryConfig,
  u_ResStrings;

{$R *.dfm}

constructor TfrmMarkEditPoly.Create(
  const ALanguageManager: ILanguageManager;
  const AMediaPath: IPathConfig;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
  const AMarkFactory: IMarkFactory;
  const ACategoryDB: IMarkCategoryDB
);
begin
  inherited Create(ALanguageManager);
  FCategoryDB := ACategoryDB;
  FAppearanceOfMarkFactory := AAppearanceOfMarkFactory;
  FMarkFactory := AMarkFactory;

  frMarkDescription := TfrMarkDescription.Create(ALanguageManager, AMediaPath);
  frMarkCategory :=
    TfrMarkCategorySelectOrAdd.Create(
      ALanguageManager,
      FCategoryDB
    );
end;

destructor TfrmMarkEditPoly.Destroy;
begin
  FreeAndNil(frMarkDescription);
  FreeAndNil(frMarkCategory);
  inherited;
end;

function TfrmMarkEditPoly.EditMark(
  const AMark: IVectorDataItemPoly;
  const AIsNewMark: Boolean;
  var AVisible: Boolean
): IVectorDataItemPoly;
var
  VAppearanceBorder: IAppearancePolygonBorder;
  VAppearanceFill: IAppearancePolygonFill;
  VCategory: ICategory;
  VMarkWithCategory: IVectorDataItemWithCategory;
begin
  VCategory := nil;
  if Supports(AMark, IVectorDataItemWithCategory, VMarkWithCategory) then begin
    VCategory := VMarkWithCategory.Category;
  end;
  frMarkCategory.Init(VCategory);
  try
    edtName.Text:=AMark.Name;
    frMarkDescription.Description:=AMark.Desc;
    if Supports(AMark.Appearance, IAppearancePolygonBorder, VAppearanceBorder) then begin
      seLineTransp.Value := 100-round(AlphaComponent(VAppearanceBorder.LineColor)/255*100);
      seLineWidth.Value := VAppearanceBorder.LineWidth;
      clrbxLineColor.Selected := WinColor(VAppearanceBorder.LineColor);
    end else begin
      seLineTransp.Value := 0;
      seLineWidth.Value :=0;
      clrbxLineColor.Selected := WinColor(clBlack32);
    end;
    if Supports(AMark.Appearance, IAppearancePolygonFill, VAppearanceFill) then begin
      seFillTransp.Value := 100-round(AlphaComponent(VAppearanceFill.FillColor)/255*100);
      clrbxFillColor.Selected := WinColor(VAppearanceFill.FillColor);
    end else begin
      seFillTransp.Value := 0;
      clrbxFillColor.Selected := 0;
    end;
    chkVisible.Checked:= AVisible;
    if AIsNewMark then begin
      Caption:=SAS_STR_AddNewPoly;
    end else begin
      Caption:=SAS_STR_EditPoly;
    end;
    Self.PopupParent := Application.MainForm;
    if ShowModal=mrOk then begin
      Result :=
        FMarkFactory.CreatePoly(
          AMark.Line,
          edtName.Text,
          frMarkDescription.Description,
          frMarkCategory.GetCategory,
          MakeAppearance
        );
      AVisible := chkVisible.Checked;
    end else begin
      Result := nil;
    end;
  finally
    frMarkCategory.Clear;
  end;
end;

procedure TfrmMarkEditPoly.FormShow(Sender: TObject);
begin
  frMarkCategory.Parent := pnlCategory;
  frMarkDescription.Parent := pnlDescription;
  edtName.SetFocus;
end;

function TfrmMarkEditPoly.MakeAppearance: IAppearance;
begin
  Result :=
    FAppearanceOfMarkFactory.CreatePolygonAppearance(
      SetAlpha(Color32(clrbxLineColor.Selected),round(((100-seLineTransp.Value)/100)*256)),
      seLineWidth.Value,
      SetAlpha(Color32(clrbxFillColor.Selected),round(((100-seFillTransp.Value)/100)*256))
    );
end;

procedure TfrmMarkEditPoly.btnOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmMarkEditPoly.btnSetAsTemplateClick(Sender: TObject);
var
  VConfig: IMarkPolyTemplateConfig;
  VTemplate: IMarkTemplatePoly;
begin
  if MessageBox(handle, pchar('Set as default for new marks?'), pchar(SAS_MSG_coution), 36) = IDYES then begin
    VConfig := FMarkFactory.Config.PolyTemplateConfig;
    VTemplate :=
      VConfig.CreateTemplate(
        MakeAppearance,
        frMarkCategory.GetCategory
      );
    VConfig.DefaultTemplate := VTemplate;
  end;
end;

procedure TfrmMarkEditPoly.btnLineColorClick(Sender: TObject);
begin
 if ColorDialog1.Execute then clrbxLineColor.Selected:=ColorDialog1.Color;
end;

procedure TfrmMarkEditPoly.btnFillColorClick(Sender: TObject);
begin
 if ColorDialog1.Execute then clrbxFillColor.Selected:=ColorDialog1.Color;
end;

end.
