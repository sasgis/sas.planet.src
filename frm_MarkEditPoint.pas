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

unit frm_MarkEditPoint;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  Buttons,
  Spin,
  GR32,
  u_CommonFormAndFrameParents,
  i_LanguageManager,
  i_PathConfig,
  i_LocalCoordConverterChangeable,
  i_ValueToStringConverter,
  i_MarkPicture,
  i_MarksSimple,
  i_MarkCategoryDB,
  i_MarksDb,
  fr_MarkDescription,
  fr_LonLat,
  fr_PictureSelectFromList,
  fr_SelectedPicture,
  fr_MarkCategorySelectOrAdd;

type
  TfrmMarkEditPoint = class(TFormWitghLanguageManager)
    edtName: TEdit;
    lblName: TLabel;
    btnOk: TButton;
    btnCancel: TButton;
    Bevel1: TBevel;
    chkVisible: TCheckBox;
    clrbxTextColor: TColorBox;
    lblTextColor: TLabel;
    lblShadowColor: TLabel;
    seFontSize: TSpinEdit;
    lblFontSize: TLabel;
    clrbxShadowColor: TColorBox;
    lblIconSize: TLabel;
    seIconSize: TSpinEdit;
    seTransp: TSpinEdit;
    lblTransp: TLabel;
    btnTextColor: TSpeedButton;
    btnShadowColor: TSpeedButton;
    ColorDialog1: TColorDialog;
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
    btnSetAsTemplate: TButton;
    procedure btnOkClick(Sender: TObject);
    procedure btnTextColorClick(Sender: TObject);
    procedure btnShadowColorClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnSetAsTemplateClick(Sender: TObject);
    procedure imgIconMouseDown(Sender: TObject);
  private
    FCategoryDB: IMarkCategoryDB;
    FMarksDb: IMarksDb;
    frMarkDescription: TfrMarkDescription;
    frLonLatPoint: TfrLonLat;
    frMarkCategory: TfrMarkCategorySelectOrAdd;
    frSelectPicture: TfrPictureSelectFromList;
    frSelectedPicture: TfrSelectedPicture;
    procedure SelectImageFromList(Sender: TObject);
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMediaPath: IPathConfig;
      const ACategoryDB: IMarkCategoryDB;
      const AMarksDb: IMarksDb;
      const AViewPortState: ILocalCoordConverterChangeable;
      const AValueToStringConverterConfig: IValueToStringConverterConfig
    ); reintroduce;
    destructor Destroy; override;
    function EditMark(const AMark: IMarkPoint; AIsNewMark: Boolean): IMarkPoint;
  end;

implementation

uses
  t_GeoTypes,
  i_MarkTemplate,
  i_MarksFactoryConfig,
  u_ResStrings;

{$R *.dfm}

constructor TfrmMarkEditPoint.Create(
  const ALanguageManager: ILanguageManager;
  const AMediaPath: IPathConfig;
  const ACategoryDB: IMarkCategoryDB;
  const AMarksDb: IMarksDb;
  const AViewPortState: ILocalCoordConverterChangeable;
  const AValueToStringConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(ALanguageManager);
  FMarksDb := AMarksDb;
  FCategoryDB := ACategoryDB;

  frMarkDescription := TfrMarkDescription.Create(ALanguageManager, AMediaPath);
  frLonLatPoint :=
    TfrLonLat.Create(
      ALanguageManager,
      AViewPortState,
      AValueToStringConverterConfig,
      tssCenter
    );
  frMarkCategory :=
    TfrMarkCategorySelectOrAdd.Create(
      ALanguageManager,
      FCategoryDB
    );
  frSelectPicture :=
    TfrPictureSelectFromList.Create(
      ALanguageManager,
      FMarksDb.Factory.MarkPictureList,
      Self.SelectImageFromList
    );
  frSelectedPicture := TfrSelectedPicture.Create(ALanguageManager, Self.imgIconMouseDown);
end;

destructor TfrmMarkEditPoint.Destroy;
begin
  FreeAndNil(frMarkDescription);
  FreeAndNil(frLonLatPoint);
  FreeAndNil(frMarkCategory);
  FreeAndNil(frSelectPicture);
  FreeAndNil(frSelectedPicture);
  inherited;
end;

function TfrmMarkEditPoint.EditMark(const AMark: IMarkPoint; AIsNewMark: Boolean): IMarkPoint;
var
  VLonLat:TDoublePoint;
begin
  frMarkDescription.Description:='';
  frSelectPicture.Visible := False;
  frSelectPicture.Parent := Self;
  frSelectPicture.Picture := AMark.Pic;

  frSelectedPicture.Parent := pnlImage;
  frSelectedPicture.Picture := AMark.Pic;

  edtName.Text:=AMark.Name;
  frMarkDescription.Description:=AMark.Desc;
  seFontSize.Value:=AMark.FontSize;
  seIconSize.Value:=AMark.MarkerSize;
  seTransp.Value:=100-round(AlphaComponent(AMark.TextColor)/255*100);
  clrbxTextColor.Selected:=WinColor(AMark.TextColor);
  clrbxShadowColor.Selected:=WinColor(AMark.TextBgColor);
  chkVisible.Checked:= FMarksDb.GetMarkVisible(AMark);
  frMarkCategory.Init(AMark.Category);
  try
    if AIsNewMark then begin
      Caption:=SAS_STR_AddNewMark;
    end else begin
      Caption:=SAS_STR_EditMark;
    end;
    frLonLatPoint.LonLat := AMark.Point;
    Self.PopupParent := Application.MainForm;
    if ShowModal=mrOk then begin
      VLonLat := frLonLatPoint.LonLat;
      Result := FMarksDb.Factory.ModifyPoint(
        AMark,
        edtName.Text,
        chkVisible.Checked,
        frSelectPicture.Picture,
        frMarkCategory.GetCategory,
        frMarkDescription.Description,
        VLonLat,
        SetAlpha(Color32(clrbxTextColor.Selected),round(((100-seTransp.Value)/100)*256)),
        SetAlpha(Color32(clrbxShadowColor.Selected),round(((100-seTransp.Value)/100)*256)),
        seFontSize.Value,
        seIconSize.Value
      );
    end else begin
      Result := nil;
    end;
  finally
    frMarkCategory.Clear;
  end;
end;

procedure TfrmMarkEditPoint.btnOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmMarkEditPoint.FormShow(Sender: TObject);
begin
  frMarkCategory.Parent := pnlCategory;
  frLonLatPoint.Parent := pnlLonLat;
  frMarkDescription.Parent := pnlDescription;
  edtName.SetFocus;
  flwpnlTextColor.Realign;
  flwpnlShadowColor.Realign;
end;

procedure TfrmMarkEditPoint.btnTextColorClick(Sender: TObject);
begin
 if ColorDialog1.Execute then clrbxTextColor.Selected:=ColorDialog1.Color;
end;

procedure TfrmMarkEditPoint.btnSetAsTemplateClick(Sender: TObject);
var
  VConfig: IMarkPointTemplateConfig;
  VTemplate: IMarkTemplatePoint;
  VPicName: string;
begin
  if MessageBox(handle, pchar('Set as default for new marks?'), pchar(SAS_MSG_coution), 36) = IDYES then begin
    VConfig := FMarksDb.Factory.Config.PointTemplateConfig;
    VPicName := '';
    if frSelectPicture.Picture <> nil then begin
      VPicName := frSelectPicture.Picture.GetName;
    end;
    VTemplate :=
      VConfig.CreateTemplate(
        VPicName,
        frMarkCategory.GetCategory,
        SetAlpha(Color32(clrbxTextColor.Selected),round(((100-seTransp.Value)/100)*256)),
        SetAlpha(Color32(clrbxShadowColor.Selected),round(((100-seTransp.Value)/100)*256)),
        seFontSize.Value,
        seIconSize.Value
      );
    VConfig.DefaultTemplate := VTemplate;
  end;
end;

procedure TfrmMarkEditPoint.btnShadowColorClick(Sender: TObject);
begin
 if ColorDialog1.Execute then clrbxShadowColor.Selected:=ColorDialog1.Color;
end;

procedure TfrmMarkEditPoint.imgIconMouseDown(Sender: TObject);
begin
  if frSelectPicture.Visible then begin
    frSelectPicture.Visible := False;
  end else begin
    frSelectPicture.Left := 5;
    frSelectPicture.Width := Self.ClientWidth - frSelectPicture.Left - 5;
    frSelectPicture.Top := pnlImage.Top + pnlImage.Height;
    frSelectPicture.Height := Self.ClientHeight - frSelectPicture.Top - 5;
    frSelectPicture.Visible := True;
    frSelectPicture.SetFocus;
  end;
end;

procedure TfrmMarkEditPoint.SelectImageFromList(Sender: TObject);
begin
  frSelectPicture.Visible := False;
  frSelectedPicture.Picture := frSelectPicture.Picture;
end;

end.
