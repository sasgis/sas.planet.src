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
  i_VectorItemsFactory,
  i_VectorDataItemSimple,
  i_Appearance,
  i_AppearanceOfMarkFactory,
  i_MarkFactory,
  i_MarkCategoryDB,
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
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure imgIconMouseDown(Sender: TObject);
  private
    FGeometryFactory: IGeometryLonLatFactory;
    FSourceMark: IVectorDataItemPoint;
    FCategoryDB: IMarkCategoryDB;
    FAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
    FPictureList: IMarkPictureList;
    FMarkFactory: IMarkFactory;
    frMarkDescription: TfrMarkDescription;
    frLonLatPoint: TfrLonLat;
    frMarkCategory: TfrMarkCategorySelectOrAdd;
    frSelectPicture: TfrPictureSelectFromList;
    frSelectedPicture: TfrSelectedPicture;
    procedure SelectImageFromList(Sender: TObject);
    function MakeAppearance: IAppearance;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMediaPath: IPathConfig;
      const AGeometryFactory: IGeometryLonLatFactory;
      const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
      const AMarkFactory: IMarkFactory;
      const ACategoryDB: IMarkCategoryDB;
      const APictureList: IMarkPictureList;
      const AViewPortState: ILocalCoordConverterChangeable;
      const AValueToStringConverterConfig: IValueToStringConverterConfig
    ); reintroduce;
    destructor Destroy; override;
    function EditMark(
      const AMark: IVectorDataItemPoint;
      const AIsNewMark: Boolean;
      var AVisible: Boolean
    ): IVectorDataItemPoint;
  end;

implementation

uses
  t_GeoTypes,
  i_MarkTemplate,
  i_AppearanceOfVectorItem,
  i_GeometryLonLat,
  i_Category,
  i_MarkFactoryConfig,
  u_ResStrings;

{$R *.dfm}

constructor TfrmMarkEditPoint.Create(
  const ALanguageManager: ILanguageManager;
  const AMediaPath: IPathConfig;
  const AGeometryFactory: IGeometryLonLatFactory;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
  const AMarkFactory: IMarkFactory;
  const ACategoryDB: IMarkCategoryDB;
  const APictureList: IMarkPictureList;
  const AViewPortState: ILocalCoordConverterChangeable;
  const AValueToStringConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(ALanguageManager);
  FCategoryDB := ACategoryDB;
  FGeometryFactory := AGeometryFactory;
  FAppearanceOfMarkFactory := AAppearanceOfMarkFactory;
  FMarkFactory := AMarkFactory;
  FPictureList := APictureList;

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
      APictureList,
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

function TfrmMarkEditPoint.EditMark(
  const AMark: IVectorDataItemPoint;
  const AIsNewMark: Boolean;
  var AVisible: Boolean
): IVectorDataItemPoint;
var
  VLonLat:TDoublePoint;
  VAppearanceCaption: IAppearancePointCaption;
  VAppearanceIcon: IAppearancePointIcon;
  VPicIndex: Integer;
  VPic: IMarkPicture;
  VCategory: ICategory;
  VPoint: IGeometryLonLatPoint;
  VMarkWithCategory: IVectorDataItemWithCategory;
begin
  FSourceMark := AMark;
  frMarkDescription.Description:='';
  frSelectPicture.Visible := False;
  frSelectPicture.Parent := Self;

  frSelectedPicture.Parent := pnlImage;

  if Supports(AMark.Appearance, IAppearancePointIcon, VAppearanceIcon) then begin
    if Assigned(VAppearanceIcon.Pic) then begin
      VPic := VAppearanceIcon.Pic;
    end else begin
      if VAppearanceIcon.PicName = '' then begin
        VPic := FPictureList.GetDefaultPicture;
      end else begin
        VPicIndex := FPictureList.GetIndexByName(VAppearanceIcon.PicName);
        if VPicIndex >= 0 then begin
          VPic := FPictureList.Get(VPicIndex);
        end else begin
          VPic := nil;
        end;
      end;
    end;
    frSelectedPicture.Picture := VPic;
    frSelectPicture.Picture := VPic;
    seIconSize.Value := VAppearanceIcon.MarkerSize;
  end else begin
    frSelectedPicture.Picture := nil;
    seIconSize.Value := 0;
  end;

  edtName.Text:=AMark.Name;
  frMarkDescription.Description:=AMark.Desc;

  if Supports(AMark.Appearance, IAppearancePointCaption, VAppearanceCaption) then begin
    seFontSize.Value := VAppearanceCaption.FontSize;
    seTransp.Value := 100-round(AlphaComponent(VAppearanceCaption.TextColor)/255*100);
    clrbxTextColor.Selected := WinColor(VAppearanceCaption.TextColor);
    clrbxShadowColor.Selected := WinColor(VAppearanceCaption.TextBgColor);
  end else begin
    seFontSize.Value := 0;
    seTransp.Value := 0;
    clrbxTextColor.Selected := WinColor(clBlack32);
    clrbxShadowColor.Selected := WinColor(clWhite32);
  end;
  chkVisible.Checked:= AVisible;
  VCategory := nil;
  if Supports(AMark, IVectorDataItemWithCategory, VMarkWithCategory) then begin
    VCategory := VMarkWithCategory.Category;
  end;
  frMarkCategory.Init(VCategory);
  try
    if AIsNewMark then begin
      Caption:=SAS_STR_AddNewMark;
    end else begin
      Caption:=SAS_STR_EditMark;
    end;
    frLonLatPoint.LonLat := AMark.Point.Point;
    Self.PopupParent := Application.MainForm;
    if ShowModal=mrOk then begin
      VLonLat := frLonLatPoint.LonLat;
      VPoint := FGeometryFactory.CreateLonLatPoint(VLonLat);
      Result :=
        FMarkFactory.CreatePoint(
          VPoint,
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
    FSourceMark := nil;
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
begin
  if MessageBox(handle, pchar('Set as default for new marks?'), pchar(SAS_MSG_coution), 36) = IDYES then begin
    VConfig := FMarkFactory.Config.PointTemplateConfig;
    VTemplate :=
      VConfig.CreateTemplate(
        MakeAppearance,
        frMarkCategory.GetCategory
      );
    VConfig.DefaultTemplate := VTemplate;
  end;
end;

procedure TfrmMarkEditPoint.btnShadowColorClick(Sender: TObject);
begin
 if ColorDialog1.Execute then clrbxShadowColor.Selected:=ColorDialog1.Color;
end;

procedure TfrmMarkEditPoint.FormCloseQuery(Sender: TObject; var CanClose:
    Boolean);
begin
  if ModalResult = mrOk then begin
    CanClose := frLonLatPoint.Validate;
  end;
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

function TfrmMarkEditPoint.MakeAppearance: IAppearance;
var
  VPic: IMarkPicture;
  VPicName: string;
  VAppearanceIcon: IAppearancePointIcon;
begin
  VPic := frSelectedPicture.Picture;
  VPicName := '';
  if Assigned(VPic) then begin
    VPicName := VPic.GetName;
  end else begin
    if Assigned(FSourceMark) then begin
      if Supports(FSourceMark.Appearance, IAppearancePointIcon, VAppearanceIcon) then begin
        VPicName := VAppearanceIcon.PicName;
      end;
    end;
  end;

  Result :=
    FAppearanceOfMarkFactory.CreatePointAppearance(
      SetAlpha(Color32(clrbxTextColor.Selected),round(((100-seTransp.Value)/100)*256)),
      SetAlpha(Color32(clrbxShadowColor.Selected),round(((100-seTransp.Value)/100)*256)),
      seFontSize.Value,
      VPicName,
      frSelectPicture.Picture,
      seIconSize.Value
    );
end;

procedure TfrmMarkEditPoint.SelectImageFromList(Sender: TObject);
begin
  frSelectPicture.Visible := False;
  frSelectedPicture.Picture := frSelectPicture.Picture;
end;

end.
