{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit frm_ImportConfigEdit;

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
  i_MarkCategory,
  i_LanguageManager,
  i_ImportConfig,
  i_MarksDb,
  i_MarkCategoryDB,
  fr_MarkCategorySelectOrAdd;

type
  TfrmImportConfigEdit = class(TFormWitghLanguageManager)
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
    pnlCategory: TPanel;
    procedure btnPointTextColorClick(Sender: TObject);
    procedure btnPointShadowColorClick(Sender: TObject);
    procedure cbbPointIconDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure btnLineColorClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnPolyLineColorClick(Sender: TObject);
    procedure btnPolyFillColorClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    frMarkCategory: TfrMarkCategorySelectOrAdd;
    FCategoryDB: IMarkCategoryDB;
    FMarksDb: IMarksDb;
  public
    constructor Create(
      ALanguageManager: ILanguageManager;
      ACategoryDB: IMarkCategoryDB;
      AMarksDb: IMarksDb
    ); reintroduce;
    destructor Destroy; override;
    function GetImportConfig: IImportConfig;
    procedure RefreshTranslation; override;
  end;

implementation

uses
  i_MarkTemplate,
  i_MarkPicture,
  u_ImportConfig;

{$R *.dfm}

constructor TfrmImportConfigEdit.Create(
  ALanguageManager: ILanguageManager;
  ACategoryDB: IMarkCategoryDB;
  AMarksDb: IMarksDb
);
begin
  inherited Create(ALanguageManager);
  FMarksDb := AMarksDb;
  FCategoryDB := ACategoryDB;

  frMarkCategory :=
    TfrMarkCategorySelectOrAdd.Create(
      nil,
      FCategoryDB
    );
end;

destructor TfrmImportConfigEdit.Destroy;
begin
  FreeAndNil(frMarkCategory);
  inherited;
end;

procedure TfrmImportConfigEdit.btnPointTextColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then clrbxPointTextColor.Selected:=ColorDialog1.Color;
end;

procedure TfrmImportConfigEdit.btnPointShadowColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then clrbxPointShadowColor.Selected:=ColorDialog1.Color;
end;

procedure TfrmImportConfigEdit.btnLineColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then clrbxLineColor.Selected:=ColorDialog1.Color;
end;

procedure TfrmImportConfigEdit.btnPolyLineColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then clrbxPolyLineColor.Selected:=ColorDialog1.Color;
end;

procedure TfrmImportConfigEdit.btnPolyFillColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then clrbxPolyFillColor.Selected:=ColorDialog1.Color;
end;

procedure TfrmImportConfigEdit.cbbPointIconDrawItem(Control: TWinControl; Index: Integer;
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

function TfrmImportConfigEdit.GetImportConfig: IImportConfig;
var
  VIndex: Integer;
  VPic: IMarkPicture;
  VMarkTemplatePoint: IMarkTemplatePoint;
  VMarkTemplateLine: IMarkTemplateLine;
  VMarkTemplatePoly: IMarkTemplatePoly;
  VPictureList: IMarkPictureList;
  i: Integer;
  VCategory: ICategory;
begin
  VPictureList := FMarksDb.Factory.MarkPictureList;
  cbbPointIcon.Items.Clear;
  for i := 0 to VPictureList.Count - 1 do begin
    cbbPointIcon.Items.AddObject(VPictureList.GetName(i), Pointer(VPictureList.Get(i)));
  end;
  try
    cbbPointIcon.Repaint;
    cbbPointIcon.ItemIndex:=0;

    frMarkCategory.Init(nil);
    try
      if ShowModal = mrOk then begin
        if not chkPointIgnore.Checked then begin
          VIndex := cbbPointIcon.ItemIndex;
          if VIndex < 0 then begin
            VPic := nil;
          end else begin
            VPic := IMarkPicture(Pointer(cbbPointIcon.Items.Objects[VIndex]));
          end;
          VCategory := frMarkCategory.GetCategory;
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
          VMarkTemplateLine :=
            FMarksDb.Factory.Config.LineTemplateConfig.CreateTemplate(
              VCategory,
              SetAlpha(Color32(clrbxLineColor.Selected),round(((100-seLineTransp.Value)/100)*256)),
              seLineWidth.Value
            );
        end;
        VMarkTemplatePoly := nil;
        if not chkPolyIgnore.Checked then begin
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
      end else begin
        Result := nil;
      end;
    finally
      frMarkCategory.Clear;
    end;
  finally
    cbbPointIcon.Items.Clear;
  end;
end;

procedure TfrmImportConfigEdit.RefreshTranslation;
begin
  inherited;
  frMarkCategory.RefreshTranslation;
end;

procedure TfrmImportConfigEdit.btnOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmImportConfigEdit.FormShow(Sender: TObject);
begin
  frMarkCategory.Parent := pnlCategory;
end;

end.
