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
  graphics,
  ExtCtrls,
  StdCtrls,
  Grids,
  Buttons,
  Spin,
  GR32,
  GR32_Resamplers,
  t_GeoTypes,
  u_CommonFormAndFrameParents,
  u_ResStrings,
  i_LanguageManager,
  i_PathConfig,
  i_ViewPortState,
  i_ValueToStringConverter,
  i_MarkPicture,
  i_MarksSimple,
  i_MarkCategory,
  i_MarkCategoryDB,
  i_MarksDb,
  fr_MarkDescription,
  fr_LonLat,
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
    drwgrdIcons: TDrawGrid;
    imgIcon: TImage;
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
    procedure drwgrdIconsDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure imgIconMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure drwgrdIconsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure btnSetAsTemplateClick(Sender: TObject);
  private
    FCategoryDB: IMarkCategoryDB;
    FMarksDb: IMarksDb;
    FPic: IMarkPicture;
    frMarkDescription: TfrMarkDescription;
    frLonLatPoint: TfrLonLat;
    frMarkCategory: TfrMarkCategorySelectOrAdd;
    procedure DrawFromMarkIcons(
      canvas: TCanvas;
      const APic: IMarkPicture;
      const bound: TRect
    );
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMediaPath: IPathConfig;
      const ACategoryDB: IMarkCategoryDB;
      const AMarksDb: IMarksDb;
      const AViewPortState: IViewPortState;
      const AValueToStringConverterConfig: IValueToStringConverterConfig
    ); reintroduce;
    destructor Destroy; override;
    function EditMark(const AMark: IMarkPoint): IMarkPoint;
  end;

implementation

uses
  Math,
  i_BitmapMarker,
  i_MarkTemplate,
  i_MarksFactoryConfig;

{$R *.dfm}

constructor TfrmMarkEditPoint.Create(
  const ALanguageManager: ILanguageManager;
  const AMediaPath: IPathConfig;
  const ACategoryDB: IMarkCategoryDB;
  const AMarksDb: IMarksDb;
  const AViewPortState: IViewPortState;
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
end;

destructor TfrmMarkEditPoint.Destroy;
begin
  FreeAndNil(frMarkDescription);
  FreeAndNil(frLonLatPoint);
  FreeAndNil(frMarkCategory);
  inherited;
end;

function TfrmMarkEditPoint.EditMark(const AMark: IMarkPoint): IMarkPoint;
var
  VPicCount: Integer;
  VColCount: Integer;
  VRowCount: Integer;
  VPictureList: IMarkPictureList;
  VLonLat:TDoublePoint;
begin
  frMarkDescription.Description:='';
  VPictureList := FMarksDb.Factory.MarkPictureList;
  VPicCount := VPictureList.Count;
  VColCount := drwgrdIcons.ColCount;
  VRowCount := VPicCount div VColCount;
  if (VPicCount mod VColCount) > 0 then begin
    Inc(VRowCount);
  end;
  drwgrdIcons.RowCount := VRowCount;
  drwgrdIcons.Repaint;
  FPic := AMark.Pic;
  edtName.Text:=AMark.name;
  frMarkDescription.Description:=AMark.Desc;
  seFontSize.Value:=AMark.FontSize;
  seIconSize.Value:=AMark.MarkerSize;
  seTransp.Value:=100-round(AlphaComponent(AMark.TextColor)/255*100);
  clrbxTextColor.Selected:=WinColor(AMark.TextColor);
  clrbxShadowColor.Selected:=WinColor(AMark.TextBgColor);
  chkVisible.Checked:= FMarksDb.GetMarkVisible(AMark);
  frMarkCategory.Init(AMark.Category);
  try
    if FMarksDb.GetMarkIsNew(AMark) then begin
      Caption:=SAS_STR_AddNewMark;
    end else begin
      Caption:=SAS_STR_EditMark;
    end;
    DrawFromMarkIcons(imgIcon.canvas, AMark.Pic, bounds(4,4,36,36));
    frLonLatPoint.LonLat := AMark.Point;
    if ShowModal=mrOk then begin
      VLonLat := frLonLatPoint.LonLat;
      Result := FMarksDb.Factory.ModifyPoint(
        AMark,
        edtName.Text,
        chkVisible.Checked,
        FPic,
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
  drwgrdIcons.Visible:=false;
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
    VConfig := FMarksDb.Factory.Config.PointTemplateConfig;
    VTemplate :=
      VConfig.CreateTemplate(
        FPic,
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

procedure TfrmMarkEditPoint.DrawFromMarkIcons(
  canvas: TCanvas;
  const APic: IMarkPicture;
  const bound:TRect
);
var
  VBitmap: TBitmap32;
  wdth:integer;
  VResampler: TCustomResampler;
  VMarker: IBitmapMarker;
begin
  canvas.FillRect(bound);
  if APic <> nil then begin
    VMarker := APic.GetMarker;
    if VMarker <> nil then begin
      wdth:=min(bound.Right-bound.Left,bound.Bottom-bound.Top);
      VBitmap:=TBitmap32.Create;
      try
        VBitmap.SetSize(wdth,wdth);
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
        VBitmap.DrawTo(canvas.Handle, bound, VBitmap.BoundsRect);
      finally
        VBitmap.Free;
      end;
    end;
  end;
end;

procedure TfrmMarkEditPoint.drwgrdIconsDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  i:Integer;
  VPictureList: IMarkPictureList;
begin
  i:=(Arow*drwgrdIcons.ColCount)+ACol;
  VPictureList := FMarksDb.Factory.MarkPictureList;
  if i < VPictureList.Count then
    DrawFromMarkIcons(drwgrdIcons.Canvas, VPictureList.Get(i), drwgrdIcons.CellRect(ACol,ARow));
end;

procedure TfrmMarkEditPoint.imgIconMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 drwgrdIcons.Visible:=not(drwgrdIcons.Visible);
 if drwgrdIcons.Visible then drwgrdIcons.SetFocus;
end;

procedure TfrmMarkEditPoint.drwgrdIconsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i:integer;
  ACol,ARow: Integer;
  VPictureList: IMarkPictureList;
begin
 drwgrdIcons.MouseToCell(X,Y,ACol,ARow);
 i:=(ARow*drwgrdIcons.ColCount)+ACol;
 VPictureList := FMarksDb.Factory.MarkPictureList;
 if (ARow>-1)and(ACol>-1) and (i < VPictureList.Count) then begin
   FPic := VPictureList.Get(i);
   imgIcon.Canvas.FillRect(imgIcon.Canvas.ClipRect);
   DrawFromMarkIcons(imgIcon.Canvas, FPic, bounds(5,5,36,36));
   drwgrdIcons.Visible:=false;
 end;
end;

end.
