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
  u_CommonFormAndFrameParents,
  u_ResStrings,
  i_MarkPicture,
  i_MarksSimple,
  i_MarkCategory,
  u_MarksDbGUIHelper,
  fr_MarkDescription,
  fr_LonLat,
  t_GeoTypes;

type
  TfrmMarkEditPoint = class(TCommonFormParent)
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
    lblCategory: TLabel;
    CBKateg: TComboBox;
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
  private
    FPic: IMarkPicture;
    frMarkDescription: TfrMarkDescription;
    frLonLatPoint: TfrLonLat;
    FMarkDBGUI: TMarksDbGUIHelper;
    FCategoryList: IInterfaceList;
    FCategory: ICategory;
    procedure DrawFromMarkIcons(canvas:TCanvas; APic: IMarkPicture; bound:TRect);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function EditMark(AMark: IMarkPoint; AMarkDBGUI: TMarksDbGUIHelper): IMarkPoint;
    procedure RefreshTranslation; override;
  end;

var
  frmMarkEditPoint: TfrmMarkEditPoint;

implementation

uses
  Math,
  u_GlobalState;

{$R *.dfm}

function TfrmMarkEditPoint.EditMark(AMark: IMarkPoint; AMarkDBGUI: TMarksDbGUIHelper): IMarkPoint;
var
  VLastUsedCategoryName:string;
  i: Integer;
  VCategory: ICategory;
  VPicCount: Integer;
  VColCount: Integer;
  VRowCount: Integer;
  VPictureList: IMarkPictureList;
  VLonLat:TDoublePoint;
begin
  FMarkDBGUI := AMarkDBGUI;
  frMarkDescription.Description:='';
  VLastUsedCategoryName:=CBKateg.Text;
  FCategoryList := FMarkDBGUI.MarksDB.CategoryDB.GetCategoriesList;
  try
    FMarkDBGUI.CategoryListToStrings(FCategoryList, CBKateg.Items);
    CBKateg.Sorted:=true;
    CBKateg.Text:=VLastUsedCategoryName;
    VPictureList := FMarkDBGUI.MarkPictureList;
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
    chkVisible.Checked:= FMarkDBGUI.MarksDB.MarksDb.GetMarkVisible(AMark);
    FCategory := AMark.Category;
    if FCategory <> nil then begin
      for i := 0 to CBKateg.Items.Count - 1 do begin
        VCategory := ICategory(Pointer(CBKateg.Items.Objects[i]));
        if VCategory <> nil then begin
          if VCategory.IsSame(FCategory) then begin
            CBKateg.ItemIndex := i;
            Break;
          end;
        end;
      end;
    end else begin
      CBKateg.ItemIndex := -1;
    end;
    if AMark.IsNew then begin
      Caption:=SAS_STR_AddNewMark;
    end else begin
      Caption:=SAS_STR_EditMark;
    end;
    DrawFromMarkIcons(imgIcon.canvas, AMark.Pic, bounds(4,4,36,36));
    frLonLatPoint.LonLat := AMark.Point;
    if ShowModal=mrOk then begin
      VLonLat := frLonLatPoint.LonLat;
      Result := AMarkDBGUI.MarksDB.MarksDb.Factory.ModifyPoint(
        AMark,
        edtName.Text,
        chkVisible.Checked,
        FPic,
        FCategory,
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
    FCategoryList := nil;
  end;
end;

procedure TfrmMarkEditPoint.btnOkClick(Sender: TObject);
var
  VIndex: Integer;
  VCategoryText: string;
begin
  FCategory := nil;
  VCategoryText := CBKateg.Text;
  VIndex := CBKateg.ItemIndex;
  if VIndex < 0 then begin
    VIndex:= CBKateg.Items.IndexOf(VCategoryText);
  end;
  if VIndex >= 0 then begin
    FCategory := ICategory(Pointer(CBKateg.Items.Objects[VIndex]));
  end;
  if FCategory = nil then begin
    FCategory := FMarkDBGUI.AddKategory(VCategoryText);
  end;
  ModalResult := mrOk;
end;

procedure TfrmMarkEditPoint.FormShow(Sender: TObject);
begin
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

procedure TfrmMarkEditPoint.btnShadowColorClick(Sender: TObject);
begin
 if ColorDialog1.Execute then clrbxShadowColor.Selected:=ColorDialog1.Color;
end;

constructor TfrmMarkEditPoint.Create(AOwner: TComponent);
begin
  inherited;
  frMarkDescription := TfrMarkDescription.Create(nil);
  frLonLatPoint :=
    TfrLonLat.Create(
      nil,
      GState.MainFormConfig.ViewPortState,
      GState.ValueToStringConverterConfig,
      tssCenter
    );
end;

destructor TfrmMarkEditPoint.Destroy;
begin
  FreeAndNil(frMarkDescription);
  FreeAndNil(frLonLatPoint);
  inherited;
end;

procedure TfrmMarkEditPoint.DrawFromMarkIcons(canvas:TCanvas; APic: IMarkPicture; bound:TRect);
var
  Bitmap: TCustomBitmap32;
  Bitmap2: TBitmap32;
  wdth:integer;
begin
  canvas.FillRect(bound);
  if APic <> nil then begin
    wdth:=min(bound.Right-bound.Left,bound.Bottom-bound.Top);
    Bitmap:=TCustomBitmap32.Create;
    try
      APic.LoadBitmap(Bitmap);
      Bitmap.DrawMode:=dmBlend;
      Bitmap.Resampler:=TKernelResampler.Create;
      TKernelResampler(Bitmap.Resampler).Kernel:=TLinearKernel.Create;

      Bitmap2:=TBitmap32.Create;
      try
        Bitmap2.SetSize(wdth,wdth);
        Bitmap2.Clear(clWhite32);
        Bitmap2.Draw(Bounds(0, 0, wdth,wdth), Bounds(0, 0, Bitmap.Width,Bitmap.Height),Bitmap);
        Bitmap2.DrawTo(canvas.Handle, bound, Bounds(0, 0, Bitmap2.Width,Bitmap2.Height));
      finally
        Bitmap2.Free;
      end;
    finally
      Bitmap.Free;
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
  VPictureList := FMarkDBGUI.MarkPictureList;
  if i < VPictureList.Count then
    DrawFromMarkIcons(drwgrdIcons.Canvas, VPictureList.Get(i), drwgrdIcons.CellRect(ACol,ARow));
end;

procedure TfrmMarkEditPoint.imgIconMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 drwgrdIcons.Visible:=not(drwgrdIcons.Visible);
 if drwgrdIcons.Visible then drwgrdIcons.SetFocus;
end;

procedure TfrmMarkEditPoint.RefreshTranslation;
begin
  inherited;
  frLonLatPoint.RefreshTranslation;
  frMarkDescription.RefreshTranslation;
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
 VPictureList := FMarkDBGUI.MarkPictureList;
 if (ARow>-1)and(ACol>-1) and (i < VPictureList.Count) then begin
   FPic := VPictureList.Get(i);
   imgIcon.Canvas.FillRect(imgIcon.Canvas.ClipRect);
   DrawFromMarkIcons(imgIcon.Canvas, FPic, bounds(5,5,36,36));
   drwgrdIcons.Visible:=false;
 end;
end;

end.
