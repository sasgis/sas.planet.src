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
  u_CommonFormAndFrameParents,
  i_MarksDB;

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
    procedure btnPointTextColorClick(Sender: TObject);
    procedure btnPointShadowColorClick(Sender: TObject);
    procedure btnLineColorClick(Sender: TObject);
    procedure btnPolyLineColorClick(Sender: TObject);
    procedure btnPolyFillColorClick(Sender: TObject);
    procedure cbbPointIconDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  private
  protected
    { Private declarations }
  public
    procedure Init(const AMarksDb: IMarksDb);
    procedure Clear;
  end;

implementation

uses
  i_MarkPicture,
  i_BitmapMarker;

{$R *.dfm}
procedure TfrMarksGeneralOptions.Init(const AMarksDb: IMarksDb);
var
  VPictureList: IMarkPictureList;
  i: Integer;
begin
  VPictureList := AMarksDb.Factory.MarkPictureList;
  cbbPointIcon.Items.Clear;
  for i := 0 to VPictureList.Count - 1 do begin
    cbbPointIcon.Items.AddObject(VPictureList.GetName(i), Pointer(VPictureList.Get(i)));
  end;
  cbbPointIcon.Repaint;
  cbbPointIcon.ItemIndex:=0;
end;

procedure TfrMarksGeneralOptions.Clear;
begin
  cbbPointIcon.Items.Clear;
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
