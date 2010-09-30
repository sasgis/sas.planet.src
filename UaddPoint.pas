unit UaddPoint;

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
  Mask,
  Grids,
  Buttons,
  Spin,
  rxToolEdit,
  rxCurrEdit,
  GR32,
  GR32_Resamplers,
  u_CommonFormAndFrameParents,
  UResStrings,
  UMarksExplorer,
  ugeofun,
  u_MarksSimple,
  fr_MarkDescription,
  t_GeoTypes;

type
  TFaddPoint = class(TCommonFormParent)
    EditName: TEdit;
    Label1: TLabel;
    Badd: TButton;
    Button2: TButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel5: TBevel;
    CheckBox2: TCheckBox;
    lat_ns: TComboBox;
    Lat1: TCurrencyEdit;
    lat2: TCurrencyEdit;
    lat3: TCurrencyEdit;
    lon1: TCurrencyEdit;
    lon2: TCurrencyEdit;
    lon3: TCurrencyEdit;
    Lon_we: TComboBox;
    Label21: TLabel;
    Label22: TLabel;
    ColorBox1: TColorBox;
    Label3: TLabel;
    Label4: TLabel;
    SpinEdit1: TSpinEdit;
    Label5: TLabel;
    ColorBox2: TColorBox;
    Label6: TLabel;
    SpinEdit2: TSpinEdit;
    SEtransp: TSpinEdit;
    Label7: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    ColorDialog1: TColorDialog;
    Label8: TLabel;
    CBKateg: TComboBox;
    DrawGrid1: TDrawGrid;
    Image1: TImage;
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
    procedure BaddClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawGrid1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
  private
    FMark: TMarkFull;
    frMarkDescription: TfrMarkDescription;
    procedure DrawFromMarkIcons(canvas:TCanvas;index:integer;bound:TRect);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function EditMark(AMark: TMarkFull):boolean;
    procedure RefreshTranslation; override;
  end;

var
  FaddPoint: TFaddPoint;

implementation

uses
  Math,
  u_GlobalState,
  u_MarksReadWriteSimple;

{$R *.dfm}

function TFaddPoint.EditMark(AMark: TMarkFull): boolean;
var
  VLastUsedCategoryName:string;
  i: Integer;
  VCategory: TCategoryId;
  VId: integer;
  DMS:TDMS;
begin
  FMark := AMark;
  frMarkDescription.Description:='';
  EditName.Text:=SAS_STR_NewMark;
  VLastUsedCategoryName:=CBKateg.Text;
  Kategory2StringsWithObjects(CBKateg.Items);
  CBKateg.Sorted:=true;
  CBKateg.Text:=VLastUsedCategoryName;
  DrawGrid1.RowCount:=(GState.MarkIcons.Count div DrawGrid1.ColCount);
  if (GState.MarkIcons.Count mod DrawGrid1.ColCount)>0 then begin
  DrawGrid1.RowCount:=DrawGrid1.RowCount+1;
  end;
  DrawGrid1.Repaint;
  if FMark.id < 0 then begin
    if GState.MarkIcons.Count>0 then begin
      DrawFromMarkIcons(Image1.canvas,0,bounds(4,4,36,36));
    end;
    Caption:=SAS_STR_AddNewMark;
    Badd.Caption:=SAS_STR_Add;
    CheckBox2.Checked:=true;
  end else begin
    Caption:=SAS_STR_EditMark;
    Badd.Caption:=SAS_STR_Edit;
    EditName.Text:=FMark.name;
    frMarkDescription.Description:=FMark.Desc;
    SpinEdit1.Value:=FMark.Scale1;
    SpinEdit2.Value:=FMark.Scale2;
    SEtransp.Value:=100-round(AlphaComponent(FMark.Color1)/255*100);
    ColorBox1.Selected:=WinColor(FMark.Color1);
    ColorBox2.Selected:=WinColor(FMark.Color2);
    CheckBox2.Checked:=FMark.visible;

    DrawFromMarkIcons(Image1.canvas,GState.MarkIcons.IndexOf(FMark.PicName),bounds(4,4,36,36));
    VId := FMark.CategoryId;
    for i := 0 to CBKateg.Items.Count - 1 do begin
      VCategory := TCategoryId(CBKateg.Items.Objects[i]);
      if VCategory <> nil then begin
        if VCategory.id = VId then begin
          CBKateg.ItemIndex := i;
          Break;
        end;
      end;
    end;
  end;
  DMS:=D2DMS(FMark.Points[0].y);
  lat1.Value:=DMS.D;
  lat2.Value:=DMS.M;
  lat3.Value:=DMS.S;
  if DMS.N then begin
    Lat_ns.ItemIndex:=1
  end else begin
    Lat_ns.ItemIndex:=0;
  end;
  DMS:=D2DMS(FMark.Points[0].x);
  lon1.Value:=DMS.D;
  lon2.Value:=DMS.M;
  lon3.Value:=DMS.S;
  if DMS.N then begin
    Lon_we.ItemIndex:=1
  end else begin
    Lon_we.ItemIndex:=0;
  end;
  result := ShowModal=mrOk;
end;
procedure TFaddPoint.BaddClick(Sender: TObject);
var
    All:TExtendedPoint;
    VCategory: TCategoryId;
    VIndex: Integer;
    VId: Integer;
begin
 ALL:=ExtPoint(DMS2G(lon1.Value,lon2.Value,lon3.Value,Lon_we.ItemIndex=1),
               DMS2G(lat1.Value,lat2.Value,lat3.Value,Lat_ns.ItemIndex=1));

  FMark.name:=EditName.Text;
  FMark.Desc:=frMarkDescription.Description;
  SetLength(FMark.Points, 1);
  FMark.Points[0] := All;
  FMark.Scale1:=SpinEdit1.Value;
  FMark.Scale2:=SpinEdit2.Value;
  FMark.Color1:=SetAlpha(Color32(ColorBox1.Selected),round(((100-SEtransp.Value)/100)*256));
  FMark.Color2:=SetAlpha(Color32(ColorBox2.Selected),round(((100-SEtransp.Value)/100)*256));
  FMark.visible:=CheckBox2.Checked;
  FMark.LLRect.TopLeft := All;
  FMark.LLRect.BottomRight := All;
  VIndex := CBKateg.ItemIndex;
  if VIndex < 0 then begin
    VIndex:= CBKateg.Items.IndexOf(CBKateg.Text);
  end;
  if VIndex < 0 then begin
    VId := AddKategory(CBKateg.Text);
  end else begin
    VCategory := TCategoryId(CBKateg.Items.Objects[VIndex]);
    if VCategory <> nil then begin
      VId := VCategory.id;
    end else begin
      VId := AddKategory(CBKateg.Text);
    end;
  end;
  FMark.CategoryId:= VId;
  ModalResult := mrOk;
end;

procedure TFaddPoint.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FMark := nil;
end;

//procedure TFaddPoint.EditCommentKeyPress(Sender: TObject; var Key: Char);
//begin
// if key='$' then
//  begin
//   if (sender is TEdit) then key:=' ';
//   if (sender is TMemo) then key:=' ';
//  end;
//end;
//
//procedure TFaddPoint.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
//begin
// if Key=VK_ESCAPE then close;
// if Key=VK_RETURN then BaddClick(Sender);
//end;

procedure TFaddPoint.FormShow(Sender: TObject);
begin
  EditName.SetFocus;
end;

procedure TFaddPoint.SpeedButton1Click(Sender: TObject);
begin
 if ColorDialog1.Execute then ColorBox1.Selected:=ColorDialog1.Color;
end;

procedure TFaddPoint.SpeedButton2Click(Sender: TObject);
begin
 if ColorDialog1.Execute then ColorBox2.Selected:=ColorDialog1.Color;
end;

constructor TFaddPoint.Create(AOwner: TComponent);
begin
  inherited;
  frMarkDescription := TfrMarkDescription.Create(nil);
  frMarkDescription.Parent := pnlDescription
end;

destructor TFaddPoint.Destroy;
var
  i: Integer;
begin
  for i := 0 to CBKateg.Items.Count - 1 do begin
    CBKateg.Items.Objects[i].Free;
  end;
  CBKateg.Items.Clear;
  FreeAndNil(frMarkDescription);
  inherited;
end;

procedure TFaddPoint.DrawFromMarkIcons(canvas:TCanvas;index:integer;bound:TRect);
var
  Bitmap: TCustomBitmap32;
  Bitmap2: TBitmap32;
  wdth:integer;
begin
  if index<0 then index:=0;
  canvas.FillRect(bound);
  wdth:=min(bound.Right-bound.Left,bound.Bottom-bound.Top);
  Bitmap:=TCustomBitmap32.Create;
  Bitmap2:=TBitmap32.Create;
  try
   Bitmap.Assign(TCustomBitmap32(GState.MarkIcons.Objects[index]));
   Bitmap.DrawMode:=dmBlend;
   Bitmap.Resampler:=TKernelResampler.Create;
   TKernelResampler(Bitmap.Resampler).Kernel:=TLinearKernel.Create;
   Bitmap2.SetSize(wdth,wdth);
   Bitmap2.Clear(clWhite32);
   Bitmap2.Draw(Bounds(0, 0, wdth,wdth), Bounds(0, 0, Bitmap.Width,Bitmap.Height),Bitmap);
   Bitmap2.DrawTo(canvas.Handle, bound, Bounds(0, 0, Bitmap2.Width,Bitmap2.Height));
  finally
   Bitmap.Free;
   Bitmap2.Free;
  end;
end;

procedure TFaddPoint.DrawGrid1DrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var i:Integer;
begin
   i:=(Arow*DrawGrid1.ColCount)+ACol;
   if i<GState.MarkIcons.Count then
    DrawFromMarkIcons(DrawGrid1.Canvas,i,DrawGrid1.CellRect(ACol,ARow));
end;

procedure TFaddPoint.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 DrawGrid1.Visible:=not(DrawGrid1.Visible);
end;

procedure TFaddPoint.RefreshTranslation;
begin
  inherited;

end;

procedure TFaddPoint.DrawGrid1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i:integer;
    ACol,ARow: Integer;
begin
 DrawGrid1.MouseToCell(X,Y,ACol,ARow);
 i:=(ARow*DrawGrid1.ColCount)+ACol;
 if (ARow>-1)and(ACol>-1)and(i<GState.MarkIcons.Count) then begin
   FMark.PicName:=GState.MarkIcons.Strings[i];
   image1.Canvas.FillRect(image1.Canvas.ClipRect);
   DrawFromMarkIcons(image1.Canvas,i,bounds(5,5,36,36));
   DrawGrid1.Visible:=false;
 end;
end;

end.
