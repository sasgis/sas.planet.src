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
  ColorGrd,
  Buttons,
  Spin,
  DB,
  DBClient,
  DBCtrls,
  rxToolEdit,
  rxCurrEdit,
  TB2Item,
  TBX,
  TB2Dock,
  TB2Toolbar,
  pngimage,
  ugeofun,
  GR32,
  GR32_Resamplers,
  UResStrings,
  UMarksExplorer,
  t_GeoTypes, ComCtrls;

type
  TFaddPoint = class(TForm)
    EditName: TEdit;
    EditComment: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Badd: TButton;
    Button2: TButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
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
    ComboBox1: TComboBox;
    Label6: TLabel;
    SpinEdit2: TSpinEdit;
    SEtransp: TSpinEdit;
    Label7: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    ColorDialog1: TColorDialog;
    Label8: TLabel;
    CBKateg: TComboBox;
    TBXToolbar1: TTBXToolbar;
    TBXItem1: TTBXItem;
    TBXItem2: TTBXItem;
    TBXItem3: TTBXItem;
    TBXSeparatorItem1: TTBXSeparatorItem;
    TBXItem4: TTBXItem;
    TBXItem5: TTBXItem;
    TBXItem6: TTBXItem;
    TBXSeparatorItem2: TTBXSeparatorItem;
    TBXItem7: TTBXItem;
    ListView1: TListView;
    procedure BaddClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure EditCommentKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ComboBox1DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure TBXItem3Click(Sender: TObject);
    procedure EditCommentKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListView1AdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
  private
    new_:boolean;
  public
   function show_(aLL:TExtendedPoint;new:boolean):boolean;
  end;

  TEditBtn = (ebB,ebI,ebU,ebLeft,ebCenter,ebRight,ebImg);

var
  FaddPoint: TFaddPoint;

implementation

uses
  Math,
  u_GlobalState,
  Unit1,
  Unit2;

{$R *.dfm}
function TFaddPoint.show_(aLL:TExtendedPoint;new:boolean):boolean;
var DMS:TDMS;
    ms:TMemoryStream;
    arrLL:PArrLL;
    namecatbuf:string;
    i:integer;
begin
 new_:=new;
 EditComment.Text:='';
 EditName.Text:=SAS_STR_NewMark;
 namecatbuf:=CBKateg.Text;
 CBKateg.Clear;
 Kategory2Strings(CBKateg.Items);
 CBKateg.Text:=namecatbuf;
 ComboBox1.Items.Assign(GState.MarkIcons);
 ListView1.Clear;
 for i:=0 to GState.MarkIcons.Count-1 do begin
  ListView1.AddItem({GState.MarkIcons.Strings[i]}'',GState.MarkIcons.Objects[i]);
 end;
 if new then begin
              If ComboBox1.ItemIndex<0 then ComboBox1.ItemIndex:=0;
              faddPoint.Caption:=SAS_STR_AddNewMark;
              Badd.Caption:=SAS_STR_Add;
              CheckBox2.Checked:=true;
             end
        else begin
              ms:=TMemoryStream.Create;
              TBlobField(Fmain.CDSmarks.FieldByName('LonLatArr')).SaveToStream(ms);
              ms.Position:=0;
              GetMem(arrLL,ms.size);
              ms.ReadBuffer(arrLL^,ms.size);
              ms.free;
              aLL:=arrLL^[0];
              faddPoint.Caption:=SAS_STR_EditMark;
              Badd.Caption:=SAS_STR_Edit;
              EditName.Text:=Fmain.CDSmarks.FieldByName('name').AsString;
              EditComment.Text:=Fmain.CDSmarks.FieldByName('descr').AsString;
              SpinEdit1.Value:=Fmain.CDSmarks.FieldByName('Scale1').AsInteger;
              SpinEdit2.Value:=Fmain.CDSmarks.FieldByName('Scale2').AsInteger;
              SEtransp.Value:=100-round(AlphaComponent(TColor32(Fmain.CDSmarks.FieldByName('Color1').AsInteger))/255*100);
              ColorBox1.Selected:=WinColor(TColor32(Fmain.CDSmarks.FieldByName('Color1').AsInteger));
              ColorBox2.Selected:=WinColor(TColor32(Fmain.CDSmarks.FieldByName('Color2').AsInteger));
              CheckBox2.Checked:=Fmain.CDSmarks.FieldByName('Visible').AsBoolean;
              ComboBox1.ItemIndex:=GState.MarkIcons.IndexOf(Fmain.CDSmarkspicname.AsString);
              Fmain.CDSKategory.Locate('id',Fmain.CDSmarkscategoryid.AsInteger,[]);
              CBKateg.Text:=Fmain.CDSKategory.fieldbyname('name').AsString;
             end;
 DMS:=D2DMS(aLL.y);
 lat1.Value:=DMS.D; lat2.Value:=DMS.M; lat3.Value:=DMS.S;
 if DMS.N then Lat_ns.ItemIndex:=1 else Lat_ns.ItemIndex:=0;
 DMS:=D2DMS(aLL.x);
 lon1.Value:=DMS.D; lon2.Value:=DMS.M; lon3.Value:=DMS.S;
 if DMS.N then Lon_we.ItemIndex:=1 else Lon_we.ItemIndex:=0;
 ShowModal;
 result:=ModalResult=mrOk;
end;

procedure TFaddPoint.BaddClick(Sender: TObject);
var
    ms:TMemoryStream;
    All:TExtendedPoint;
begin
 ALL:=ExtPoint(DMS2G(lon1.Value,lon2.Value,lon3.Value,Lon_we.ItemIndex=1),
               DMS2G(lat1.Value,lat2.Value,lat3.Value,Lat_ns.ItemIndex=1));

 if new_ then Fmain.CDSmarks.Insert
         else Fmain.CDSmarks.Edit;
 Fmain.CDSmarks.FieldByName('name').AsString:=EditName.Text;
 Fmain.CDSmarks.FieldByName('descr').AsString:=EditComment.Text;
 ms:=TMemoryStream.Create;
 ms.WriteBuffer(All,SIZEOF(TExtendedPoint));
 TBlobField(Fmain.CDSmarks.FieldByName('LonLatArr')).LoadFromStream(ms);
 ms.free;
 Fmain.CDSmarks.FieldByName('Scale1').AsInteger:=SpinEdit1.Value;
 Fmain.CDSmarks.FieldByName('Scale2').AsInteger:=SpinEdit2.Value;
 Fmain.CDSmarks.FieldByName('Color1').AsFloat:=SetAlpha(Color32(ColorBox1.Selected),round(((100-SEtransp.Value)/100)*256));
 Fmain.CDSmarks.FieldByName('Color2').AsFloat:=SetAlpha(Color32(ColorBox2.Selected),round(((100-SEtransp.Value)/100)*256));
 Fmain.CDSmarks.FieldByName('Visible').AsBoolean:=CheckBox2.Checked;
 Fmain.CDSmarks.FieldByName('PicName').AsString:=ComboBox1.Text;
 Fmain.CDSmarks.FieldByName('LonL').AsFloat:=ALL.x;
 Fmain.CDSmarks.FieldByName('LatT').AsFloat:=ALL.y;
 Fmain.CDSmarks.FieldByName('LonR').AsFloat:=ALL.x;
 Fmain.CDSmarks.FieldByName('LatB').AsFloat:=ALL.y;
 if not(Fmain.CDSKategory.Locate('name',CBKateg.Text,[]))
  then AddKategory(CBKateg.Text);
 Fmain.CDSmarks.FieldByName('categoryid').AsFloat:=Fmain.CDSKategory.FieldByName('id').AsInteger;
 Fmain.CDSmarks.Post;
 SaveMarks2File;
 close;
 ModalResult:=mrOk;
end;

procedure TFaddPoint.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 if Fmain.aoper=ao_add_point then Fmain.setalloperationfalse(ao_movemap);
end;

procedure TFaddPoint.Button2Click(Sender: TObject);
begin
 close;
end;

procedure TFaddPoint.EditCommentKeyPress(Sender: TObject; var Key: Char);
begin
 if key='$' then
  begin
   if (sender is TEdit) then key:=' ';
   if (sender is TMemo) then key:=' ';
  end;
end;

procedure TFaddPoint.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 if Key=VK_ESCAPE then close;
 if Key=VK_RETURN then BaddClick(Sender);
end;

procedure TFaddPoint.ComboBox1DrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var Bitmap,Bitmap2: TBitmap32;
begin
   with ComboBox1.Canvas do
   begin
     FillRect(Rect);
     Bitmap:=TBitmap32.Create;
     Bitmap2:=TBitmap32.Create;
     Bitmap.SetSize(TPNGObject(ComboBox1.Items.Objects[Index]).Width,TPNGObject(ComboBox1.Items.Objects[Index]).Height);
     Bitmap2.SetSize(31,31);
     Bitmap.Clear(clWhite32);
     Bitmap.Assign(TPNGObject(ComboBox1.Items.Objects[Index]));
     Bitmap.Resampler:=TKernelResampler.Create;
     TKernelResampler(Bitmap.Resampler).Kernel:=TCubicKernel.Create;
     Bitmap2.Draw(Bounds(0, 0, 31,31), Bounds(0, 0, Bitmap.Width,Bitmap.Height),Bitmap);
     if Bitmap <> nil then
     begin
      CopyRect(Bounds(Rect.Left + 2, Rect.Top + 2, 31,31),
               Bitmap2.Canvas, Bounds(0, 0, Bitmap2.Width,Bitmap2.Height));
      Bitmap.Free;
     end;
     Bitmap2.Free;
   end;
end;

procedure TFaddPoint.SpeedButton1Click(Sender: TObject);
begin
 if ColorDialog1.Execute then ColorBox1.Selected:=ColorDialog1.Color;
end;

procedure TFaddPoint.SpeedButton2Click(Sender: TObject);
begin
 if ColorDialog1.Execute then ColorBox2.Selected:=ColorDialog1.Color;
end;

procedure TFaddPoint.TBXItem3Click(Sender: TObject);
var s:string;
    seli:integer;
begin
 s:=EditComment.Text;
 seli:=EditComment.SelStart;
 case TEditBtn(TTBXItem(sender).Tag) of
  ebB: begin
        Insert('<b>',s,EditComment.SelStart+1);
        Insert('</b>',s,EditComment.SelStart+EditComment.SelLength+3+1);
       end;
  ebI: begin
        Insert('<i>',s,EditComment.SelStart+1);
        Insert('</i>',s,EditComment.SelStart+EditComment.SelLength+3+1);
       end;
  ebU: begin
        Insert('<u>',s,EditComment.SelStart+1);
        Insert('</u>',s,EditComment.SelStart+EditComment.SelLength+3+1);
       end;
  ebImg:
       begin
        if (FMain.OpenPictureDialog.Execute)and(FMain.OpenPictureDialog.FileName<>'') then begin
         Insert('<img src="'+FMain.OpenPictureDialog.FileName+'"/>',s,EditComment.SelStart+1);
        end;
       end;
  ebCenter:
       begin
        Insert('<CENTER>',s,EditComment.SelStart+1);
        Insert('</CENTER>',s,EditComment.SelStart+EditComment.SelLength+8+1);
       end;
  ebLeft:
       begin
        Insert('<div ALIGN=LEFT>',s,EditComment.SelStart+1);
        Insert('</div>',s,EditComment.SelStart+EditComment.SelLength+16+1);
       end;
  ebRight:
       begin
        Insert('<div ALIGN=RIGHT>',s,EditComment.SelStart+1);
        Insert('</div>',s,EditComment.SelStart+EditComment.SelLength+17+1);
       end;
 end;
 EditComment.Text:=s;
 EditComment.SelStart:=seli;
end;

procedure TFaddPoint.EditCommentKeyDown(Sender: TObject; var Key: Word;  Shift: TShiftState);
var s:string;
    seli:integer;
begin
 if Key=13 then begin
   Key:=0;
   s:=EditComment.Text;
   seli:=EditComment.SelStart;
   Insert('<BR>',s,EditComment.SelStart+1);
   EditComment.Text:=s;
   EditComment.SelStart:=seli+4;
 end;
end;

procedure TFaddPoint.ListView1AdvancedCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  Stage: TCustomDrawStage; var DefaultDraw: Boolean);
var Bitmap,Bitmap2: TBitmap32;
    Rect:TRect;
    wdth:Integer;
begin
   if Stage = cdPrePaint then
   with ListView1.Canvas do
   begin
     Rect:=item.DisplayRect(drBounds);
     if rect.Top<0 then Continue;
     wdth:=max(Rect.Right-Rect.Left,Rect.Bottom-Rect.Top);
    { if cdsHot in State then ListView1.Canvas.Rectangle(bounds(Rect.Left,Rect.Top,wdth,wdth))
                        else }FillRect(Rect);
     Bitmap:=TBitmap32.Create;
     Bitmap.SetSize(TPNGObject(Item.Data).Width,TPNGObject(Item.Data).Height);
     Bitmap.Clear(clWhite32);
     Bitmap.Assign(TPNGObject(Item.Data));
     Bitmap.Resampler:=TKernelResampler.Create;
     TKernelResampler(Bitmap.Resampler).Kernel:=TLinearKernel.Create;
     Bitmap2:=TBitmap32.Create;
     Bitmap2.SetSize(wdth,wdth);
     Bitmap2.Draw(Bounds(0, 0, wdth,wdth), Bounds(0, 0, Bitmap.Width,Bitmap.Height),Bitmap);
     if Bitmap <> nil then
     begin
      CopyRect(Bounds(Rect.Left, Rect.Top,wdth,wdth),
               Bitmap2.Canvas, Bounds(0, 0, Bitmap2.Width,Bitmap2.Height));
     Bitmap.Free;
     end;
     Bitmap2.Free;
   end;
end;

end.
