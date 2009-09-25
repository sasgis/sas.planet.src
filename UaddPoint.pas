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
  pngimage,
  ugeofun,
  GR32,
  GR32_Resamplers,
  UResStrings,
  UMarksExplorer,
  t_GeoTypes;

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
    procedure BaddClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure EditCommentKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ComboBox1DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    { Private declarations }
  public
   function show_(aLL:TExtendedPoint;new:boolean):boolean;
  end;

var
  FaddPoint: TFaddPoint;
  new_:boolean;
  iconName:string;

implementation

uses
  Math,
  Unit1,
  Unit2;

{$R *.dfm}
function TFaddPoint.show_(aLL:TExtendedPoint;new:boolean):boolean;
var DMS:TDMS;
    ms:TMemoryStream;
    arrLL:PArrLL;
    namecatbuf:string;
begin
 new_:=new;
 EditComment.Text:='';
 EditName.Text:=SAS_STR_NewMark;
 namecatbuf:=CBKateg.Text;
 CBKateg.Clear;
 Kategory2Strings(CBKateg.Items);
 CBKateg.Text:=namecatbuf;
 ComboBox1.Items.Assign(marksicons);
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
              ComboBox1.ItemIndex:=marksicons.IndexOf(Fmain.CDSmarkspicname.AsString);
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
 Fmain.CDSmarks.ApplyRange;
 Fmain.CDSmarks.MergeChangeLog;
 Fmain.CDSmarks.SaveToFile(extractfilepath(paramstr(0))+'marks.sml',dfXMLUTF8);
 close;
 ModalResult:=mrOk;
end;

procedure TFaddPoint.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 if aoper=add_point then Fmain.setalloperationfalse(movemap);
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

end.
