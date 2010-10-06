unit UImport;

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
  UMarksExplorer,
  UPLT;

type
  TFImport = class(TCommonFormParent)
    Label8: TLabel;
    CBKateg: TComboBox;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    ColorBox1: TColorBox;
    SpinEdit1: TSpinEdit;
    ColorBox2: TColorBox;
    SpinEdit2: TSpinEdit;
    SEtransp: TSpinEdit;
    ComboBox1: TComboBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    ColorDialog1: TColorDialog;
    Label1: TLabel;
    Label2: TLabel;
    Label9: TLabel;
    SpeedButton3: TSpeedButton;
    ColorBox3: TColorBox;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    SpeedButton4: TSpeedButton;
    Label13: TLabel;
    Label14: TLabel;
    SpeedButton5: TSpeedButton;
    Label15: TLabel;
    Label16: TLabel;
    ColorBox4: TColorBox;
    SpinEdit5: TSpinEdit;
    SpinEdit6: TSpinEdit;
    ColorBox5: TColorBox;
    SEtransp2: TSpinEdit;
    Button1: TButton;
    Button2: TButton;
    Label17: TLabel;
    CBMarkIgnor: TCheckBox;
    CBPathIgnor: TCheckBox;
    CBPolyIgnor: TCheckBox;
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure ComboBox1DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormActivate(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    FileName:string;
  public
    function ImportFile(AFileName: string): Boolean;
  end;

var
  FImport: TFImport;

implementation

uses
  u_GlobalState,
  u_KmlInfoSimple,
  u_MarksSimple,
  u_MarksReadWriteSimple,
  t_GeoTypes;

procedure KMLDataToMark(ASource: TKMLData; ATarget: TMarkFull);
var
  alltl,allbr:TExtendedPoint;
  j: Integer;
begin
  ATarget.name := ASource.Name;
  ATarget.Desc := ASource.description;
  ATarget.Points := Copy(ASource.coordinates);

  alltl:=ATarget.Points[0];
  allbr:=ATarget.Points[0];
  for j:=1 to Length(ATarget.Points)-1 do begin
    if alltl.x>ATarget.Points[j].x then alltl.x:=ATarget.Points[j].x;
    if alltl.y<ATarget.Points[j].y then alltl.y:=ATarget.Points[j].y;
    if allbr.x<ATarget.Points[j].x then allbr.x:=ATarget.Points[j].x;
    if allbr.y>ATarget.Points[j].y then allbr.y:=ATarget.Points[j].y;
  end;
  ATarget.LLRect.TopLeft := alltl;
  ATarget.LLRect.BottomRight := allbr;
end;

procedure PLTDataToMark(ASource: TPLTData; ATarget: TMarkFull);
var
  alltl,allbr:TExtendedPoint;
  j: Integer;
begin
  ATarget.name := ASource.Name;
  ATarget.Desc := ASource.description;
  ATarget.Points := Copy(ASource.coordinates);

  alltl:=ATarget.Points[0];
  allbr:=ATarget.Points[0];
  for j:=1 to Length(ATarget.Points)-1 do begin
    if alltl.x>ATarget.Points[j].x then alltl.x:=ATarget.Points[j].x;
    if alltl.y<ATarget.Points[j].y then alltl.y:=ATarget.Points[j].y;
    if allbr.x<ATarget.Points[j].x then allbr.x:=ATarget.Points[j].x;
    if allbr.y>ATarget.Points[j].y then allbr.y:=ATarget.Points[j].y;
  end;
  ATarget.LLRect.TopLeft := alltl;
  ATarget.LLRect.BottomRight := allbr;
end;

{$R *.dfm}

procedure TFImport.SpeedButton1Click(Sender: TObject);
begin
 if ColorDialog1.Execute then ColorBox1.Selected:=ColorDialog1.Color;
end;

procedure TFImport.SpeedButton2Click(Sender: TObject);
begin
 if ColorDialog1.Execute then ColorBox2.Selected:=ColorDialog1.Color;
end;

procedure TFImport.Button2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFImport.ComboBox1DrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  Bitmap: TCustomBitmap32;
  Bitmap2: TBitmap32;
begin
  ComboBox1.Canvas.FillRect(Rect);

  Bitmap:=TCustomBitmap32.Create;
  try
    Bitmap.Assign(TCustomBitmap32(ComboBox1.Items.Objects[Index]));
    Bitmap.DrawMode:=dmBlend;
    Bitmap.Resampler:=TKernelResampler.Create;
    TKernelResampler(Bitmap.Resampler).Kernel:=TCubicKernel.Create;

    Bitmap2:=TBitmap32.Create;
    try
      Bitmap2.SetSize(31,31);
      Bitmap2.Clear(clWhite32);
      Bitmap2.Draw(Bounds(0, 0, 31,31), Bounds(0, 0, Bitmap.Width,Bitmap.Height),Bitmap);
      Bitmap2.DrawTo(
        ComboBox1.Canvas.Handle,
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

procedure TFImport.FormActivate(Sender: TObject);
begin
 ComboBox1.Items.Assign(GState.MarkIcons);
 ComboBox1.Repaint;
 ComboBox1.ItemIndex:=0;
end;

procedure TFImport.SpeedButton3Click(Sender: TObject);
begin
 if ColorDialog1.Execute then ColorBox1.Selected:=ColorDialog1.Color;
end;

procedure TFImport.FormShow(Sender: TObject);
begin
 Kategory2StringsWithObjects(CBKateg.Items);
end;

function TFImport.ImportFile(AFileName: string): Boolean;
begin
  FileName := AFileName;
  Result := ShowModal = mrOk;
end;

procedure TFImport.Button1Click(Sender: TObject);
var
  KML:TKmlInfoSimple;
  PLT:TPLT;
  i,j,lenarr:integer;
  ms:TMemoryStream;
  alltl,allbr:TExtendedPoint;
  markignor,pathignor,polyignor:boolean;
  VCategory: TCategoryId;
  VIndex: Integer;
  VId: Integer;
  VMarkTemplatePoint: TMarkFull;
  VMarkTemplateLine: TMarkFull;
  VMarkTemplatePoly: TMarkFull;
  VMark: TMarkFull;
begin
 VMarkTemplatePoint := nil;
 markignor:=CBMarkIgnor.Checked;
 if not markignor then begin
  VMarkTemplatePoint := TMarkFull.Create;
 end;
  VMarkTemplateLine := nil;
 pathignor:=CBPathIgnor.Checked;
 if not pathignor then begin
  VMarkTemplateLine := TMarkFull.Create;
 end;
  VMarkTemplatePoly := nil;
 polyignor:=CBPolyIgnor.Checked;
 if not polyignor then begin
  VMarkTemplatePoly := TMarkFull.Create;
 end;

 try
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
    if VMarkTemplatePoint <> nil then begin
      VMarkTemplatePoint.id := -1;
      VMarkTemplatePoint.visible := True;
      VMarkTemplatePoint.Scale1:=SpinEdit1.Value;
      VMarkTemplatePoint.Scale2:=SpinEdit2.Value;
      VMarkTemplatePoint.Color1:=SetAlpha(Color32(ColorBox1.Selected),round(((100-SEtransp.Value)/100)*256));
      VMarkTemplatePoint.Color2:=SetAlpha(Color32(ColorBox2.Selected),round(((100-SEtransp.Value)/100)*256));
      VMarkTemplatePoint.PicName:=ComboBox1.Text;
      VMarkTemplatePoint.CategoryId:=VId;
    end;
    if VMarkTemplateLine <> nil then begin
      VMarkTemplateLine.id := -1;
      VMarkTemplateLine.visible:=true;
      VMarkTemplateLine.Scale1:=SpinEdit3.Value;
      VMarkTemplateLine.Color1:=SetAlpha(Color32(ColorBox3.Selected),round(((100-SpinEdit4.Value)/100)*256));
      VMarkTemplateLine.CategoryId:=VId;
    end;
    if VMarkTemplatePoly <> nil then begin
      VMarkTemplatePoly.id := -1;
      VMarkTemplatePoly.visible:=true;
      VMarkTemplatePoly.Scale1:=SpinEdit5.Value;
      VMarkTemplatePoly.Color1:=SetAlpha(Color32(ColorBox4.Selected),round(((100-SpinEdit6.Value)/100)*256));
      VMarkTemplatePoly.Color2:=SetAlpha(Color32(ColorBox5.Selected),round(((100-SEtransp2.Value)/100)*256));
      VMarkTemplatePoly.CategoryId:=VId;
    end;
   if (LowerCase(ExtractFileExt(FileName))='.kml') or (LowerCase(ExtractFileExt(FileName))='.kmz') then
    begin
     KML:=TKmlInfoSimple.Create;
     try
     if (LowerCase(ExtractFileExt(FileName))='.kml') then begin
       GState.KmlLoader.LoadFromFile(FileName, KML);
     end else if (LowerCase(ExtractFileExt(FileName))='.kmz') then begin
       GState.KmzLoader.LoadFromFile(FileName, KML);
     end else begin
       Abort;
     end;
     for i:=0 to length(KML.Data)-1 do begin
        VMark := nil;
        if KML.Data[i].IsPoint then begin
          if VMarkTemplatePoint <> nil then begin
            VMark := TMarkFull.Create;
            VMark.Assign(VMarkTemplatePoint);
          end;
        end else if KML.Data[i].IsPoly then begin
          if VMarkTemplatePoly <> nil then begin
            VMark := TMarkFull.Create;
            VMark.Assign(VMarkTemplatePoly);
          end;
        end else if KML.Data[i].IsPoly then begin
          if VMarkTemplateLine <> nil then begin
            VMark := TMarkFull.Create;
            VMark.Assign(VMarkTemplateLine);
          end;
        end;
        if VMark <> nil then begin
          try
            KMLDataToMark(KML.Data[i], VMark);
            WriteMark(VMark);
          finally
            VMark.Free;
          end;
        end;
      end;
     finally
       KML.Free;
     end;
    end;
   if LowerCase(ExtractFileExt(FileName))='.plt' then
    begin
     PLT:=TPLT.Create;
     try
     PLT.loadFromFile(FileName);
     for i:=0 to length(PLT.Data)-1 do  begin
        VMark := nil;
        if PLT.Data[i].IsPoint then begin
          if VMarkTemplatePoint <> nil then begin
            VMark := TMarkFull.Create;
            VMark.Assign(VMarkTemplatePoint);
          end;
        end else if PLT.Data[i].IsPoly then begin
          if VMarkTemplatePoly <> nil then begin
            VMark := TMarkFull.Create;
            VMark.Assign(VMarkTemplatePoly);
          end;
        end else if PLT.Data[i].IsPoly then begin
          if VMarkTemplateLine <> nil then begin
            VMark := TMarkFull.Create;
            VMark.Assign(VMarkTemplateLine);
          end;
        end;
        if VMark <> nil then begin
          try
            PLTDataToMark(PLT.Data[i], VMark);
            WriteMark(VMark);
          finally
            VMark.Free;
          end;
        end;
      end;
     finally
       plt.Free;
     end;
    end;
    SaveMarks2File;
  finally
    VMarkTemplatePoint.Free;
    VMarkTemplateLine.Free;
    VMarkTemplatePoly.Free;
  end;
  ModalResult := mrOk;
end;

end.
