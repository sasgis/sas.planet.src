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
  u_KmlInfoSimple,
  i_MarksSimple,
  u_MarksDbGUIHelper,
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
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
  private
    { Private declarations }
    FileName:string;
    FMarkDBGUI: TMarksDbGUIHelper;
    FCategoryList: TList;
    function KMLDataToMark(ASource: TKMLData; ATemplate: IMarkFull): IMarkFull;
    function PLTDataToMark(ASource: TPLTData; ATemplate: IMarkFull): IMarkFull;
  public
    function ImportFile(AFileName: string; AMarkDBGUI: TMarksDbGUIHelper): Boolean;
  end;

var
  FImport: TFImport;

implementation

uses
  u_GlobalState,
  i_IMarkPicture,
  u_MarksSimple,
  u_MarksReadWriteSimple,
  Ugeofun,
  t_GeoTypes;

{$R *.dfm}

function TFImport.KMLDataToMark(ASource: TKMLData; ATemplate: IMarkFull): IMarkFull;
begin
  if ASource.IsPoint then begin
    Result := FMarkDBGUI.MarksDB.MarksDb.Factory.CreateNewPoint(
      ASource.coordinates[0],
      ASource.Name,
      ASource.description,
      ATemplate
    );
  end else if ASource.IsLine then begin
    Result := FMarkDBGUI.MarksDB.MarksDb.Factory.CreateNewLine(
      ASource.coordinates,
      ASource.Name,
      ASource.description,
      ATemplate
    );
  end else if ASource.IsPoly then begin
    Result := FMarkDBGUI.MarksDB.MarksDb.Factory.CreateNewPoly(
      ASource.coordinates,
      ASource.Name,
      ASource.description,
      ATemplate
    );
  end else begin
    Result := nil;
  end;
end;

function TFImport.PLTDataToMark(ASource: TPLTData; ATemplate: IMarkFull): IMarkFull;
begin
  if ASource.IsPoint then begin
    Result := FMarkDBGUI.MarksDB.MarksDb.Factory.CreateNewPoint(
      ASource.coordinates[0],
      ASource.Name,
      ASource.description,
      ATemplate
    );
  end else if ASource.IsLine then begin
    Result := FMarkDBGUI.MarksDB.MarksDb.Factory.CreateNewLine(
      ASource.coordinates,
      ASource.Name,
      ASource.description,
      ATemplate
    );
  end else if ASource.IsPoly then begin
    Result := FMarkDBGUI.MarksDB.MarksDb.Factory.CreateNewPoly(
      ASource.coordinates,
      ASource.Name,
      ASource.description,
      ATemplate
    );
  end else begin
    Result := nil;
  end;
end;

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
  VPic: IMarkPicture;
begin
  ComboBox1.Canvas.FillRect(Rect);

  Bitmap:=TCustomBitmap32.Create;
  try
    VPic := IMarkPicture(Pointer(ComboBox1.Items.Objects[Index]));
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
var
  i: Integer;
  VPictureList: IMarkPictureList;
begin
  VPictureList := GState.MarksDB.MarksDb.MarkPictureList;
  ComboBox1.Items.Clear;
  for i := 0 to VPictureList.Count - 1 do begin
    ComboBox1.Items.AddObject(VPictureList.GetName(i), Pointer(VPictureList.Get(i)));
  end;
  ComboBox1.Repaint;
  ComboBox1.ItemIndex:=0;
end;

procedure TFImport.SpeedButton3Click(Sender: TObject);
begin
 if ColorDialog1.Execute then ColorBox3.Selected:=ColorDialog1.Color;
end;

procedure TFImport.SpeedButton4Click(Sender: TObject);
begin
 if ColorDialog1.Execute then ColorBox4.Selected:=ColorDialog1.Color;
end;

procedure TFImport.SpeedButton5Click(Sender: TObject);
begin
 if ColorDialog1.Execute then ColorBox5.Selected:=ColorDialog1.Color;
end;

function TFImport.ImportFile(AFileName: string; AMarkDBGUI: TMarksDbGUIHelper): Boolean;
var
  VCategoryList: TList;
begin
  FileName := AFileName;
  FMarkDBGUI := AMarkDBGUI;
  VCategoryList := FMarkDBGUI.MarksDB.CategoryDB.GetCategoriesList;
  try
    FMarkDBGUI.CategoryListToStrings(VCategoryList, CBKateg.Items);
    Result := ShowModal = mrOk;
  finally
    FreeAndNil(VCategoryList);
  end;
end;

procedure TFImport.Button1Click(Sender: TObject);
var
  KML:TKmlInfoSimple;
  PLT:TPLT;
  i,j,lenarr:integer;
  ms:TMemoryStream;
  alltl,allbr:TDoublePoint;
  markignor,pathignor,polyignor:boolean;
  VCategory: TCategoryId;
  VIndex: Integer;
  VId: Integer;
  VMarkTemplatePoint: IMarkFull;
  VMarkTemplateLine: IMarkFull;
  VMarkTemplatePoly: IMarkFull;
  VMark: IMarkFull;
  VPic: IMarkPicture;
  VPicName: string;
begin
  VCategory := nil;
  VIndex := CBKateg.ItemIndex;
  if VIndex < 0 then begin
    VIndex:= CBKateg.Items.IndexOf(CBKateg.Text);
  end;
  if VIndex >= 0 then begin
    VCategory := TCategoryId(CBKateg.Items.Objects[VIndex]);
  end;
  if VCategory = nil then begin
    VCategory := FMarkDBGUI.AddKategory(CBKateg.Text);
    if VCategory <> nil then begin
      FCategoryList.Add(VCategory);
      FMarkDBGUI.CategoryListToStrings(FCategoryList, CBKateg.Items);
    end;
  end;
  VMarkTemplatePoint := nil;
  markignor:=CBMarkIgnor.Checked;
  if not markignor then begin
    VIndex := ComboBox1.ItemIndex;
    if VIndex < 0 then begin
      VPic := nil;
      VPicName := '';
    end else begin
      VPic := IMarkPicture(Pointer(ComboBox1.Items.Objects[VIndex]));
      VPicName := ColorBox1.Items.Strings[VIndex];
    end;
    VMarkTemplatePoint :=
      FMarkDBGUI.MarksDB.MarksDb.Factory.CreatePointTemplate(
        True,
        VPicName,
        VPic,
        VId,
        SetAlpha(Color32(ColorBox1.Selected),round(((100-SEtransp.Value)/100)*256)),
        SetAlpha(Color32(ColorBox2.Selected),round(((100-SEtransp.Value)/100)*256)),
        SpinEdit1.Value,
        SpinEdit2.Value
      );
  end;
  VMarkTemplateLine := nil;
  pathignor:=CBPathIgnor.Checked;
  if not pathignor then begin
    VMarkTemplateLine :=
      FMarkDBGUI.MarksDB.MarksDb.Factory.CreateLineTemplate(
        True,
        VId,
        SetAlpha(Color32(ColorBox3.Selected),round(((100-SpinEdit4.Value)/100)*256)),
        SpinEdit3.Value
      );
  end;
  VMarkTemplatePoly := nil;
  polyignor:=CBPolyIgnor.Checked;
  if not polyignor then begin
    VMarkTemplatePoly :=
      FMarkDBGUI.MarksDB.MarksDb.Factory.CreatePolyTemplate(
        True,
        VId,
        SetAlpha(Color32(ColorBox4.Selected),round(((100-SpinEdit6.Value)/100)*256)),
        SetAlpha(Color32(ColorBox5.Selected),round(((100-SEtransp2.Value)/100)*256)),
        SpinEdit5.Value
      );
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
            VMark := VMarkTemplatePoint;
          end;
        end else if KML.Data[i].IsPoly then begin
          if VMarkTemplatePoly <> nil then begin
            VMark := VMarkTemplatePoly;
          end;
        end else if KML.Data[i].IsLine then begin
          if VMarkTemplateLine <> nil then begin
            VMark := VMarkTemplateLine;
          end;
        end;
        if VMark <> nil then begin
          VMark := KMLDataToMark(KML.Data[i], VMark);
          GState.MarksDb.MarksDb.WriteMark(VMark);
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
            VMark := VMarkTemplatePoint;
          end;
        end else if PLT.Data[i].IsPoly then begin
          if VMarkTemplatePoly <> nil then begin
            VMark := VMarkTemplatePoly;
          end;
        end else if PLT.Data[i].IsLine then begin
          if VMarkTemplateLine <> nil then begin
            VMark := VMarkTemplateLine;
          end;
        end;
        if VMark <> nil then begin
          VMark := PLTDataToMark(PLT.Data[i], VMark);
          GState.MarksDb.MarksDb.WriteMark(VMark);
        end;
      end;
     finally
       plt.Free;
     end;
    end;
  ModalResult := mrOk;
end;

end.
