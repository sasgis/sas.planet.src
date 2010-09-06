unit USaveas;

interface

uses
  Windows,
  SysUtils,
  Graphics,
  Forms,
  Buttons,
  Spin,
  CheckLst,
  Classes,
  Controls,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  inifiles,
  ComCtrls,
  filectrl,
  GR32,
  UGeoFun,
  UMapType,
  UResStrings,
  t_GeoTypes,
  u_GeoTostr;

type
  TFsaveas = class(TForm)
    Button1: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    Label22: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    CheckBox2: TCheckBox;
    Label25: TLabel;
    CBscleit: TComboBox;
    Label26: TLabel;
    Label4: TLabel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Label5: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    CheckBox1: TCheckBox;
    CheckList: TCheckListBox;
    Label23: TLabel;
    CBmapDel: TComboBox;
    CBFormat: TComboBox;
    CheckBox7: TCheckBox;
    CBzamena: TCheckBox;
    CBsavefull: TCheckBox;
    Button3: TButton;
    Bevel6: TBevel;
    CBDateDo: TCheckBox;
    DateDo: TDateTimePicker;
    QualitiEdit: TSpinEdit;
    CBMapLoad: TComboBox;
    CBZoomload: TComboBox;
    CBmtForm: TComboBox;
    Label15: TLabel;
    CBalhForm: TComboBox;
    Label18: TLabel;
    SpeedButton1: TSpeedButton;
    GroupBox1: TGroupBox;
    EditNTg: TSpinEdit;
    Label19: TLabel;
    Label20: TLabel;
    EditNTv: TSpinEdit;
    Label27: TLabel;
    CBSclHib: TComboBox;
    Label28: TLabel;
    SaveSelDialog: TSaveDialog;
    CBusedReColor: TCheckBox;
    Panel1: TPanel;
    Label11: TLabel;
    Label13: TLabel;
    Label12: TLabel;
    CheckListBox1: TCheckListBox;
    CBZipped: TCheckBox;
    CBReplace: TCheckBox;
    Button2: TButton;
    CBMove: TCheckBox;
    EditPath: TEdit;
    CheckBox4: TCheckBox;
    CheckBox3: TCheckBox;
    CheckListBox2: TCheckListBox;
    ComboBox: TComboBox;
    CBSecondLoadTNE: TCheckBox;
    CBCloseWithStart: TCheckBox;
    CBGenFromPrev: TCheckBox;
    PrTypesBox: TCheckListBox;
    CBUsedMarks: TCheckBox;
    SEDelBytes: TSpinEdit;
    CBDelBytes: TCheckBox;
    TabSheet6: TTabSheet;
    CBCahceType: TComboBox;
    Label32: TLabel;
    Bevel7: TBevel;
    pnlExport: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure ComboBoxChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CBDateDoClick(Sender: TObject);
    procedure CBZoomloadChange(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure TabSheet1Show(Sender: TObject);
    procedure TabSheet2Show(Sender: TObject);
    procedure TabSheet4Show(Sender: TObject);
    procedure CBZippedClick(Sender: TObject);
    procedure CBFormatChange(Sender: TObject);
  private
    FZoom_rect:byte;
    FPolygonLL: TExtendedPointArray;
    procedure LoadRegion(APolyLL: TExtendedPointArray);
    procedure DelRegion(APolyLL: TExtendedPointArray);
    procedure genbacksatREG(APolyLL: TExtendedPointArray);
    procedure scleitRECT(APolyLL: TExtendedPointArray);
    procedure savefilesREG(APolyLL: TExtendedPointArray);
    procedure ExportREG(APolyLL: TExtendedPointArray);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadSelFromFile(FileName:string);
    procedure Show_(Azoom:byte;Polygon_: TExtendedPointArray);
   end;

var
  Fsaveas: TFsaveas;

implementation

uses
  u_GlobalState,
  i_ILogSimple,
  i_ILogForTaskThread,
  u_LogForTaskThread,
  i_IMapCalibration,
  i_ICoordConverter,
  UTrAllLoadMap,
  u_ThreadMapCombineBMP,
  u_ThreadMapCombineECW,
  u_ThreadMapCombineJPG,
  u_ThreadMapCombineKMZ,
  u_ExportProviderAbstract,
  u_ExportProviderYaMaps,
  u_ExportProviderGEKml,
  u_ExportProviderIPhone,
  u_ExportProviderAUX,
  u_ThreadExportToFileSystem,
  u_ThreadExportToZip,
  u_ThreadExportIPhone,
  u_ThreadExportKML,
  u_ThreadExportYaMaps,
  u_ThreadDeleteTiles,
  u_ThreadGenPrevZoom,
  UProgress,
  UImgFun,
  unit1;

{$R *.dfm}

procedure TFsaveas.LoadSelFromFile(FileName:string);
var ini:TMemIniFile;
    i:integer;
begin
 if FileExists(FileName) then
  begin
   ini:=TMemIniFile.Create(FileName);
   i:=1;
   while str2r(Ini.ReadString('HIGHLIGHTING','PointLon_'+inttostr(i),'2147483647'))<>2147483647 do
    begin
     setlength(GState.LastSelectionPolygon,i);
     GState.LastSelectionPolygon[i-1].x:=str2r(Ini.ReadString('HIGHLIGHTING','PointLon_'+inttostr(i),'2147483647'));
     GState.LastSelectionPolygon[i-1].y:=str2r(Ini.ReadString('HIGHLIGHTING','PointLat_'+inttostr(i),'2147483647'));
     inc(i);
    end;
   if length(GState.LastSelectionPolygon)>0 then
    begin
     GState.poly_zoom_save:=Ini.Readinteger('HIGHLIGHTING','zoom',1);
     fsaveas.Show_(GState.poly_zoom_save - 1,GState.LastSelectionPolygon);
    end;
    FMain.LayerSelection.Redraw
  end
end;

procedure TFsaveas.DelRegion(APolyLL: TExtendedPointArray);
begin
  if (MessageBox(handle,pchar(SAS_MSG_youasure),pchar(SAS_MSG_coution),36)=IDYES) then begin
    TThreadDeleteTiles.Create(
      APolyLL,
      CBZoomload.ItemIndex+1,
      TMapType(CBmapDel.Items.Objects[CBmapDel.ItemIndex]),
      CBDelBytes.Checked,
      SEDelBytes.Value
    );
  end;
end;

destructor TFsaveas.Destroy;
var
  i: Integer;
begin
  for i := 0 to CBFormat.Items.Count - 1 do begin
    CBFormat.Items.Objects[i].Free;
    CBFormat.Items.Objects[i] := nil;
  end;
  inherited;
end;

procedure TFsaveas.ExportREG(APolyLL: TExtendedPointArray);
var
  VExportProvider: TExportProviderAbstract;
begin
  VExportProvider := TExportProviderAbstract(CBFormat.Items.Objects[CBFormat.ItemIndex]);
  if VExportProvider <> nil then begin
    VExportProvider.StartProcess(APolyLL);
  end;
end;


procedure TFsaveas.savefilesREG(APolyLL: TExtendedPointArray);
var i:integer;
    path:string;
    Zoomarr:array [0..23] of boolean;
    typemaparr:array of TMapType;
    ziped:boolean;
    Replace:boolean;
begin
  for i:=0 to 23 do ZoomArr[i]:=CheckListBox2.Checked[i];
  for i:=0 to CheckListBox1.Items.Count-1 do
   if CheckListBox1.Checked[i] then
    begin
     setlength(typemaparr,length(typemaparr)+1);
     typemaparr[length(typemaparr)-1]:=TMapType(CheckListBox1.Items.Objects[i]);
    end;
  ziped:=CBZipped.Checked;
  path:=IncludeTrailingPathDelimiter(EditPath.Text);
  Replace:=CBReplace.Checked;
  if ziped then begin
    TThreadExportToZip.Create(path,APolyLL,ZoomArr,typemaparr,GState.TileNameGenerator.GetGenerator(CBCahceType.ItemIndex + 1))
  end else begin
    TThreadExportToFileSystem.Create(path,APolyLL,ZoomArr,typemaparr,CBMove.Checked,Replace,GState.TileNameGenerator.GetGenerator(CBCahceType.ItemIndex + 1))
  end;
end;

procedure TFsaveas.LoadRegion(APolyLL: TExtendedPointArray);
var
  smb:TMapType;
  polyg:TPointArray;
  VZoom: byte;
  VLog: TLogForTaskThread;
  VSimpleLog: ILogSimple;
  VThreadLog:ILogForTaskThread;
  VThread: ThreadAllLoadMap;
begin
  smb:=TMapType(CBmapLoad.Items.Objects[CBmapLoad.ItemIndex]);
  VZoom := CBZoomload.ItemIndex;
  polyg := smb.GeoConvert.LonLatArray2PixelArray(APolyLL, VZoom);

  VLog := TLogForTaskThread.Create(5000, 0);
  VSimpleLog := VLog;
  VThreadLog := VLog;
  VThread := ThreadAllLoadMap.Create(VSimpleLog, Polyg,CheckBox2.Checked,CheckBox7.Checked,CBDateDo.Checked,CBSecondLoadTNE.Checked,strtoint(CBZoomload.Text),smb,DateDo.DateTime);
  TFProgress.Create(Application, VThread, VThreadLog);
  polyg := nil;
end;

procedure TFsaveas.genbacksatREG(APolyLL: TExtendedPointArray);
var
  i:integer;
  VInZooms: TArrayOfByte;
begin
  for i:=0 to ComboBox.ItemIndex do begin
    if CheckList.Checked[i] then begin
      SetLength(VInZooms, Length(VInZooms)+1);
      VInZooms[Length(VInZooms)-1]:=ComboBox.ItemIndex - i + 1;
    end;
  end;

  TThreadGenPrevZoom.Create(
    ComboBox.ItemIndex+2,
    VInZooms,
    APolyLL,
    TMapType(CBmtForm.Items.Objects[CBmtForm.ItemIndex]),
    CBzamena.Checked,
    CBsavefull.Checked,
    CBGenFromPrev.Checked,
    TTileResamplingType(CBalhForm.ItemIndex)
   );
end;

procedure TFsaveas.scleitRECT(APolyLL: TExtendedPointArray);
var
  Amt,Hmt:TMapType;
  i:integer;
  VPrTypes: IInterfaceList;
  VFileName: string;
  VSplitCount: TPoint;
  VFileExt: string;
begin
  Amt:=TMapType(CBscleit.Items.Objects[CBscleit.ItemIndex]);
  Hmt:=TMapType(CBSclHib.Items.Objects[CBSclHib.ItemIndex]);
  if (FMain.SaveDialog1.Execute)then begin
    VFileName := FMain.SaveDialog1.FileName;
    VPrTypes := TInterfaceList.Create;
    for i:=0 to PrTypesBox.Items.Count-1 do begin
      if PrTypesBox.Checked[i] then begin
        VPrTypes.Add(IInterface(Pointer(PrTypesBox.Items.Objects[i])));
      end;
    end;
    VSplitCount.X := EditNTg.Value;
    VSplitCount.Y := EditNTv.Value;
    VFileExt := UpperCase(ExtractFileExt(VFileName));
    if (VFileExt='.ECW')or(VFileExt='.JP2') then begin
      TThreadMapCombineECW.Create(
        VPrTypes,
        VFileName,
        APolyLL,
        VSplitCount,
        CBZoomload.ItemIndex+1,
        Amt,Hmt,
        CBusedReColor.Checked,
        CBUsedMarks.Checked,
        QualitiEdit.Value
      );
    end else if (VFileExt='.BMP') then begin
      TThreadMapCombineBMP.Create(
        VPrTypes,
        VFileName,
        APolyLL,
        VSplitCount,
        CBZoomload.ItemIndex+1,
        Amt,Hmt,
        CBusedReColor.Checked,
        CBUsedMarks.Checked
      );
    end else if (VFileExt='.KMZ') then begin
      TThreadMapCombineKMZ.Create(
        VPrTypes,
        VFileName,
        APolyLL,
        VSplitCount,
        CBZoomload.ItemIndex+1,
        Amt,Hmt,
        CBusedReColor.Checked,
        CBUsedMarks.Checked,
        QualitiEdit.Value
      );
    end else begin
      TThreadMapCombineJPG.Create(
        VPrTypes,
        VFileName,
        APolyLL,
        VSplitCount,
        CBZoomload.ItemIndex+1,
        Amt,Hmt,
        CBusedReColor.Checked,
        CBUsedMarks.Checked,
        QualitiEdit.Value
      );
    end;
  end;
end;

procedure TFsaveas.Button1Click(Sender: TObject);
begin
 case PageControl1.ActivePage.Tag of
  0: LoadRegion(FPolygonLL);
  1: scleitRECT(FPolygonLL);
  2: genbacksatREG(FPolygonLL);
  3: delRegion(FPolygonLL);
  4: ExportREG(FPolygonLL);
  5: savefilesREG(FPolygonLL);
 end;
 if CBCloseWithStart.Checked then
  begin
   Fmain.Enabled:=true;
   close;
  end;
end;

procedure TFsaveas.ComboBoxChange(Sender: TObject);
var i:integer;
begin
 CheckList.Items.Clear;
 for i:=ComboBox.ItemIndex+1 downto 1 do CheckList.Items.Add(inttostr(i));
 if ComboBox.ItemIndex+1-7>1 then
 for i:=ComboBox.ItemIndex downto ComboBox.ItemIndex - (ComboBox.ItemIndex-7)+1 do
  CheckList.ItemEnabled[i]:=false;
end;

constructor TFsaveas.Create(AOwner: TComponent);
var
  VExportProvider: TExportProviderAbstract;
begin
  inherited;
  VExportProvider := TExportProviderIPhone.Create(pnlExport, True);
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);
  VExportProvider := TExportProviderIPhone.Create(pnlExport, False);
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);
  VExportProvider := TExportProviderGEKml.Create(pnlExport);
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);
  VExportProvider := TExportProviderYaMaps.Create(pnlExport);
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);
  VExportProvider := TExportProviderAUX.Create(pnlExport);
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);
  CBFormat.ItemIndex := 0;
end;

procedure TFsaveas.Show_(Azoom:byte;Polygon_: TExtendedPointArray);
var
  i:integer;
  vramkah,zagran:boolean;
  VMapCalibration: IMapCalibration;
  VConverter: ICoordConverter;
  VPoint: TPoint;
  VZoom: Byte;
  VActiveMap: TMapType;
  VAddedIndex: Integer;
  VMapType: TMapType;
  VExportProvider: TExportProviderAbstract;
begin
  CBSecondLoadTNE.Enabled:=GState.SaveTileNotExists;
  CBZoomload.Items.Clear;
  ComboBox.Items.Clear;
  CheckListBox2.Items.Clear;
  for i:=1 to 24 do begin
    CBZoomload.Items.Add(inttostr(i));
    if i>1 then begin
      ComboBox.Items.Add(inttostr(i));
    end;
    CheckListBox2.Items.Add(inttostr(i));
  end;
  DateDo.Date:=now;
  CBMapLoad.Items.Clear;
  CBscleit.Items.Clear;
  CBmtForm.Items.Clear;
  CBmapDel.Items.Clear;
  CheckListBox1.Items.Clear;
  CBSclHib.Items.Clear;
  CBSclHib.Items.Add(SAS_STR_No);
  VActiveMap := GState.ViewState.GetCurrentMap;
  For i:=0 to length(GState.MapType)-1 do begin
    VMapType := GState.MapType[i];
    if (VMapType.Usedwn) then begin
      VAddedIndex := CBMapLoad.Items.AddObject(VMapType.name, VMapType);
      if VMapType = VActiveMap then begin
        CBMapLoad.ItemIndex := VAddedIndex;
      end;
    end;
    if VMapType.Usestick then begin
      VAddedIndex := CBscleit.Items.AddObject(VMapType.name,VMapType);
      if (VMapType.asLayer) then begin
        CBSclHib.Items.AddObject(VMapType.name,VMapType);
      end;
      if VMapType = VActiveMap then begin
        CBscleit.ItemIndex:=VAddedIndex;
      end;
    end;
    if (VMapType.UseGenPrevious) then begin
      VAddedIndex := CBmtForm.Items.AddObject(VMapType.name,VMapType);
      if VMapType = VActiveMap then begin
        CBmtForm.ItemIndex:=VAddedIndex;
      end;
    end;
    if (VMapType.TileStorage.GetUseDel) then begin
      VAddedIndex := CBmapDel.Items.AddObject(VMapType.name, VMapType);
      if VMapType = VActiveMap then begin
        CBmapDel.ItemIndex:=VAddedIndex;
      end;
    end;
    if (VMapType.TileStorage.GetUseSave) then begin
      VAddedIndex := CheckListBox1.Items.AddObject(VMapType.name,VMapType);
      if VMapType = VActiveMap then begin
        CheckListBox1.Checked[VAddedIndex]:=true;
      end;
    end;
  end;
  PrTypesBox.Clear;
  GState.MapCalibrationList.Lock;
  try
    for i := 0 to GState.MapCalibrationList.Count - 1 do begin
      VMapCalibration := GState.MapCalibrationList.Get(i) as IMapCalibration;
      PrTypesBox.AddItem(VMapCalibration.GetName, Pointer(VMapCalibration));
    end;
  finally
    GState.MapCalibrationList.Unlock;
  end;

  if CBscleit.ItemIndex=-1 then CBscleit.ItemIndex:=0;
  if CBmtForm.ItemIndex=-1 then CBmtForm.ItemIndex:=0;
  if CBmapDel.ItemIndex=-1 then CBmapDel.ItemIndex:=0;
  if CBMapLoad.ItemIndex=-1 then CBMapLoad.ItemIndex:=0;
  CBSclHib.ItemIndex:=0;
  FZoom_rect:=Azoom;
  setlength(FPolygonLL,length(polygon_));
  setlength(GState.LastSelectionPolygon,length(polygon_));
  for i:=0 to length(polygon_)-1 do begin
    FPolygonLL[i]:=polygon_[i];
    GState.LastSelectionPolygon[i]:=polygon_[i];
  end;
  GState.poly_zoom_save:=FZoom_rect + 1;
  vramkah:=false;
  zagran:=false;
  VConverter := GState.ViewState.GetCurrentCoordConverter;
  VZoom := FZoom_rect;
  for i:=0 to length(FPolygonLL)-1 do begin
    VPoint := VConverter.LonLat2PixelPos(FPolygonLL[i], VZoom);
    if VConverter.CheckPixelPos(VPoint , VZoom, False)
    then begin
      vramkah:=true;
    end else begin
      zagran:=true;
    end;
  end;
  if not(vramkah) then begin
    showmessage(SAS_ERR_SelectArea);
    exit;
  end else if zagran then begin
    showmessage(SAS_MSG_SelectArea);
  end;
  for i := 0 to CBFormat.Items.Count - 1 do begin
    VExportProvider := TExportProviderAbstract(CBFormat.Items.Objects[i]);
    if VExportProvider <> nil then begin
      VExportProvider.InitFrame(Azoom);
    end;
  end;
  CBFormatChange(CBFormat);

  Fmain.Enabled:=false;
  fSaveas.Visible:=true;
  CheckBox1.Checked:=false;
  CBZoomload.ItemIndex:=FZoom_rect;
  if FZoom_rect=0 then begin
    combobox.ItemIndex:=0;
  end else begin
    combobox.ItemIndex:=FZoom_rect-1;
  end;
  ComboBoxChange(self);
  CBZoomloadChange(self);
end;


procedure TFsaveas.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Fmain.TBmoveClick(Fmain);
 Fmain.Enabled:=true;
 fsaveas.visible:=false;
end;

procedure TFsaveas.CheckBox1Click(Sender: TObject);
var i:integer;
begin
 for i:=0 to CheckList.Count-1 do CheckList.Checked[i]:=CheckBox1.Checked;
end;

procedure TFsaveas.Button2Click(Sender: TObject);
var  TempPath: string;
begin
  if SelectDirectory('', '', TempPath) then
  begin
   EditPath.Text := IncludeTrailingPathDelimiter(TempPath);
  end;
end;

procedure TFsaveas.CheckBox4Click(Sender: TObject);
var i:byte;
begin
 for i:=0 to CheckListBox2.Count-1 do
  begin
   CheckListBox2.Checked[i]:=TCheckBox(sender).Checked;
  end;
end;

procedure TFsaveas.CheckBox3Click(Sender: TObject);
var i:byte;
begin
 for i:=0 to CheckListBox1.Count-1 do
  CheckListBox1.Checked[i]:=CheckBox3.Checked;
end;

procedure TFsaveas.Button3Click(Sender: TObject);
begin
 close;
end;

procedure TFsaveas.CheckBox2Click(Sender: TObject);
begin
 CheckBox7.Enabled:=CheckBox2.Checked;
 CBDateDo.Enabled:=CheckBox2.Checked;
end;

procedure TFsaveas.CBDateDoClick(Sender: TObject);
begin
 DateDo.Enabled:=CBDateDo.Checked;
end;

procedure TFsaveas.CBZoomloadChange(Sender: TObject);
var polyg:TPointArray;
    min,max:TPoint;
    numd:int64 ;
    Vmt: TMapType;
    VZoom: byte;
begin
  Vmt := TMapType(CBmapLoad.Items.Objects[CBmapLoad.ItemIndex]);
  VZoom := CBZoomload.ItemIndex;
  polyg := Vmt.GeoConvert.LonLatArray2PixelArray(FPolygonLL, VZoom);
  numd:=GetDwnlNum(min,max,polyg,true);
  label6.Caption:=SAS_STR_filesnum+': '+inttostr((max.x-min.x)div 256+1)+'x'
                  +inttostr((max.y-min.y)div 256+1)+'('+inttostr(numd)+')';
  if PageControl1.TabIndex=1 then begin
    GetMinMax(min,max,polyg,false);
    label6.Caption:=label6.Caption+', '+SAS_STR_Resolution+' '+inttostr(max.x-min.x)+'x'
                  +inttostr(max.y-min.y);
  end;
end;

procedure TFsaveas.SpeedButton1Click(Sender: TObject);
var Ini: Tinifile;
    i:integer;
begin
 if (SaveSelDialog.Execute)and(SaveSelDialog.FileName<>'') then
  begin
   If FileExists(SaveSelDialog.FileName) then DeleteFile(SaveSelDialog.FileName);
   Ini:=TiniFile.Create(SaveSelDialog.FileName);
   if length(GState.LastSelectionPolygon)>0 then
    begin
     Ini.WriteInteger('HIGHLIGHTING','zoom',GState.poly_zoom_save);
     for i:=1 to length(GState.LastSelectionPolygon) do
      begin
       Ini.WriteFloat('HIGHLIGHTING','PointLon_'+inttostr(i),GState.LastSelectionPolygon[i-1].x);
       Ini.WriteFloat('HIGHLIGHTING','PointLat_'+inttostr(i),GState.LastSelectionPolygon[i-1].y);
      end;
    end;
    ini.Free;
  end;
end;

procedure TFsaveas.TabSheet1Show(Sender: TObject);
begin
Label6.Parent:=TabSheet1;
Label3.Parent:=TabSheet1;
CBZoomload.Parent:=TabSheet1;
end;

procedure TFsaveas.TabSheet2Show(Sender: TObject);
begin
Label6.Parent:=TabSheet2;
Label3.Parent:=TabSheet2;
CBZoomload.Parent:=TabSheet2;
end;

procedure TFsaveas.TabSheet4Show(Sender: TObject);
begin
Label6.Parent:=TabSheet4;
Label3.Parent:=TabSheet4;
CBZoomload.Parent:=TabSheet4;
end;

procedure TFsaveas.CBZippedClick(Sender: TObject);
begin
 CBMove.Enabled:=not(TCheckBox(sender).Checked);
 CBReplace.Enabled:=not(TCheckBox(sender).Checked);
end;

procedure TFsaveas.CBFormatChange(Sender: TObject);
var
  VExportProvider: TExportProviderAbstract;
  i: Integer;
begin
  for i := 0 to CBFormat.Items.Count - 1 do begin
    VExportProvider := TExportProviderAbstract(CBFormat.Items.Objects[i]);
    if VExportProvider <> nil then begin
      if i = CBFormat.ItemIndex then begin
        VExportProvider.Show;
      end else begin
        VExportProvider.Hide;
      end;
    end;
  end;
end;

end.
