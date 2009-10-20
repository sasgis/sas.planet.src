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
  math,
  StdCtrls,
  ExtCtrls,
  inifiles,
  ComCtrls,
  Mask,
  filectrl,
  UTrAllLoadMap,
  UThreadScleit,
  UThreadExport,
  UGeoFun,
  UMapType,
  UResStrings,
  UOpDelTiles,
  UOpGenPreviousZoom,
  t_GeoTypes;

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
    CB2Ozi: TCheckBox;
    Label27: TLabel;
    CB2Tab: TCheckBox;
    CBSclHib: TComboBox;
    Label28: TLabel;
    SaveSelDialog: TSaveDialog;
    CBusedReColor: TCheckBox;
    CBtoWorld: TCheckBox;
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
    Panel2: TPanel;
    Label7: TLabel;
    Label14: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label21: TLabel;
    Label24: TLabel;
    Button4: TButton;
    EditPath2: TEdit;
    CheckBox9: TCheckBox;
    CkLZoomSel: TCheckListBox;
    CmBExpSat: TComboBox;
    CmBExpMap: TComboBox;
    CmBExpHib: TComboBox;
    RBSatSel: TRadioButton;
    RBMapSel: TRadioButton;
    RBHibSel: TRadioButton;
    cSatEdit: TSpinEdit;
    Label29: TLabel;
    cMapEdit: TSpinEdit;
    Label30: TLabel;
    Label31: TLabel;
    CkBNotReplase: TCheckBox;
    ComboBox: TComboBox;
    cHybEdit: TSpinEdit;
    Label33: TLabel;
    Panel3: TPanel;
    Label34: TLabel;
    Label35: TLabel;
    Label37: TLabel;
    Button5: TButton;
    EditPath3: TEdit;
    CheckBox5: TCheckBox;
    CkLZoomSel3: TCheckListBox;
    CBoxMaps2Save: TComboBox;
    ChBoxRelativePath: TCheckBox;
    SaveKMLDialog: TSaveDialog;
    ChBoxNotSaveIfNotExists: TCheckBox;
    CBSecondLoadTNE: TCheckBox;
    CBCloseWithStart: TCheckBox;
    OpenSessionDialog: TOpenDialog;
    GroupBox2: TGroupBox;
    SpeedButton2: TSpeedButton;
    CBLastSuccess: TCheckBox;
    Label32: TLabel;
    CBGenFromPrev: TCheckBox;
    Panel4: TPanel;
    Label36: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Button6: TButton;
    EditPath4: TEdit;
    CkLZoomSelYa: TCheckListBox;
    CmBExpSatYa: TComboBox;
    CmBExpMapYa: TComboBox;
    CmBExpHibYa: TComboBox;
    cSatEditYa: TSpinEdit;
    cMapEditYa: TSpinEdit;
    CkBNotReplaseYa: TCheckBox;
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
    procedure Button5Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    zoom_rect:byte;
  public
    procedure LoadRegion(APolyLL: TExtendedPointArray);
    procedure Show_(Azoom:byte;Polygon_: TExtendedPointArray);
    procedure DelRegion(APolyLL: TExtendedPointArray);
    procedure genbacksatREG(APolyLL: TExtendedPointArray);
    procedure scleitRECT(APolyLL: TExtendedPointArray);
    procedure savefilesREG(APolyLL: TExtendedPointArray);
   end;

var
  type_reg_:byte;
  Fsaveas: TFsaveas;
  PolygonLL: TExtendedPointArray;

implementation

uses
  Gauges,
  u_GlobalState,
  unit1,
  Unit4,
  UImgFun;
  
{$R *.dfm}

function PolygonSquare(Poly:TPointArray): Double;
var
  I, J, HP: Integer;
begin
  Result := 0;
  HP := High(Poly);
  for I := Low(Poly) to HP do
  begin
    if I = HP then
      J := 0
    else
      J := I + 1;
    Result := Result + (Poly[I].X + Poly[J].X) * (Poly[I].Y - Poly[J].Y);
  end;
  Result := Abs(Result) / 2;
end;

procedure TFsaveas.DelRegion(APolyLL: TExtendedPointArray);
begin
 with TOpDelTiles.Create(true,CBZoomload.ItemIndex+1,TMapType(CBmapDel.Items.Objects[CBmapDel.ItemIndex])) do
  begin
   Priority := tpLowest;
   FreeOnTerminate:=true;
   polyg := typemap.GeoConvert.PoligonProject((Zoom - 1) + 8, APolyLL);
   ProcessTiles:=GetDwnlNum(min,max,Polyg,true);
   Suspended:=false;
  end;
end;

procedure TFsaveas.savefilesREG(APolyLL: TExtendedPointArray);
var i:integer;
    path:string;
    Zoomarr:array [0..23] of boolean;
    typemaparr:array of TMapType;
    ziped:boolean;
    comprSat,comprMap,comprHyb:byte;
    RelativePath,Replace:boolean;
begin
  ziped := false;
  comprSat := 80;
  comprMap := 9;
  comprHyb := 80;
 case CBFormat.ItemIndex of
  4,5: begin
        for i:=0 to 23 do ZoomArr[i]:=CkLZoomSel.Checked[i];
        setlength(typemaparr,3);
        if CmBExpSat.Items.Objects[CmBExpSat.ItemIndex]<>nil then
          TMapType(CmBExpSat.Items.Objects[CmBExpSat.ItemIndex]).active:=RBSatSel.Checked;
        if CmBExpMap.Items.Objects[CmBExpMap.ItemIndex]<>nil then
          TMapType(CmBExpMap.Items.Objects[CmBExpMap.ItemIndex]).active:=RBMapSel.Checked;
        if CmBExpHib.Items.Objects[CmBExpHib.ItemIndex]<>nil then
          TMapType(CmBExpHib.Items.Objects[CmBExpHib.ItemIndex]).active:=RBHibSel.Checked;
        comprSat:=cSatEdit.Value;
        comprMap:=cMapEdit.Value;
        comprHyb:=cHybEdit.Value;
        typemaparr[0]:=TMapType(CmBExpSat.Items.Objects[CmBExpSat.ItemIndex]);
        typemaparr[1]:=TMapType(CmBExpMap.Items.Objects[CmBExpMap.ItemIndex]);
        typemaparr[2]:=TMapType(CmBExpHib.Items.Objects[CmBExpHib.ItemIndex]);
        path:=IncludeTrailingPathDelimiter(EditPath2.Text);
        RelativePath:=false;
        Replace:=(not CkBNotReplase.Checked);
       end;
    7: begin
        for i:=0 to 23 do ZoomArr[i]:=CkLZoomSelYa.Checked[i];
        setlength(typemaparr,3);
        typemaparr[0]:=TMapType(CmBExpSatYa.Items.Objects[CmBExpSatYa.ItemIndex]);
        typemaparr[1]:=TMapType(CmBExpMapYa.Items.Objects[CmBExpMapYa.ItemIndex]);
        typemaparr[2]:=TMapType(CmBExpHibYa.Items.Objects[CmBExpHibYa.ItemIndex]);
        comprSat:=cSatEditYa.Value;
        comprMap:=cMapEditYa.Value;
        comprHyb:=cSatEditYa.Value;
        path:=IncludeTrailingPathDelimiter(EditPath4.Text);
        RelativePath:=false;
        Replace:=CkBNotReplaseYa.Checked;
       end;
    6: begin
        for i:=0 to 23 do ZoomArr[i]:=CkLZoomSel3.Checked[i];
        setlength(typemaparr,3);
        typemaparr[0]:=TMapType(CBoxMaps2Save.Items.Objects[CBoxMaps2Save.ItemIndex]);
        ziped:=false;
        path:=EditPath3.Text;
        RelativePath:=ChBoxRelativePath.Checked;
        Replace:=ChBoxNotSaveIfNotExists.Checked;
       end;
  else begin
        for i:=0 to 23 do ZoomArr[i]:=CheckListBox2.Checked[i];
        for i:=0 to CheckListBox1.Items.Count-1 do
         if CheckListBox1.Checked[i] then
          begin
           setlength(typemaparr,length(typemaparr)+1);
           typemaparr[length(typemaparr)-1]:=TMapType(CheckListBox1.Items.Objects[i]);
          end;
        ziped:=CBZipped.Checked;
        path:=IncludeTrailingPathDelimiter(EditPath.Text);
        RelativePath:=false;
        Replace:=CBReplace.Checked;
       end;
 end;
 with ThreadExport.Create(false,path,APolyLL,ZoomArr,typemaparr,CBMove.Checked,Replace,ziped,CBFormat.ItemIndex,comprSat,comprMap,comprHyb,RelativePath) do
  begin
   Priority := tpLowest;
   FreeOnTerminate:=true;
  end;
end;

procedure TFsaveas.LoadRegion(APolyLL: TExtendedPointArray);
var smb:TMapType;
    polyg:TPointArray;
    VZoom: byte;
begin
 smb:=TMapType(CBmapLoad.Items.Objects[CBmapLoad.ItemIndex]);
 VZoom := CBZoomload.ItemIndex;
 polyg := smb.GeoConvert.PoligonProject(VZoom + 8, APolyLL);
 with ThreadAllLoadMap.Create(false,Polyg,CheckBox2.Checked,CheckBox7.Checked,CBDateDo.Checked,CBSecondLoadTNE.Checked,strtoint(CBZoomload.Text),smb,DateDo.DateTime) do
  begin
   OnTerminate:=Fmain.ThreadDone;
   Priority := tpLower;
   FreeOnTerminate:=true;
  end;
  polyg := nil;
end;

procedure TFsaveas.genbacksatREG(APolyLL: TExtendedPointArray);
var i:integer;
begin
 with TOpGenPreviousZoom.Create(ComboBox.ItemIndex+2,TMapType(CBmtForm.Items.Objects[CBmtForm.ItemIndex])) do
  begin
   Priority := tpLowest;
   FreeOnTerminate:=true;
   Replace:=CBzamena.Checked;
   savefull:=CBsavefull.Checked;
   GenFormPrev:=CBGenFromPrev.Checked;
   SetLength(PolygLL,length(APolyLL));
   CopyMemory(@PolygLL[0],@APolyLL[0],length(APolyLL)*sizeOf(TExtendedPoint));
   for i:=0 to FromZoom-2 do
    if CheckList.Checked[i] then
     begin
      SetLength(InZooms,Length(InZooms)+1);
      InZooms[Length(InZooms)-1]:=FromZoom-i-1;
     end;
   Suspended:=false;
  end;
end;

procedure TFsaveas.scleitRECT(APolyLL: TExtendedPointArray);
var Amt,Hmt:TMapType;
    polyg:TPointArray;
    VZoom: byte;
begin
 Amt:=TMapType(CBscleit.Items.Objects[CBscleit.ItemIndex]);
 Hmt:=TMapType(CBSclHib.Items.Objects[CBSclHib.ItemIndex]);
 VZoom := CBZoomload.ItemIndex;
 polyg := Amt.GeoConvert.PoligonProject(VZoom + 8, APolyLL);
 if (FMain.SaveDialog1.Execute)then
  begin
   with ThreadScleit.Create(true,FMain.SaveDialog1.FileName,polyg,EditNTg.Value,EditNTv.Value,CBZoomload.ItemIndex+1,Amt,Hmt,0,CB2Ozi.Checked,CB2Tab.Checked,CBtoWorld.Checked,CBusedReColor.Checked) do
    begin
     ProcessTiles:=GetDwnlNum(PolyMin,polyMax,polyg,true);
     GetMinMax(PolyMin,polyMax,polyg,false);
     Priority := tpLower;
     FreeOnTerminate:=true;
     Suspended:=false;
    end;
  end;
  Polyg := nil;
end;

procedure TFsaveas.Button1Click(Sender: TObject);
begin
 case PageControl1.ActivePage.Tag of
  0: LoadRegion(PolygonLL);
  1: scleitRECT(PolygonLL);
  2: genbacksatREG(PolygonLL);
  3: if (MessageBox(handle,pchar(SAS_MSG_youasure),pchar(SAS_MSG_coution),36)=IDYES)
      then delRegion(PolygonLL);
  4: savefilesREG(PolygonLL);
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

procedure TFsaveas.Show_(Azoom:byte;Polygon_: TExtendedPointArray);
var i:integer;
    XX:tpOINT;
    vramkah,zagran:boolean;
begin
 CBSecondLoadTNE.Enabled:=GState.SaveTileNotExists;
 CBZoomload.Items.Clear;
 ComboBox.Items.Clear;
 CkLZoomSel.Items.Clear;
 CheckListBox2.Items.Clear;
 CkLZoomSelYa.Items.Clear;
 for i:=1 to 24 do
  begin
   CBZoomload.Items.Add(inttostr(i));
   if i>1 then ComboBox.Items.Add(inttostr(i));
   CkLZoomSel.Items.Add(inttostr(i));
   CkLZoomSel3.Items.Add(inttostr(i));
   CheckListBox2.Items.Add(inttostr(i));
   CkLZoomSelYa.Items.Add(inttostr(i));
  end;
 DateDo.Date:=now;
 CBMapLoad.Items.Clear;
 CBscleit.Items.Clear;
 CBmtForm.Items.Clear;
 CBmapDel.Items.Clear;
 CheckListBox1.Items.Clear;
 CBSclHib.Items.Clear;
 CBSclHib.Items.Add(SAS_STR_No);
 CmBExpSat.items.Clear;
 CmBExpMap.items.Clear;
 CmBExpHib.items.Clear;
 CmBExpSat.Items.AddObject(SAS_STR_No,nil);
 CmBExpMap.Items.AddObject(SAS_STR_No,nil);
 CmBExpHib.Items.AddObject(SAS_STR_No,nil);
 CmBExpSatYa.items.Clear;
 CmBExpMapYa.items.Clear;
 CmBExpHibYa.items.Clear;
 CmBExpSatYa.Items.AddObject(SAS_STR_No,nil);
 CmBExpMapYa.Items.AddObject(SAS_STR_No,nil);
 CmBExpHibYa.Items.AddObject(SAS_STR_No,nil);
 For i:=0 to length(MapType)-1 do
  begin
   if (MapType[i].Usedwn) then
    begin
     CBMapLoad.Items.AddObject(MapType[i].name,MapType[i]);
     if (MapType[i].active)and(not(MapType[i].asLayer)) then CBMapLoad.ItemIndex:=CBMapLoad.Items.IndexOfObject(MapType[i]);
    end;
   if MapType[i].Usestick then
    begin
     CBscleit.Items.AddObject(MapType[i].name,MapType[i]);
     if (MapType[i].asLayer)and(MapType[i].ext<>'.kml') then CBSclHib.Items.AddObject(MapType[i].name,MapType[i]);
     if (MapType[i].active)and(not(MapType[i].asLayer)) then CBscleit.ItemIndex:=CBscleit.Items.IndexOfObject(MapType[i]);
    end;
   if (MapType[i].UseGenPrevious) then
    begin
     CBmtForm.Items.AddObject(MapType[i].name,MapType[i]);
     if (MapType[i].active)and(not(MapType[i].asLayer)) then CBmtForm.ItemIndex:=CBmtForm.Items.IndexOfObject(MapType[i]);
    end;
   if (MapType[i].Usedel) then
    begin
     CBmapDel.Items.AddObject(MapType[i].name,MapType[i]);
     if (MapType[i].active)and(not(MapType[i].asLayer)) then CBmapDel.ItemIndex:=CBmapDel.Items.IndexOfObject(MapType[i]);
    end;
   if (MapType[i].UseSave) then
    begin
     if((MapType[i].ext='.jpg')or(MapType[i].ext='.png')or(MapType[i].ext='.gif')) then
      if (not(MapType[i].asLayer)) then begin
                                         CmBExpSat.Items.AddObject(MapType[i].name,MapType[i]);
                                         CmBExpMap.Items.AddObject(MapType[i].name,MapType[i]);
                                         CmBExpSatYa.Items.AddObject(MapType[i].name,MapType[i]);
                                         CmBExpMapYa.Items.AddObject(MapType[i].name,MapType[i]);
                                         if MapType[i].active then begin
                                                                    CmBExpSat.ItemIndex:=CmBExpSat.Items.IndexOfObject(MapType[i]);
                                                                    CmBExpMap.ItemIndex:=CmBExpSat.Items.IndexOfObject(MapType[i]);
                                                                    CmBExpSatYa.ItemIndex:=CmBExpSatYa.Items.IndexOfObject(MapType[i]);
                                                                    CmBExpMapYa.ItemIndex:=CmBExpSatYa.Items.IndexOfObject(MapType[i]);
                                                                   end;
                                        end
                                   else if(MapType[i].ext='.png') then
                                        begin
                                         CmBExpHib.Items.AddObject(MapType[i].name,MapType[i]);
                                         CmBExpHibYa.Items.AddObject(MapType[i].name,MapType[i]);
                                         if (MapType[i].active)and(CmBExpHib.ItemIndex=-1) then begin
                                           CmBExpHib.ItemIndex:=CmBExpHib.Items.IndexOfObject(MapType[i]);
                                           CmBExpHibYa.ItemIndex:=CmBExpHibYa.Items.IndexOfObject(MapType[i]);
                                         end;
                                        end;
     CheckListBox1.Items.AddObject(MapType[i].name,MapType[i]);
     if (MapType[i].active)and(not(MapType[i].asLayer)) then CheckListBox1.Checked[CheckListBox1.Items.IndexOfObject(MapType[i])]:=true;
     CBoxMaps2Save.Items.AddObject(MapType[i].name,MapType[i]);
     if (MapType[i].active)and(not(MapType[i].asLayer)) then CBoxMaps2Save.ItemIndex:=CheckListBox1.Items.IndexOfObject(MapType[i]);
    end;
  end;
 if CBscleit.ItemIndex=-1 then CBscleit.ItemIndex:=0;
 if CBmtForm.ItemIndex=-1 then CBmtForm.ItemIndex:=0;
 if CBmapDel.ItemIndex=-1 then CBmapDel.ItemIndex:=0;
 if CBMapLoad.ItemIndex=-1 then CBMapLoad.ItemIndex:=0;
 if CmBExpSat.ItemIndex=-1 then CmBExpSat.ItemIndex:=1;
 if CmBExpMap.ItemIndex=-1 then CmBExpMap.ItemIndex:=0;
 if CmBExpHib.ItemIndex=-1 then CmBExpHib.ItemIndex:=0;
 if CmBExpSatYa.ItemIndex=-1 then CmBExpSatYa.ItemIndex:=1;
 if CmBExpMapYa.ItemIndex=-1 then CmBExpMapYa.ItemIndex:=0;
 if CmBExpHibYa.ItemIndex=-1 then CmBExpHibYa.ItemIndex:=0;
 CBSclHib.ItemIndex:=0;
 zoom_rect:=Azoom;
 setlength(polygonLL,0);
 setlength(poly_save,0);
 for i:=0 to length(polygon_)-1 do
  begin
   setlength(poly_save,i+1);
   setlength(polygonLL,i+1);
   polygonLL[i]:=polygon_[i];
   poly_save[i]:=polygon_[i];
  end;
 poly_zoom_save:=zoom_rect;
 vramkah:=false;
 zagran:=false;
 for i:=0 to length(polygonLL)-1 do
   if ((sat_map_both.GeoConvert.LonLat2Pos(polygonLL[i],(zoom_rect - 1) + 8).y>=0)and
      (sat_map_both.GeoConvert.LonLat2Pos(polygonLL[i],(zoom_rect - 1) + 8).y<=zoom[zoom_rect]))then vramkah:=true
                                              else zagran:=true;
 if not(vramkah)
  then begin
        showmessage(SAS_ERR_SelectArea);
        exit;
       end
  else if zagran then showmessage(SAS_MSG_SelectArea);

 Fmain.Enabled:=false;
 fSaveas.Visible:=true;
 CheckBox1.Checked:=false;
 CBZoomload.ItemIndex:=zoom_rect-1;
 if zoom_rect=1 then combobox.ItemIndex:=0
                else combobox.ItemIndex:=zoom_rect-2;
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
   EditPath.Text := String(TempPath)+'\';
   EditPath2.Text := String(TempPath)+'\';
   EditPath3.Text := String(TempPath)+'\';
   EditPath4.Text := String(TempPath)+'\';
  end;
end;

procedure TFsaveas.CheckBox4Click(Sender: TObject);
var i:byte;
begin
 for i:=0 to CheckListBox2.Count-1 do
  begin
   CheckListBox2.Checked[i]:=TCheckBox(sender).Checked;
   CkLZoomSel.Checked[i]:=TCheckBox(Sender).Checked;
   CkLZoomSel3.Checked[i]:=TCheckBox(Sender).Checked;
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
    numd:integer;
    Vmt: TMapType;
    VZoom: byte;
begin
  Vmt := TMapType(CBmapLoad.Items.Objects[CBmapLoad.ItemIndex]);
  VZoom := CBZoomload.ItemIndex;
  polyg := Vmt.GeoConvert.PoligonProject(VZoom + 8, PolygonLL);
  numd:=GetDwnlNum(min,max,polyg,true);
  label6.Caption:=SAS_STR_filesnum+': '+inttostr((max.x-min.x)div 256+1)+'x'
                  +inttostr((max.y-min.y)div 256+1)+'('+inttostr(numd)+')';
end;

procedure TFsaveas.SpeedButton1Click(Sender: TObject);
var Ini: Tinifile;
    i:integer;
begin
 if (SaveSelDialog.Execute)and(SaveSelDialog.FileName<>'') then
  begin
   If FileExists(SaveSelDialog.FileName) then DeleteFile(SaveSelDialog.FileName);
   Ini:=TiniFile.Create(SaveSelDialog.FileName);
   if length(poly_save)>0 then
    begin
     Ini.WriteInteger('HIGHLIGHTING','zoom',poly_zoom_save);
     for i:=1 to length(poly_save) do
      begin
       Ini.WriteFloat('HIGHLIGHTING','PointLon_'+inttostr(i),poly_save[i-1].x);
       Ini.WriteFloat('HIGHLIGHTING','PointLat_'+inttostr(i),poly_save[i-1].y);
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
begin
 Panel4.Visible:=(CBFormat.ItemIndex in [7]);
 Panel3.Visible:=(CBFormat.ItemIndex in [6]);
 Panel2.Visible:=(CBFormat.ItemIndex in [4,5]);
 Panel1.Visible:=(CBFormat.ItemIndex in [0,1,2,3]);
end;

procedure TFsaveas.Button5Click(Sender: TObject);
begin
 if SaveKMLDialog.Execute then
  EditPath3.Text:=SaveKMLDialog.FileName;
end;

procedure TFsaveas.SpeedButton2Click(Sender: TObject);
begin
 if (OpenSessionDialog.Execute)and(FileExists(OpenSessionDialog.FileName)) then
  begin
   Fmain.Enabled:=true;
   with ThreadAllLoadMap.Create(false,OpenSessionDialog.FileName,CBLastSuccess.Checked) do
    begin
     OnTerminate:=Fmain.ThreadDone;
     Priority := tpLower;
     FreeOnTerminate:=true;
    end;
   if CBCloseWithStart.Checked then close;
  end;
end;

end.
