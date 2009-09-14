unit USaveas;
interface
uses
 Windows, SysUtils, Graphics, Forms, CheckLst, Classes, Controls,
 math, StdCtrls, ExtCtrls, UTrAllLoadMap, UThreadScleit, Mask, UThreadExport,
 ComCtrls, UGeoFun, inifiles, UMapType, UResStrings,
 filectrl, Buttons, Spin, UOpDelTiles, UOpGenPreviousZoom, Dialogs;

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
    procedure LoadRegion(APolyLL:array of TExtendedPoint);
    procedure Show_(Azoom:byte;Polygon_:array of TExtendedPoint);
    procedure DelRegion(APolyLL:array of TExtendedPoint);
    procedure genbacksatREG(APolyLL:array of TExtendedPoint);
    procedure formatePoligon(AType:PMapType;Anewzoom:byte;Apolyg:array of TExtendedPoint; var resApolyg:array of TPoint);
    procedure scleitRECT(APolyLL:array of TExtendedPoint);
    procedure savefilesREG(APolyLL:array of TExtendedPoint);
    function GetDwnlNum(var min,max:TPoint; Polyg:array of Tpoint; getNum:boolean):longint;
    procedure GetMinMax(var min,max:TPoint; Polyg:array of Tpoint;round_:boolean);
   end;

var
  type_reg_:byte;
  Fsaveas: TFsaveas;
  PolygonLL:array of TExtendedPoint;
  function RgnAndRgn(Polyg:array of TPoint;x,y:integer;prefalse:boolean):boolean;

implementation
uses unit1, Gauges, Unit4, UImgFun;
{$R *.dfm}

function PolygonSquare(Poly:array of TPoint): Double;
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

function RgnAndRgn(Polyg:array of TPoint;x,y:integer;prefalse:boolean):boolean;
var i,xm128,ym128,xp128,yp128:integer;
begin
 xm128:=x-128;
 ym128:=y-128;
 if (prefalse=false)and(PtInPolygon(Point(xm128,ym128),polyg)) then begin result:=true; exit; end;
 xp128:=x+128;
 if (prefalse=false)and(PtInPolygon(Point(xp128,ym128),polyg)) then begin result:=true; exit; end;
 yp128:=y+128;
 if PtInPolygon(Point(xp128,yp128),polyg) then begin result:=true; exit; end;
 if PtInPolygon(Point(xm128,yp128),polyg) then begin result:=true; exit; end;
{
 if PtInRgn(polyg,Point(x-128,y-128)) then begin result:=true; exit; end;
 if PtInRgn(polyg,Point(x+128,y-128)) then begin result:=true; exit; end;
 if PtInRgn(polyg,Point(x+128,y+128)) then begin result:=true; exit; end;
 if PtInRgn(polyg,Point(x-128,y+128)) then begin result:=true; exit; end;
}
 for i:=0 to length(polyg)-2 do
  begin
   if (polyg[i].x<xp128)and(polyg[i].x>xm128)and(polyg[i].y<yp128)and(polyg[i].y>ym128)
    then begin result:=true; exit; end;
  end;
 result:=false;
end;

procedure TFsaveas.formatePoligon(AType:PMapType;Anewzoom:byte;Apolyg:array of TExtendedPoint; var resApolyg:array of TPoint);
var i:integer;
begin
 for i:=0 to length(APolyg)-1 do
  begin
   resAPolyg[i]:=GLonLat2Pos(Apolyg[i],Anewzoom,Atype);
//   resAPolyg[i]:=Point(round(resAPolyg[i].X*intpower(2,Anewzoom-zoom_rect)),round(resAPolyg[i].Y*intpower(2,Anewzoom-zoom_rect)));
//   if resAPolyg[i].x<0 then resAPolyg[i].x:=1;
   if resAPolyg[i].y<0 then resAPolyg[i].y:=1;
//   if resAPolyg[i].x>zoom[AnewZoom] then resAPolyg[i].x:=zoom[AnewZoom]-1;
   if resAPolyg[i].y>zoom[AnewZoom] then resAPolyg[i].y:=zoom[AnewZoom]-1;
  end;
end;

Procedure TFsaveas.GetMinMax(var min,max:TPoint; Polyg:array of Tpoint;round_:boolean);
var i:integer;
begin
 max:=Polyg[0];
 min:=Polyg[0];
 //Polyg[0].x:=(Polyg[0].x-(Polyg[0].x mod 256)+128) div 128;
 //Polyg[0].y:=(Polyg[0].y-(Polyg[0].y mod 256)+128) div 128;
 for i:=1 to length(Polyg)-1 do
  begin
   if min.x>Polyg[i].x then min.x:=Polyg[i].x;
   if min.y>Polyg[i].y then min.y:=Polyg[i].y;
   if max.x<Polyg[i].x then max.x:=Polyg[i].x;
   if max.y<Polyg[i].y then max.y:=Polyg[i].y;
   //Polyg[i].x:=(Polyg[i].x-(Polyg[i].x mod 256)+128) div 128;
   //Polyg[i].y:=(Polyg[i].y-(Polyg[i].y mod 256)+128) div 128;
  end;
 if round_ then
  begin
   max.X:=max.X-1;
   max.Y:=max.Y-1;
   min.X:=min.X+1;
   min.Y:=min.Y+1;
   min.X:=min.x-(min.X mod 256)+128;
   max.X:=max.x-(max.X mod 256)+128;//max.X+(256-((max.X) mod 256))-128;
   min.y:=min.y-(min.y mod 256)+128;
   max.y:=max.y-(max.y mod 256)+128;//max.y+(256-((max.y) mod 256))-128;
  end;
end;

function TFsaveas.GetDwnlNum(var min,max:TPoint; Polyg:array of Tpoint; getNum:boolean):longint;
var i,j:integer;
    prefalse:boolean;
begin
 GetMinMax(min,max,polyg,true);
 result:=0;
 if getNum then
  if (length(Polyg)=5)and(Polyg[0].x=Polyg[3].x)and(Polyg[1].x=Polyg[2].x)
                      and(Polyg[0].y=Polyg[1].y)and(Polyg[2].y=Polyg[3].y)
    then result:=((max.X-min.X) div 256+1)*((max.Y-min.Y) div 256+1)
    else begin
          i:=min.X;
          while i<=max.x do
          begin
           j:=min.y;
           prefalse:=false;
           while j<=max.y do
            begin
             prefalse:=not(RgnAndRgn(Polyg,i,j,prefalse));
             if not(prefalse) then inc(result);
             inc(j,256);
            end;
           inc(i,256);
          end;
         end;
// if getNum then result:=((max.X-min.X) div 256+1{integer((max.X-min.X)<1)})*
//                        ((max.Y-min.Y) div 256+1{integer((max.Y-min.Y)<1)});
// result:=round(PolygonSquare(Polyg)/2);
 max.X:=max.X+1;
 max.Y:=max.Y+1;
{ min.X:=min.X-1;
 min.Y:=min.Y-1; }
{ r:=PolygonSquare(Polyg);
 result:=round(r/(256*256))+1; }
{ if getNum then
  begin
   i:=min.x;
   while i<=max.X do
    begin
     j:=min.Y;
     while j<=max.y do
      begin
       if RgnAndRgn(Polyg,i,j) then inc(result);
       inc(n); if n>499999 then exit;
       inc(j,256);
      end;
     inc(i,256);
    end;
  end;  }
end;

procedure TFsaveas.DelRegion(APolyLL:array of TExtendedPoint);
begin
 with TOpDelTiles.Create(true,CBZoomload.ItemIndex+1,PMapType(CBmapDel.Items.Objects[CBmapDel.ItemIndex])) do
  begin
   //OnTerminate:=Fmain.ThreadExportDone;
   Priority := tpLowest;
   FreeOnTerminate:=true;
   SetLength(Polyg,length(APolyLL));
   Fsaveas.formatepoligon(typemap,zoom,APolyLL,polyg);
   ProcessTiles:=Fsaveas.GetDwnlNum(min,max,Polyg,true);
   Suspended:=false;
  end;
end;

procedure TFsaveas.savefilesREG(APolyLL:array of TExtendedPoint);
var i:integer;
    path:string;
    Zoomarr:array [0..23] of boolean;
    typemaparr:array of PMapType;
    ziped:boolean;
    RelativePath,Replace:boolean;
begin
 case CBFormat.ItemIndex of
  4,5: begin
        for i:=0 to 23 do ZoomArr[i]:=CkLZoomSel.Checked[i];
        setlength(typemaparr,3);
        if CmBExpSat.Items.Objects[CmBExpSat.ItemIndex]<>nil then
          PMapType(CmBExpSat.Items.Objects[CmBExpSat.ItemIndex]).active:=RBSatSel.Checked;
        if CmBExpMap.Items.Objects[CmBExpMap.ItemIndex]<>nil then
          PMapType(CmBExpMap.Items.Objects[CmBExpMap.ItemIndex]).active:=RBMapSel.Checked;
        if CmBExpHib.Items.Objects[CmBExpHib.ItemIndex]<>nil then
          PMapType(CmBExpHib.Items.Objects[CmBExpHib.ItemIndex]).active:=RBHibSel.Checked;
        typemaparr[0]:=PMapType(CmBExpSat.Items.Objects[CmBExpSat.ItemIndex]);
        typemaparr[1]:=PMapType(CmBExpMap.Items.Objects[CmBExpMap.ItemIndex]);
        typemaparr[2]:=PMapType(CmBExpHib.Items.Objects[CmBExpHib.ItemIndex]);
        path:=EditPath2.Text;
        RelativePath:=false;
        Replace:=(not CkBNotReplase.Checked);
       end;
    6: begin
        for i:=0 to 23 do ZoomArr[i]:=CkLZoomSel3.Checked[i];
        setlength(typemaparr,3);
        typemaparr[0]:=PMapType(CBoxMaps2Save.Items.Objects[CBoxMaps2Save.ItemIndex]);
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
           typemaparr[length(typemaparr)-1]:=PMapType(CheckListBox1.Items.Objects[i]);
          end;
        ziped:=CBZipped.Checked;
        path:=EditPath.Text;
        RelativePath:=false;
        Replace:=CBReplace.Checked;
       end;
 end;
 with ThreadExport.Create(false,path,APolyLL,ZoomArr,typemaparr,CBMove.Checked,Replace,ziped,CBFormat.ItemIndex,cSatEdit.Value,cMapEdit.Value,cHybEdit.Value,RelativePath) do
  begin
   OnTerminate:=Fmain.ThreadExportDone;
   Priority := tpLowest;
   FreeOnTerminate:=true;
  end;
end;

procedure TFsaveas.LoadRegion(APolyLL:array of TExtendedPoint);
var smb:PMapType;
    polyg:array of TPoint;
begin
 smb:=PMapType(CBmapLoad.Items.Objects[CBmapLoad.ItemIndex]);
 setlength(polyg,length(APolyLL));
 formatepoligon(smb,CBZoomload.ItemIndex+1,APolyLL,polyg);
 with ThreadAllLoadMap.Create(false,Polyg,3,CheckBox2.Checked,CheckBox7.Checked,CBDateDo.Checked,CBSecondLoadTNE.Checked,strtoint(CBZoomload.Text),smb,DateDo.DateTime) do
  begin
   OnTerminate:=Fmain.ThreadDone;
   Priority := tpLower;
   FreeOnTerminate:=true;
  end;
end;

procedure TFsaveas.genbacksatREG(APolyLL:array of TExtendedPoint);
var i:integer;
begin
 with TOpGenPreviousZoom.Create(true,ComboBox.ItemIndex+2,PMapType(CBmtForm.Items.Objects[CBmtForm.ItemIndex])) do
  begin
   //OnTerminate:=Fmain.ThreadExportDone;
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

procedure TFsaveas.scleitRECT(APolyLL:array of TExtendedPoint);
var Amt,Hmt:PMapType;
    polyg:array of TPoint;
begin
 Amt:=PMapType(CBscleit.Items.Objects[CBscleit.ItemIndex]);
 Hmt:=PMapType(CBSclHib.Items.Objects[CBSclHib.ItemIndex]);
 setLength(polyg,length(APolyLL));
 formatepoligon(Amt,CBZoomload.ItemIndex+1,APolyLL,polyg);
 if (FMain.SaveDialog1.Execute)then
  begin
   with ThreadScleit.Create(true,FMain.SaveDialog1.FileName,polyg{[polyg[0],polyg[2]]},EditNTg.Value,EditNTv.Value,CBZoomload.ItemIndex+1,Amt,Hmt,0,CB2Ozi.Checked,CB2Tab.Checked,CBtoWorld.Checked,CBusedReColor.Checked) do
    begin
     ProcessTiles:=GetDwnlNum(PolyMin,polyMax,polyg,true);
     GetMinMax(PolyMin,polyMax,polyg,false);
     {dec(PolyMin.X,128);
     dec(PolyMin.Y,128);
     inc(PolyMax.X,127);
     inc(PolyMax.Y,127); }
     OnTerminate:=Fmain.ThreadSclDone;
     Priority := tpLower;
     FreeOnTerminate:=true;
     Suspended:=false;
    end;
  end;
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

procedure TFsaveas.Show_(Azoom:byte;Polygon_:array of TExtendedPoint);
var i:integer;
    XX:tpOINT;
    vramkah,zagran:boolean;
begin
 CBSecondLoadTNE.Enabled:=SaveTileNotExists;
 CBZoomload.Items.Clear;
 ComboBox.Items.Clear;
 CkLZoomSel.Items.Clear;
 CheckListBox2.Items.Clear;
 for i:=1 to 24 do
  begin
   CBZoomload.Items.Add(inttostr(i));
   if i>1 then ComboBox.Items.Add(inttostr(i));
   CkLZoomSel.Items.Add(inttostr(i));
   CkLZoomSel3.Items.Add(inttostr(i));
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
 CmBExpSat.items.Clear;
 CmBExpMap.items.Clear;
 CmBExpHib.items.Clear;
 CmBExpSat.Items.AddObject(SAS_STR_No,nil);
 CmBExpMap.Items.AddObject(SAS_STR_No,nil);
 CmBExpHib.Items.AddObject(SAS_STR_No,nil);
 For i:=0 to length(MapType)-1 do
  begin
   if (MapType[i].Usedwn) then
    begin
     CBMapLoad.Items.AddObject(MapType[i].name,@MapType[i]);
     if (MapType[i].active)and(not(MapType[i].asLayer)) then CBMapLoad.ItemIndex:=CBMapLoad.Items.IndexOfObject(@MapType[i]);
    end;
   if MapType[i].Usestick then
    begin
     CBscleit.Items.AddObject(MapType[i].name,@MapType[i]);
     if (MapType[i].asLayer)and(MapType[i].ext<>'.kml') then CBSclHib.Items.AddObject(MapType[i].name,@MapType[i]);
     if (MapType[i].active)and(not(MapType[i].asLayer)) then CBscleit.ItemIndex:=CBscleit.Items.IndexOfObject(@MapType[i]);
    end;
   if (MapType[i].UseGenPrevious) then
    begin
     CBmtForm.Items.AddObject(MapType[i].name,@MapType[i]);
     if (MapType[i].active)and(not(MapType[i].asLayer)) then CBmtForm.ItemIndex:=CBmtForm.Items.IndexOfObject(@MapType[i]);
    end;
   if (MapType[i].Usedel) then
    begin
     CBmapDel.Items.AddObject(MapType[i].name,@MapType[i]);
     if (MapType[i].active)and(not(MapType[i].asLayer)) then CBmapDel.ItemIndex:=CBmapDel.Items.IndexOfObject(@MapType[i]);
    end;
   if (MapType[i].UseSave) then
    begin
     if((MapType[i].ext='.jpg')or(MapType[i].ext='.png')or(MapType[i].ext='.gif')) then
      if (not(MapType[i].asLayer)) then begin
                                         CmBExpSat.Items.AddObject(MapType[i].name,@MapType[i]);
                                         CmBExpMap.Items.AddObject(MapType[i].name,@MapType[i]);
                                         if MapType[i].active then begin
                                                                    CmBExpSat.ItemIndex:=CmBExpSat.Items.IndexOfObject(@MapType[i]);
                                                                    CmBExpMap.ItemIndex:=CmBExpSat.Items.IndexOfObject(@MapType[i]);
                                                                   end;
                                        end
                                   else if(MapType[i].ext='.png') then
                                        begin
                                         CmBExpHib.Items.AddObject(MapType[i].name,@MapType[i]);
                                         if (MapType[i].active)and(CmBExpHib.ItemIndex=-1) then CmBExpHib.ItemIndex:=CmBExpHib.Items.IndexOfObject(@MapType[i]);
                                        end;
     CheckListBox1.Items.AddObject(MapType[i].name,@MapType[i]);
     if (MapType[i].active)and(not(MapType[i].asLayer)) then CheckListBox1.Checked[CheckListBox1.Items.IndexOfObject(@MapType[i])]:=true;
     CBoxMaps2Save.Items.AddObject(MapType[i].name,@MapType[i]);
     if (MapType[i].active)and(not(MapType[i].asLayer)) then CBoxMaps2Save.ItemIndex:=CheckListBox1.Items.IndexOfObject(@MapType[i]);
    end;
  end;
 if CBscleit.ItemIndex=-1 then CBscleit.ItemIndex:=0;
 if CBmtForm.ItemIndex=-1 then CBmtForm.ItemIndex:=0;
 if CBmapDel.ItemIndex=-1 then CBmapDel.ItemIndex:=0;
 if CBMapLoad.ItemIndex=-1 then CBMapLoad.ItemIndex:=0;
 if CmBExpSat.ItemIndex=-1 then CmBExpSat.ItemIndex:=1;
 if CmBExpMap.ItemIndex=-1 then CmBExpMap.ItemIndex:=0;
 if CmBExpHib.ItemIndex=-1 then CmBExpHib.ItemIndex:=0;
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
   if {((polygonLL[i].X>=-180)and(polygonLL[i].X<=180))and }
      ((GLonLat2Pos(polygonLL[i],zoom_rect,sat_map_both).y>=0)and
      (GLonLat2Pos(polygonLL[i],zoom_rect,sat_map_both).y<=zoom[zoom_rect]))then vramkah:=true
                                              else zagran:=true;
 if not(vramkah)
  then begin
        showmessage(SAS_ERR_SelectArea);
        exit;
       end
  else if zagran then showmessage(SAS_MSG_SelectArea);

 Fmain.Enabled:=false;
 fSaveas.Visible:=true;
{ if (polygonLL[0].X=polygonLL[3].X)and(polygonLL[1].X=polygonLL[2].X)
    and(polygonLL[0].Y=polygonLL[1].Y)and(polygonLL[2].Y=polygonLL[3].Y)
  then begin
        PageControl1.Pages[1].TabVisible:=true;
        type_reg_:=1;
        //Label6.Visible:=true
       end
  else begin
        PageControl1.Pages[1].TabVisible:=false;
        type_reg_:=2;
        //Label6.Visible:=false
       end;     }
// Fmain.Enabled:=false;
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
var polyg:array of Tpoint;
    min,max:TPoint;
    numd:integer;
begin
 SetLength(polyg,length(PolygonLL));
 formatePoligon(PMapType(CBmapLoad.Items.Objects[CBmapLoad.ItemIndex]),CBZoomload.ItemIndex+1,polygonLL,polyg);
 numd:=GetDwnlNum(min,max,polyg,true);
{ polyg[0]:=Point(polyg[0].x-(polyg[0].x mod 256),polyg[0].y-(polyg[0].y mod 256));
 polyg[2]:=Point(polyg[2].x-(polyg[1].x mod 256),polyg[2].y-(polyg[2].y mod 256));
 b2:=inttostr((((polyg[2].x-polyg[0].x)div 256)+1)*((polyg[2].y-polyg[0].y)div 256+1)); }
 label6.Caption:=SAS_STR_filesnum+': '+inttostr((max.x-min.x)div 256+1{integer((max.X-min.X)<1)})+'x'
                  +inttostr((max.y-min.y)div 256+1{integer((max.y-min.y)<1)})+'('+inttostr(numd)+')'
                  {+'('+inttostr(GetTickCount-tik)+')'};

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
