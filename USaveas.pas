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
  GR32,
  u_ExportProviderAbstract,
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
    Label3: TLabel;
    Label6: TLabel;
    Label25: TLabel;
    CBscleit: TComboBox;
    Label26: TLabel;
    Bevel2: TBevel;
    Bevel5: TBevel;
    Label8: TLabel;
    Label9: TLabel;
    CBFormat: TComboBox;
    Button3: TButton;
    QualitiEdit: TSpinEdit;
    CBZoomload: TComboBox;
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
    CBCloseWithStart: TCheckBox;
    PrTypesBox: TCheckListBox;
    CBUsedMarks: TCheckBox;
    TabSheet6: TTabSheet;
    pnlExport: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button3Click(Sender: TObject);
    procedure CBZoomloadChange(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure CBFormatChange(Sender: TObject);
  private
    FZoom_rect:byte;
    FPolygonLL: TExtendedPointArray;
    FProviderTilesDelte: TExportProviderAbstract;
    FProviderTilesGenPrev: TExportProviderAbstract;
    FProviderTilesCopy: TExportProviderAbstract;
    FProviderTilesDownload: TExportProviderAbstract;
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
  i_IMapCalibration,
  i_ICoordConverter,
  u_ThreadMapCombineBMP,
  u_ThreadMapCombineECW,
  u_ThreadMapCombineJPG,
  u_ThreadMapCombineKMZ,
  u_ExportProviderYaMaps,
  u_ExportProviderGEKml,
  u_ExportProviderIPhone,
  u_ExportProviderAUX,
  u_ExportProviderZip,
  u_ProviderTilesDelete,
  u_ProviderTilesGenPrev,
  u_ProviderTilesCopy,
  u_ProviderTilesDownload,
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
  FProviderTilesDelte.StartProcess(APolyLL);
end;

destructor TFsaveas.Destroy;
var
  i: Integer;
begin
  for i := 0 to CBFormat.Items.Count - 1 do begin
    CBFormat.Items.Objects[i].Free;
    CBFormat.Items.Objects[i] := nil;
  end;
  FreeAndNil(FProviderTilesDelte);
  FreeAndNil(FProviderTilesGenPrev);
  FreeAndNil(FProviderTilesCopy);
  FreeAndNil(FProviderTilesDownload);
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
begin
  FProviderTilesCopy.StartProcess(APolyLL);
end;

procedure TFsaveas.LoadRegion(APolyLL: TExtendedPointArray);
begin
  FProviderTilesDownload.StartProcess(APolyLL);
end;

procedure TFsaveas.genbacksatREG(APolyLL: TExtendedPointArray);
begin
  FProviderTilesGenPrev.StartProcess(APolyLL);
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
  VExportProvider := TExportProviderZip.Create(pnlExport);
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);
  CBFormat.ItemIndex := 0;

  FProviderTilesDelte := TProviderTilesDelete.Create(TabSheet4);
  FProviderTilesGenPrev := TProviderTilesGenPrev.Create(TabSheet3);
  FProviderTilesCopy := TProviderTilesCopy.Create(TabSheet6);
  FProviderTilesDownload := TProviderTilesDownload.Create(TabSheet1);
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
  CBZoomload.Items.Clear;
  for i:=1 to 24 do begin
    CBZoomload.Items.Add(inttostr(i));
  end;
  CBscleit.Items.Clear;
  CBSclHib.Items.Clear;
  CBSclHib.Items.Add(SAS_STR_No);
  VActiveMap := GState.ViewState.GetCurrentMap;
  For i:=0 to length(GState.MapType)-1 do begin
    VMapType := GState.MapType[i];
    if VMapType.Usestick then begin
      VAddedIndex := CBscleit.Items.AddObject(VMapType.name,VMapType);
      if (VMapType.asLayer) then begin
        CBSclHib.Items.AddObject(VMapType.name,VMapType);
      end;
      if VMapType = VActiveMap then begin
        CBscleit.ItemIndex:=VAddedIndex;
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
      VExportProvider.InitFrame(Azoom, FPolygonLL);
    end;
  end;
  CBFormatChange(CBFormat);
  FProviderTilesDelte.InitFrame(Azoom, FPolygonLL);
  FProviderTilesDelte.Show;
  FProviderTilesGenPrev.InitFrame(Azoom, FPolygonLL);
  FProviderTilesGenPrev.Show;
  FProviderTilesCopy.InitFrame(Azoom, FPolygonLL);
  FProviderTilesCopy.Show;
  FProviderTilesDownload.InitFrame(Azoom, FPolygonLL);
  FProviderTilesDownload.Show;
  Fmain.Enabled:=false;
  fSaveas.Visible:=true;
  CBZoomload.ItemIndex:=FZoom_rect;
  CBZoomloadChange(self);
end;


procedure TFsaveas.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Fmain.TBmoveClick(Fmain);
 Fmain.Enabled:=true;
 fsaveas.visible:=false;
end;

procedure TFsaveas.Button3Click(Sender: TObject);
begin
 close;
end;

procedure TFsaveas.CBZoomloadChange(Sender: TObject);
var polyg:TPointArray;
    min,max:TPoint;
    numd:int64 ;
    Vmt: TMapType;
    VZoom: byte;
begin
  Vmt := TMapType(CBscleit.Items.Objects[CBscleit.ItemIndex]);
  VZoom := CBZoomload.ItemIndex;
  polyg := Vmt.GeoConvert.LonLatArray2PixelArray(FPolygonLL, VZoom);
  numd:=GetDwnlNum(min,max,polyg,true);
  label6.Caption:=SAS_STR_filesnum+': '+inttostr((max.x-min.x)div 256+1)+'x'
                  +inttostr((max.y-min.y)div 256+1)+'('+inttostr(numd)+')';
  GetMinMax(min,max,polyg,false);
  label6.Caption:=label6.Caption+', '+SAS_STR_Resolution+' '+inttostr(max.x-min.x)+'x'
                +inttostr(max.y-min.y);
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
