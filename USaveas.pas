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
  u_CommonFormAndFrameParents,
  u_ExportProviderAbstract,
  UGeoFun,
  UMapType,
  UResStrings,
  t_GeoTypes,
  u_GeoTostr;

type
  TFsaveas = class(TCommonFormParent)
    Button1: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    Bevel5: TBevel;
    Label9: TLabel;
    CBFormat: TComboBox;
    Button3: TButton;
    SpeedButton1: TSpeedButton;
    SaveSelDialog: TSaveDialog;
    CBCloseWithStart: TCheckBox;
    TabSheet6: TTabSheet;
    pnlExport: TPanel;
    pnlBottomButtons: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button3Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure CBFormatChange(Sender: TObject);
  private
    FZoom_rect:byte;
    FPolygonLL: TExtendedPointArray;
    FProviderTilesDelte: TExportProviderAbstract;
    FProviderTilesGenPrev: TExportProviderAbstract;
    FProviderTilesCopy: TExportProviderAbstract;
    FProviderTilesDownload: TExportProviderAbstract;
    FProviderMapCombine: TExportProviderAbstract;
    procedure LoadRegion(APolyLL: TExtendedPointArray);
    procedure DelRegion(APolyLL: TExtendedPointArray);
    procedure genbacksatREG(APolyLL: TExtendedPointArray);
    procedure scleitRECT(APolyLL: TExtendedPointArray);
    procedure savefilesREG(APolyLL: TExtendedPointArray);
    procedure ExportREG(APolyLL: TExtendedPointArray);
    procedure InitExportsList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadSelFromFile(FileName:string);
    procedure Show_(Azoom:byte;Polygon_: TExtendedPointArray);
    procedure RefreshTranslation; override;
   end;

var
  Fsaveas: TFsaveas;

implementation

uses
  gnugettext,
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
  u_ProviderMapCombine,
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

procedure TFsaveas.RefreshTranslation;
var
  i: Integer;
begin
  inherited;
  for i := 0 to CBFormat.Items.Count - 1 do begin
    TExportProviderAbstract(CBFormat.Items.Objects[i]).RefreshTranslation;
  end;
  FProviderTilesDelte.RefreshTranslation;
  FProviderTilesGenPrev.RefreshTranslation;
  FProviderTilesCopy.RefreshTranslation;
  FProviderTilesDownload.RefreshTranslation;
  FProviderMapCombine.RefreshTranslation;
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
  FreeAndNil(FProviderMapCombine);
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

procedure TFsaveas.InitExportsList;
var
  VExportProvider: TExportProviderAbstract;
begin
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
end;

procedure TFsaveas.scleitRECT(APolyLL: TExtendedPointArray);
begin
  FProviderMapCombine.StartProcess(APolyLL);
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
  TP_Ignore(Self, 'CBFormat.Items');
  inherited;

  InitExportsList;

  FProviderTilesDelte := TProviderTilesDelete.Create(TabSheet4);
  FProviderTilesGenPrev := TProviderTilesGenPrev.Create(TabSheet3);
  FProviderTilesCopy := TProviderTilesCopy.Create(TabSheet6);
  FProviderTilesDownload := TProviderTilesDownload.Create(TabSheet1);
  FProviderMapCombine := TProviderMapCombine.Create(TabSheet2);
  PageControl1.ActivePageIndex:=0;
end;

procedure TFsaveas.Show_(Azoom:byte;Polygon_: TExtendedPointArray);
var
  i:integer;
  vramkah,zagran:boolean;
  VConverter: ICoordConverter;
  VPoint: TPoint;
  VZoom: Byte;
  VExportProvider: TExportProviderAbstract;
begin
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
  FProviderMapCombine.InitFrame(Azoom, FPolygonLL);
  FProviderMapCombine.Show;
  Fmain.Enabled:=false;
  fSaveas.Visible:=true;
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
