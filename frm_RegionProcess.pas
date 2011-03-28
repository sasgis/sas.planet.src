unit frm_RegionProcess;

interface

uses
  Windows,
  SysUtils,
  Forms,
  Buttons,
  Classes,
  Controls,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  inifiles,
  ComCtrls,
  u_CommonFormAndFrameParents,
  u_ExportProviderAbstract,
  t_GeoTypes,
  u_GeoTostr;

type
  TfrmRegionProcess = class(TCommonFormParent)
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
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    FZoom_rect:byte;
    FPolygonLL: TArrayOfDoublePoint;
    FProviderTilesDelte: TExportProviderAbstract;
    FProviderTilesGenPrev: TExportProviderAbstract;
    FProviderTilesCopy: TExportProviderAbstract;
    FProviderTilesDownload: TExportProviderAbstract;
    FProviderMapCombine: TExportProviderAbstract;
    procedure LoadRegion(APolyLL: TArrayOfDoublePoint);
    procedure DelRegion(APolyLL: TArrayOfDoublePoint);
    procedure genbacksatREG(APolyLL: TArrayOfDoublePoint);
    procedure scleitRECT(APolyLL: TArrayOfDoublePoint);
    procedure savefilesREG(APolyLL: TArrayOfDoublePoint);
    procedure ExportREG(APolyLL: TArrayOfDoublePoint);
    procedure InitExportsList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadSelFromFile(FileName:string);
    procedure Show_(Azoom:byte;Polygon_: TArrayOfDoublePoint);
    procedure RefreshTranslation; override;
  end;

var
  frmRegionProcess: TfrmRegionProcess;

implementation

uses
  gnugettext,
  u_GlobalState,
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
  u_GeoFun,
  frm_Main;

{$R *.dfm}

constructor TfrmRegionProcess.Create(AOwner: TComponent);
begin
  TP_Ignore(Self, 'CBFormat.Items');
  inherited;

  InitExportsList;

  FProviderTilesDelte := TProviderTilesDelete.Create(TabSheet4);
  FProviderTilesGenPrev := TProviderTilesGenPrev.Create(TabSheet3);
  FProviderTilesCopy := TProviderTilesCopy.Create(TabSheet6);
  FProviderTilesDownload := TProviderTilesDownload.Create(TabSheet1);
  FProviderMapCombine := TProviderMapCombine.Create(TabSheet2);
end;

destructor TfrmRegionProcess.Destroy;
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

procedure TfrmRegionProcess.LoadSelFromFile(FileName:string);
var
  ini:TMemIniFile;
  i:integer;
  VPolygon: TArrayOfDoublePoint;
  VZoom: Byte;
begin
 if FileExists(FileName) then
  begin
   ini:=TMemIniFile.Create(FileName);
   i:=1;
   while str2r(Ini.ReadString('HIGHLIGHTING','PointLon_'+inttostr(i),'2147483647'))<>2147483647 do
    begin
     setlength(VPolygon,i);
     VPolygon[i-1].x:=str2r(Ini.ReadString('HIGHLIGHTING','PointLon_'+inttostr(i),'2147483647'));
     VPolygon[i-1].y:=str2r(Ini.ReadString('HIGHLIGHTING','PointLat_'+inttostr(i),'2147483647'));
     inc(i);
    end;
   if length(VPolygon)>0 then
    begin
     VZoom := Ini.Readinteger('HIGHLIGHTING','zoom',1) - 1;
     Self.Show_(VZoom, VPolygon);
    end;
  end
end;

procedure TfrmRegionProcess.RefreshTranslation;
var
  i: Integer;
  VProvider: TExportProviderAbstract;
  VIndex: Integer;
begin
  inherited;
  VIndex := CBFormat.ItemIndex;
  for i := 0 to CBFormat.Items.Count - 1 do begin
    VProvider := TExportProviderAbstract(CBFormat.Items.Objects[i]);
    VProvider.RefreshTranslation;
    CBFormat.Items[i] := VProvider.GetCaption;
  end;
  CBFormat.ItemIndex := VIndex;
  FProviderTilesDelte.RefreshTranslation;
  FProviderTilesGenPrev.RefreshTranslation;
  FProviderTilesCopy.RefreshTranslation;
  FProviderTilesDownload.RefreshTranslation;
  FProviderMapCombine.RefreshTranslation;
end;

procedure TfrmRegionProcess.DelRegion(APolyLL: TArrayOfDoublePoint);
begin
  FProviderTilesDelte.StartProcess(APolyLL);
end;

procedure TfrmRegionProcess.ExportREG(APolyLL: TArrayOfDoublePoint);
var
  VExportProvider: TExportProviderAbstract;
begin
  VExportProvider := TExportProviderAbstract(CBFormat.Items.Objects[CBFormat.ItemIndex]);
  if VExportProvider <> nil then begin
    VExportProvider.StartProcess(APolyLL);
  end;
end;


procedure TfrmRegionProcess.savefilesREG(APolyLL: TArrayOfDoublePoint);
begin
  FProviderTilesCopy.StartProcess(APolyLL);
end;

procedure TfrmRegionProcess.LoadRegion(APolyLL: TArrayOfDoublePoint);
begin
  FProviderTilesDownload.StartProcess(APolyLL);
end;

procedure TfrmRegionProcess.genbacksatREG(APolyLL: TArrayOfDoublePoint);
begin
  FProviderTilesGenPrev.StartProcess(APolyLL);
end;

procedure TfrmRegionProcess.InitExportsList;
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

procedure TfrmRegionProcess.scleitRECT(APolyLL: TArrayOfDoublePoint);
begin
  FProviderMapCombine.StartProcess(APolyLL);
end;


procedure TfrmRegionProcess.Button1Click(Sender: TObject);
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
   frmMain.Enabled:=true;
   close;
  end;
end;

procedure TfrmRegionProcess.Show_(Azoom:byte;Polygon_: TArrayOfDoublePoint);
var
  i:integer;
  VExportProvider: TExportProviderAbstract;
  VPointsCount: Integer;
begin
  FZoom_rect:=Azoom;
  FPolygonLL := copy(polygon_);
  VPointsCount := Length(FPolygonLL);
  if VPointsCount > 1 then begin
    if not DoublePoitnsEqual(FPolygonLL[0], FPolygonLL[VPointsCount - 1]) then begin
      SetLength(FPolygonLL, VPointsCount + 1);
      FPolygonLL[VPointsCount] := FPolygonLL[0];
    end;
  end;

  GState.LastSelectionInfo.SetPolygon(FPolygonLL, FZoom_rect);
  for i := 0 to CBFormat.Items.Count - 1 do begin
    VExportProvider := TExportProviderAbstract(CBFormat.Items.Objects[i]);
    if VExportProvider <> nil then begin
      VExportProvider.InitFrame(Azoom, FPolygonLL);
    end;
  end;
  Self.Show;
end;


procedure TfrmRegionProcess.FormActivate(Sender: TObject);
begin
  PageControl1.ActivePageIndex:=0;
end;

procedure TfrmRegionProcess.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  frmMain.TBmoveClick(nil);
  frmMain.Enabled:=true;
end;

procedure TfrmRegionProcess.FormShow(Sender: TObject);
begin
  CBFormatChange(CBFormat);
  FProviderTilesDelte.InitFrame(FZoom_rect, FPolygonLL);
  FProviderTilesDelte.Show;
  FProviderTilesGenPrev.InitFrame(FZoom_rect, FPolygonLL);
  FProviderTilesGenPrev.Show;
  FProviderTilesCopy.InitFrame(FZoom_rect, FPolygonLL);
  FProviderTilesCopy.Show;
  FProviderTilesDownload.InitFrame(FZoom_rect, FPolygonLL);
  FProviderTilesDownload.Show;
  FProviderMapCombine.InitFrame(FZoom_rect, FPolygonLL);
  FProviderMapCombine.Show;
  frmMain.Enabled:=false;
end;

procedure TfrmRegionProcess.Button3Click(Sender: TObject);
begin
  close;
end;

procedure TfrmRegionProcess.SpeedButton1Click(Sender: TObject);
var
  Ini: Tinifile;
  i:integer;
  VZoom: Byte;
  VPolygon: TArrayOfDoublePoint;
begin
 if (SaveSelDialog.Execute)and(SaveSelDialog.FileName<>'') then
  begin
   If FileExists(SaveSelDialog.FileName) then DeleteFile(SaveSelDialog.FileName);
   VZoom := GState.LastSelectionInfo.Zoom;
   VPolygon := copy(GState.LastSelectionInfo.Polygon);
   Ini:=TiniFile.Create(SaveSelDialog.FileName);
   try
   if length(VPolygon)>0 then
    begin
     Ini.WriteInteger('HIGHLIGHTING','zoom',VZoom + 1);
     for i:=1 to length(VPolygon) do
      begin
       Ini.WriteFloat('HIGHLIGHTING','PointLon_'+inttostr(i),VPolygon[i-1].x);
       Ini.WriteFloat('HIGHLIGHTING','PointLat_'+inttostr(i),VPolygon[i-1].y);
      end;
    end;
   finally
    ini.Free;
   end;
  end;
end;

procedure TfrmRegionProcess.CBFormatChange(Sender: TObject);
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
